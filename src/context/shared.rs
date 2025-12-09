use super::*;

macro_rules! abort {
    ($spanned:expr, $message:expr) => {
        return Err(syn::Error::new($spanned.span(), $message))
    };
}

pub(super) use abort;

pub(super) enum Replacement<T> {
    /// A single value, not from a typle expansion
    Singleton(T),
    /// An empty typle expansion
    Empty,
    /// A typle expansion
    Iterator(Box<dyn Iterator<Item = syn::Result<T>>>),
    /// An error outside of the iterator
    Error(syn::Error),
}

impl<T> Iterator for Replacement<T> {
    type Item = syn::Result<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, Replacement::Empty) {
            Replacement::Singleton(t) => Some(Ok(t)),
            Replacement::Empty => None,
            Replacement::Iterator(mut iterator) => {
                let item = iterator.next();
                *self = Replacement::Iterator(iterator);
                item
            }
            Replacement::Error(t) => Some(Err(t)),
        }
    }
}

impl TypleContext {
    pub(super) fn parse_pattern_range(
        &self,
        tokens: &mut impl Iterator<Item = TokenTree>,
        span: Span,
    ) -> syn::Result<(Option<Ident>, Range<usize>)> {
        let mut collect = TokenStream::new();
        let mut pattern = None;
        let mut equals = None;
        for token in tokens.by_ref() {
            match token {
                TokenTree::Ident(ident) if pattern.is_none() && ident == "in" => {
                    if let Some(punct) = equals.take() {
                        collect.extend([TokenTree::Punct(punct)]);
                    }
                    let mut tokens = std::mem::take(&mut collect).into_iter();
                    match tokens.next() {
                        Some(TokenTree::Ident(ident)) => {
                            if ident != "_" {
                                pattern = Some(ident);
                            }
                            if let Some(tt) = tokens.next() {
                                abort!(tt, "unexpected token");
                            }
                        }
                        Some(tt) => {
                            abort!(tt, "expected identifier before keyword `in`");
                        }
                        None => {
                            abort!(ident, "expected identifier before keyword `in`");
                        }
                    }
                }
                TokenTree::Punct(punct) if punct.as_char() == '=' => {
                    equals = Some(punct);
                }
                TokenTree::Punct(punct) if equals.is_some() && punct.as_char() == '>' => {
                    equals = None;
                    break;
                }
                tt => {
                    if let Some(punct) = equals.take() {
                        collect.extend([TokenTree::Punct(punct)]);
                    }
                    collect.extend([tt]);
                }
            }
        }
        if let Some(punct) = equals.take() {
            collect.extend([TokenTree::Punct(punct)]);
        }
        if collect.is_empty() {
            return Err(syn::Error::new(span, "expected range"));
        }
        let mut expr = syn::parse2::<Expr>(collect)?;
        let mut state = BlockState::default();
        self.replace_expr(&mut expr, &mut state)?;
        let Some((start, end)) = evaluate_range(&expr) else {
            return Err(syn::Error::new(expr.span(), "expected range"));
        };
        let start = match start {
            Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                if let Some(suspicious_ident) = &state.suspicious_ident {
                    abort!(
                        suspicious_ident,
                        format!(
                            "range start invalid, possibly missing `{}: {}` bound",
                            suspicious_ident, self.typle_macro.ident
                        )
                    );
                } else {
                    abort!(span, "range start invalid");
                }
            }
            Bound::Included(Ok(start)) => start,
            Bound::Excluded(Ok(start)) => start.saturating_add(1),
            Bound::Unbounded => 0,
        };
        let end = match end {
            Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                if let Some(suspicious_ident) = &state.suspicious_ident {
                    abort!(
                        suspicious_ident,
                        format!(
                            "range end invalid, possibly missing `{}: {}` bound",
                            suspicious_ident, self.typle_macro.ident
                        )
                    );
                } else {
                    abort!(span, "range end invalid");
                }
            }
            Bound::Included(Ok(end)) => end.saturating_add(1),
            Bound::Excluded(Ok(end)) => end,
            Bound::Unbounded => match self.typle_len {
                Some(end) => end,
                None => {
                    abort!(expr, "need an explicit end in range");
                }
            },
        };
        Ok((pattern, start..end))
    }

    // This can return <QSelf>:: or <QSelf> which needs to be cleaned up by the caller.
    pub(super) fn replace_qself_path(&self, qself: &mut Option<QSelf>, path: &mut Path) -> syn::Result<()> {
        if let Some(qself) = qself {
            self.replace_type(&mut qself.ty)?;
        } else if let Some(ident) = path.segments.first().map(|segment| &segment.ident) {
            if let Some(typle) = self.typles.get(ident) {
                let mut segments = std::mem::take(&mut path.segments).into_iter();
                let mut first = segments.next().unwrap();
                match &mut first.arguments {
                    PathArguments::None => {
                        // T::clone(&t) -> <(T0, T1)>::clone(&t)
                        // T -> <(T0, T1)> (needs to be undone at call site)
                        let tuple_type = Box::new(Type::Tuple(syn::TypeTuple {
                            paren_token: token::Paren::default(),
                            elems: (0..self.typle_len.unwrap_or(self.typle_macro.max_len))
                                .map(|i| self.get_type(typle, i, first.span()))
                                .collect::<syn::Result<_>>()?,
                        }));
                        *qself = Some(QSelf {
                            lt_token: token::Lt::default(),
                            ty: tuple_type,
                            position: 0,
                            as_token: None,
                            gt_token: token::Gt::default(),
                        });
                        path.leading_colon = Some(token::PathSep::default());
                        path.segments = segments.collect();
                    }
                    PathArguments::AngleBracketed(args) => {
                        // T::<0>::default() -> <T0>::default()
                        // T::<0> -> <T0>
                        if args.args.len() != 1 {
                            abort!(first, "expected one type parameter");
                        }
                        match args.args.first_mut() {
                            Some(GenericArgument::Const(expr)) => {
                                // T<{T::LEN - 1}>
                                let mut state = BlockState::default();
                                self.replace_expr(expr, &mut state)?;
                                // T<{5 - 1}>
                                let Some(value) = evaluate_usize(expr) else {
                                    abort!(expr, "unsupported tuple type index");
                                };
                                // T<{4}>::State -> <T4>::State
                                // T<{4}> -> <T4>::
                                *qself = Some(QSelf {
                                    lt_token: token::Lt::default(),
                                    ty: Box::new(self.get_type(typle, value, first.span())?),
                                    position: 0,
                                    as_token: None,
                                    gt_token: token::Gt::default(),
                                });
                                path.leading_colon = Some(token::PathSep::default());
                                path.segments = segments.collect();
                            }
                            _ => {
                                abort!(args, "Require const parameter (wrap {} around expression)");
                            }
                        }
                    }
                    PathArguments::Parenthesized(_) => {
                        // T(u32) -> u32
                        abort!(
                            first,
                            "typled types do not support parenthesized parameters"
                        )
                    }
                }
            } else if let Some(rename) = self.renames.get(ident) {
                let ident = &mut path.segments.first_mut().unwrap().ident;
                *ident = Ident::new(rename, ident.span());
            } else if let Some(ty) = self.retypes.get(ident) {
                match ty {
                    Type::Path(syn::TypePath {
                        qself: ty_qself,
                        path: ty_path,
                    }) if ty_path.segments.len() == 1 => {
                        let ty_segment = ty_path.segments.first().unwrap();
                        let segment = path.segments.first_mut().unwrap();
                        *segment = ty_segment.clone();
                        *qself = ty_qself.clone();
                        path.leading_colon = ty_path.leading_colon;
                    }
                    _ => {
                        let mut segments = std::mem::take(&mut path.segments).into_iter();
                        let _ = segments.next().unwrap();
                        *qself = Some(QSelf {
                            lt_token: token::Lt::default(),
                            ty: Box::new(ty.clone()),
                            position: 0,
                            as_token: None,
                            gt_token: token::Gt::default(),
                        });
                        path.leading_colon = Some(token::PathSep::default());
                        path.segments = segments.collect();
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn evaluate_if(&self, ts: TokenStream) -> syn::Result<Option<TokenStream>> {
        let mut tokens = ts.into_iter();
        match tokens.next() {
            Some(TokenTree::Ident(ident)) if ident == "if" => {
                let mut tokens = tokens.collect::<Vec<_>>();
                match tokens.pop() {
                    Some(TokenTree::Group(group1)) => match tokens.last() {
                        Some(TokenTree::Ident(ident)) if ident == "else" => {
                            let else_span = ident.span();
                            tokens.pop().unwrap();
                            match tokens.pop() {
                                Some(TokenTree::Group(group0)) => {
                                    let mut cond =
                                        syn::parse2::<Expr>(tokens.into_iter().collect())?;
                                    let mut state = BlockState::default();
                                    self.replace_expr(&mut cond, &mut state)?;
                                    let b = evaluate_bool(&cond)?;
                                    self.evaluate_if(if b {
                                        group0.stream()
                                    } else {
                                        group1.stream()
                                    })
                                }
                                Some(tt) => {
                                    abort!(tt, "Expect body before `else`");
                                }
                                None => {
                                    abort!(else_span, "Expect body before `else`");
                                }
                            }
                        }
                        Some(_) => {
                            let mut cond = syn::parse2::<Expr>(tokens.into_iter().collect())?;
                            let mut state = BlockState::default();
                            self.replace_expr(&mut cond, &mut state)?;
                            let b = evaluate_bool(&cond)?;
                            if b {
                                self.evaluate_if(group1.stream())
                            } else {
                                Ok(None)
                            }
                        }
                        None => abort!(ident, "Expect expression after `if`"),
                    },
                    Some(tt) => {
                        abort!(tt, "Expect body at end of `if`");
                    }
                    None => {
                        abort!(ident, "Expect expression after `if`");
                    }
                }
            }
            Some(tt) => Ok(Some(std::iter::once(tt).chain(tokens).collect())),
            None => Ok(None),
        }
    }

    /// Replace typle constants with literal value.
    ///
    /// If the path matches a typle index variable (`i`) or typle associated
    /// constant (`T::LEN`) return the literal replacement.
    pub(super) fn replace_typle_associated_const(
        &self,
        path: &syn::ExprPath,
        state: &mut BlockState,
    ) -> syn::Result<Option<Lit>> {
        if path.qself.is_some() {
            // no support for `<T as Tuple>::LEN`
            return Ok(None);
        }
        let mut segments = path.path.segments.iter().fuse();
        if let Some(syn::PathSegment {
            ident: ident1,
            arguments: PathArguments::None,
        }) = segments.next()
        {
            match segments.next() {
                None => {
                    // Path T
                    if let Some(value) = self.constants.get(ident1) {
                        return Ok(Some(Lit::Int(syn::LitInt::new(
                            &value.to_string(),
                            ident1.span(),
                        ))));
                    }
                }
                Some(syn::PathSegment {
                    ident: ident2,
                    arguments: PathArguments::None,
                }) => {
                    if segments.next().is_some() {
                        // Path T::U::V
                        return Ok(None);
                    }
                    // Path T::U
                    if ident2 == "LEN" {
                        if ident1 == &self.typle_macro.ident || self.typles.contains_key(ident1) {
                            // Tuple::LEN or T::LEN
                            let Some(typle_len) = self.typle_len else {
                                abort!(ident2, "LEN not available outside fn or impl");
                            };
                            return Ok(Some(Lit::Int(syn::LitInt::new(
                                &typle_len.to_string(),
                                path.span(),
                            ))));
                        } else {
                            // Path looks like a typle associated constant: the caller
                            // may have omitted the typle constraint.
                            state.suspicious_ident = Some(ident1.clone());
                        }
                    } else if ident2 == "LAST" {
                        if ident1 == &self.typle_macro.ident || self.typles.contains_key(ident1) {
                            // Tuple::LAST or T::LAST
                            let Some(typle_len) = self.typle_len else {
                                abort!(ident2, "LAST not available outside fn or impl");
                            };
                            if typle_len == 0 {
                                abort!(ident2, "LAST not available when LEN == 0");
                            }
                            return Ok(Some(Lit::Int(syn::LitInt::new(
                                &(typle_len - 1).to_string(),
                                path.span(),
                            ))));
                        } else {
                            state.suspicious_ident = Some(ident1.clone());
                        }
                    } else if ident2 == "MAX" {
                        if ident1 == &self.typle_macro.ident || self.typles.contains_key(ident1) {
                            // Tuple::MAX or <T as Tuple>::MAX
                            return Ok(Some(Lit::Int(syn::LitInt::new(
                                &self.typle_macro.max_len.to_string(),
                                path.span(),
                            ))));
                        } else {
                            state.suspicious_ident = Some(ident1.clone());
                        }
                    } else if ident2 == "MIN" {
                        if ident1 == &self.typle_macro.ident || self.typles.contains_key(ident1) {
                            // Tuple::MIN or <T as Tuple>::MIN
                            return Ok(Some(Lit::Int(syn::LitInt::new(
                                &self.typle_macro.min_len.to_string(),
                                path.span(),
                            ))));
                        } else {
                            state.suspicious_ident = Some(ident1.clone());
                        }
                    }
                }
                _ => {
                    // Path T::U<V> or T::U(V)
                }
            }
        }
        Ok(None)
    }

    pub(super) fn expand_typle_macro<T, F>(&self, token_stream: TokenStream, f: F) -> Replacement<T>
    where
        T: 'static,
        F: Fn(&TypleContext, TokenStream) -> syn::Result<T> + 'static,
    {
        let default_span = token_stream.span();
        let mut tokens = token_stream.into_iter();
        let (pattern, range) = match self.parse_pattern_range(&mut tokens, default_span) {
            Ok(t) => t,
            Err(e) => {
                return Replacement::Error(e);
            }
        };
        if range.is_empty() {
            return Replacement::Empty;
        }
        let token_stream = tokens.collect::<TokenStream>();
        let mut context = self.clone();
        if let Some(ident) = pattern.clone() {
            context.constants.insert(ident, 0);
        }
        return Replacement::Iterator(Box::new(range.zip_clone(token_stream).flat_map(
            move |(index, token_stream)| {
                if let Some(ident) = &pattern {
                    *context.constants.get_mut(ident).unwrap() = index;
                }
                match context.evaluate_if(token_stream) {
                    Ok(Some(token_stream)) => Some(f(&context, token_stream)),
                    Ok(None) => None,
                    Err(e) => Some(Err(e)),
                }
            },
        )));
    }
}
