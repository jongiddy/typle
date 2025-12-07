use super::*;

impl<'a> TypleContext<'a> {
    pub(super) fn replace_pat(&self, pat: &mut Pat) -> Result<()> {
        match pat {
            Pat::Macro(m) => {
                if let Some(p) = self.replace_macro_pat(m)? {
                    *pat = p;
                }
            }
            Pat::Or(or) => {
                for pat in &mut or.cases {
                    self.replace_pat(pat)?;
                }
            }
            Pat::Paren(paren) => {
                self.replace_attrs(&mut paren.attrs)?;
                match self.replace_pat_in_list(std::mem::replace(
                    &mut paren.pat,
                    Pat::Verbatim(TokenStream::new()),
                )) {
                    Replacement::Singleton(result) => {
                        paren.pat = Box::new(result?);
                    }
                    iter => {
                        *pat = Pat::Tuple(syn::PatTuple {
                            attrs: std::mem::take(&mut paren.attrs),
                            paren_token: paren.paren_token,
                            elems: iter.collect::<Result<_>>()?,
                        });
                    }
                }
            }
            Pat::Path(path) => {
                // State::S::<typle_ident!(i)> -> State::S2
                self.replace_qself_path(&mut path.qself, &mut path.path)?;
                self.replace_path_arguments(&mut path.path)?;
            }
            Pat::Reference(reference) => {
                self.replace_pat(&mut reference.pat)?;
            }
            Pat::Slice(slice) => {
                for pat in &mut slice.elems {
                    self.replace_pat(pat)?;
                }
            }
            Pat::Struct(pat) => {
                self.replace_qself_path(&mut pat.qself, &mut pat.path)?;
                if pat.path.segments.is_empty() {
                    if let Some(qself) = &mut pat.qself {
                        if let Type::Path(syn::TypePath { qself, path }) = &mut *qself.ty {
                            pat.path = path.clone();
                            pat.qself = qself.take();
                        }
                    }
                }
                self.replace_path_arguments(&mut pat.path)?;
            }
            Pat::Tuple(tuple) => {
                tuple.elems = std::mem::take(&mut tuple.elems)
                    .into_iter()
                    .flat_map(move |pat| self.replace_pat_in_list(pat))
                    .collect::<Result<_>>()?;
            }
            Pat::TupleStruct(tuple_struct) => {
                // State::S::<typle_ident!(i)> -> State::S2
                if let Some(qself) = &mut tuple_struct.qself {
                    self.replace_type(&mut qself.ty)?;
                }
                for mut path_segment in std::mem::take(&mut tuple_struct.path.segments) {
                    match &mut path_segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(args) => {
                            if let Some(index) = self.typle_ident(args)? {
                                // X::<typle_ident!(3)> -> X3
                                path_segment.ident =
                                    format_ident!("{}{}", path_segment.ident, index);
                                path_segment.arguments = PathArguments::None;
                            } else {
                                // std::option::Option<T> -> std::option::Option<(T0, T1,...)>
                                // std::option::Option<T<3>> -> std::option::Option<T3>
                                self.replace_generic_arguments(&mut args.args)?;
                                if args.args.is_empty() {
                                    path_segment.arguments = PathArguments::None;
                                }
                            }
                        }
                        PathArguments::Parenthesized(p) => {
                            abort!(p, "parenthesized arguments not supported")
                        }
                    }
                    tuple_struct.path.segments.push(path_segment);
                }
            }
            Pat::Type(pat_type) => {
                self.replace_pat(&mut pat_type.pat)?;
                self.replace_type(&mut pat_type.ty)?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn replace_pat_in_list(
        &'a self,
        mut pat: Pat,
    ) -> Replacement<
        Result<Pat>,
        impl Iterator<Item = Result<Pat>> + 'a,
        std::iter::Empty<Result<Pat>>,
    > {
        match &mut pat {
            Pat::Macro(syn::PatMacro { mac, .. }) => {
                if let Some(ident) = mac.path.get_ident() {
                    if ident == "typle" || ident == "typle_args" {
                        let token_stream = std::mem::take(&mut mac.tokens);
                        let default_span = token_stream.span();
                        let mut tokens = token_stream.into_iter();
                        let (pattern, range) =
                            match self.parse_pattern_range(&mut tokens, default_span) {
                                Ok(t) => t,
                                Err(e) => {
                                    return Replacement::One(Err(e));
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
                        return Replacement::Iterator1(range.zip_clone(token_stream).flat_map({
                            move |(index, token_stream)| {
                                if let Some(ident) = &pattern {
                                    *context.constants.get_mut(ident).unwrap() = index;
                                }
                                let token_stream = match context.evaluate_if(token_stream) {
                                    Ok(Some(token_stream)) => token_stream,
                                    Ok(None) => {
                                        return None;
                                    }
                                    Err(e) => {
                                        return Some(Err(e));
                                    }
                                };
                                let mut pat = match Pat::parse_single.parse2(token_stream) {
                                    Ok(pat) => pat,
                                    Err(e) => return Some(Err(e)),
                                };
                                match context.replace_pat(&mut pat) {
                                    Ok(()) => Some(Ok(pat)),
                                    Err(e) => Some(Err(e)),
                                }
                            }
                        }));
                    }
                }
            }
            _ => {}
        }
        match self.replace_pat(&mut pat) {
            Ok(()) => Replacement::Singleton(Ok(pat)),
            Err(e) => Replacement::Singleton(Err(e)),
        }
    }

    fn replace_macro_pat(&self, m: &mut syn::PatMacro) -> Result<Option<Pat>> {
        self.replace_attrs(&mut m.attrs)?;
        if let Some(macro_name) = m.mac.path.get_ident() {
            if macro_name == "typle_for" {
                let mut tuple = syn::PatTuple {
                    attrs: std::mem::take(&mut m.attrs),
                    paren_token: token::Paren::default(),
                    elems: Punctuated::new(),
                };
                let token_stream = std::mem::take(&mut m.mac.tokens);
                let default_span = token_stream.span();
                let mut tokens = token_stream.into_iter();
                let (pattern, range) = self.parse_pattern_range(&mut tokens, default_span)?;
                if range.is_empty() {
                    return Ok(Some(Pat::Tuple(tuple)));
                }
                let token_stream = tokens.collect::<TokenStream>();
                let body_span = token_stream.span();
                match m.mac.delimiter {
                    MacroDelimiter::Paren(_) => {
                        let pat = Pat::parse_single.parse2(token_stream)?;
                        let mut context = self.clone();
                        if let Some(ident) = &pattern {
                            context.constants.insert(ident.clone(), 0);
                        }
                        for (index, mut component) in range.zip_clone(pat) {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            context.replace_pat(&mut component)?;
                            tuple.elems.push(component);
                        }
                    }
                    MacroDelimiter::Brace(_) => {
                        let Ok(expr) = syn::parse2::<Expr>(token_stream) else {
                            return Err(Error::new(
                                body_span,
                                "cannot parse, wrap types in `typle_pat!()`",
                            ));
                        };
                        let mut context = self.clone();
                        context.const_if = true;
                        if let Some(ident) = &pattern {
                            context.constants.insert(ident.clone(), 0);
                        }
                        for (index, mut expr) in range.zip_clone(expr) {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            let mut state = BlockState::default();
                            context.replace_expr(&mut expr, &mut state)?;
                            if let Expr::Verbatim(_) = expr {
                                // Verbatim omits the pattern from the tuple.
                            } else {
                                let mut pat = expr_to_pat(expr)?;
                                context.replace_pat(&mut pat)?;
                                tuple.elems.push(pat);
                            }
                        }
                    }
                    MacroDelimiter::Bracket(_) => {
                        abort!(m, "expected parentheses or braces");
                    }
                }
                return Ok(Some(Pat::Tuple(tuple)));
            }
        }
        m.mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut m.mac.tokens))?;
        Ok(None)
    }
}
