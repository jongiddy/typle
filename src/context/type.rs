use crate::context::shared::Replacement;

use super::*;

impl TypleContext {
    // Replace `T`` with `(T0, T1,...)`` and `T<1>`` with `T1``
    pub(super) fn replace_type(&self, ty: &mut Type) -> syn::Result<()> {
        match ty {
            Type::Array(array) => {
                self.replace_type(&mut array.elem)?;
                let mut state = BlockState::default();
                self.replace_expr(&mut array.len, &mut state)?;
            }
            Type::BareFn(bare_fn) => {
                bare_fn.inputs = std::mem::take(&mut bare_fn.inputs)
                    .into_iter()
                    .flat_map(|arg| {
                        self.replace_type_in_list(arg.ty).map(move |res| {
                            res.map(|ty| syn::BareFnArg {
                                attrs: arg.attrs.clone(),
                                name: arg.name.clone(),
                                ty,
                            })
                        })
                    })
                    .collect::<syn::Result<_>>()?;
                if let ReturnType::Type(_, ty) = &mut bare_fn.output {
                    self.replace_type(ty)?;
                }
            }
            Type::Group(group) => {
                self.replace_type(&mut group.elem)?;
            }
            Type::Macro(m) => {
                if let Some(typ) = self.replace_macro_type(m)? {
                    *ty = typ;
                }
            }
            Type::Paren(paren) => {
                match self.replace_type_in_list(std::mem::replace(
                    &mut paren.elem,
                    Type::Verbatim(TokenStream::new()),
                )) {
                    Replacement::Singleton(inner) => {
                        paren.elem = Box::new(inner);
                    }
                    iter => {
                        *ty = Type::Tuple(syn::TypeTuple {
                            paren_token: paren.paren_token,
                            elems: iter.collect::<syn::Result<_>>()?,
                        });
                    }
                }
            }
            Type::Path(path) => {
                self.replace_qself_path(&mut path.qself, &mut path.path)?;
                if path.path.segments.is_empty() {
                    if let Some(qself) = path.qself.take() {
                        *ty = *qself.ty;
                        return Ok(());
                    }
                }
                self.replace_path_arguments(&mut path.path)?;
            }
            Type::Ptr(ptr) => {
                self.replace_type(&mut ptr.elem)?;
            }
            Type::Reference(reference) => {
                self.replace_type(&mut reference.elem)?;
            }
            Type::Slice(slice) => {
                self.replace_type(&mut slice.elem)?;
            }
            Type::Tuple(tuple) => {
                tuple.elems = std::mem::take(&mut tuple.elems)
                    .into_iter()
                    .flat_map(|ty| self.replace_type_in_list(ty))
                    .collect::<syn::Result<_>>()?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn replace_type_in_list(&self, mut ty: Type) -> Replacement<Type> {
        match &mut ty {
            Type::Macro(syn::TypeMacro { mac }) => {
                if let Some(ident) = mac.path.get_ident() {
                    if ident == "typle" || ident == "typle_args" {
                        let token_stream = std::mem::take(&mut mac.tokens);
                        return self.expand_typle_macro(token_stream, |context, token_stream| {
                            let mut ty = syn::parse2::<Type>(token_stream)?;
                            context.replace_type(&mut ty)?;
                            Ok(ty)
                        });
                    }
                }
            }
            Type::Path(syn::TypePath { qself, path })
                if qself.is_none() && path.leading_colon.is_none() =>
            {
                let mut segments = path.segments.iter_mut();
                if let Some(first) = segments.next() {
                    if let (Some(typle), PathArguments::AngleBracketed(arguments), None) = (
                        self.typles.get(&first.ident),
                        &mut first.arguments,
                        segments.next(),
                    ) {
                        let mut iter = arguments.args.iter_mut().fuse();
                        if let (Some(GenericArgument::Const(ref mut expr)), None) =
                            (iter.next(), iter.next())
                        {
                            let mut state = BlockState::default();
                            match self.replace_expr(expr, &mut state) {
                                Ok(_) => {}
                                Err(e) => return Replacement::Error(e),
                            }
                            if let Some((start, end)) = evaluate_range(expr) {
                                // T<{..}>
                                let start = match start {
                                    Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                        return Replacement::Error(syn::Error::new(
                                            span,
                                            "expected integer for start of range",
                                        ));
                                    }
                                    Bound::Included(Ok(start)) => start,
                                    Bound::Excluded(Ok(start)) => start.saturating_add(1),
                                    Bound::Unbounded => 0,
                                };
                                let end = match end {
                                    Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                        return Replacement::Error(syn::Error::new(
                                            span,
                                            "expected integer for end of range",
                                        ));
                                    }
                                    Bound::Included(Ok(end)) => end.saturating_add(1),
                                    Bound::Excluded(Ok(end)) => end,
                                    Bound::Unbounded => match self.typle_len {
                                        Some(end) => end,
                                        None => {
                                            return Replacement::Error(syn::Error::new(
                                                expr.span(),
                                                "need an explicit range end",
                                            ));
                                        }
                                    },
                                };
                                return Replacement::Iterator(Box::new((start..end).map({
                                    let span = path.span();
                                    let context = self.clone();
                                    let typle = typle.clone();
                                    move |i| context.get_type(&typle, i, span)
                                })));
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        match self.replace_type(&mut ty) {
            Ok(()) => Replacement::Singleton(ty),
            Err(e) => Replacement::Error(e),
        }
    }

    fn replace_macro_type(&self, m: &mut syn::TypeMacro) -> syn::Result<Option<Type>> {
        // typle_for!(i in .. => Option<T<{i}>) -> (Option<T0>, Option<T1>)
        // typle_for!(i in .. => T::<{i}>::default()) -> (T0::default(), T1::default())
        // as opposed to
        // Option<T> -> Option<(T0, T1)>
        // T::default() -> <(T0, T1)>::default()
        if let Some(macro_name) = m.mac.path.get_ident() {
            if macro_name == "typle_for" {
                let mut tuple = syn::TypeTuple {
                    paren_token: token::Paren::default(),
                    elems: Punctuated::new(),
                };
                let token_stream = std::mem::take(&mut m.mac.tokens);
                let default_span = token_stream.span();
                let mut tokens = token_stream.into_iter();
                let (pattern, range) = self.parse_pattern_range(&mut tokens, default_span)?;
                if range.is_empty() {
                    return Ok(Some(Type::Tuple(tuple)));
                }
                let token_stream = tokens.collect::<TokenStream>();
                let mut context = self.clone();
                if let Some(ident) = &pattern {
                    context.constants.insert(ident.clone(), 0);
                }
                for (index, token_stream) in range.zip_clone(token_stream) {
                    if let Some(ident) = &pattern {
                        *context.constants.get_mut(ident).unwrap() = index;
                    }
                    let token_stream = match context.evaluate_if(token_stream) {
                        Ok(Some(token_stream)) => token_stream,
                        Ok(None) => {
                            continue;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    };
                    let mut ty = syn::parse2::<Type>(token_stream)?;
                    context.replace_type(&mut ty)?;
                    tuple.elems.push(ty);
                }
                return Ok(Some(Type::Tuple(tuple)));
            } else if macro_name == "typle_fold" {
                let default_span = macro_name.span();
                let ty = self.replace_typle_fold_type(&mut m.mac, default_span)?;
                return Ok(Some(ty));
            }
        }
        m.mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut m.mac.tokens))?;
        Ok(None)
    }

    fn replace_typle_fold_type(&self, mac: &mut Macro, default_span: Span) -> syn::Result<Type> {
        let token_stream = std::mem::take(&mut mac.tokens);
        let mut tokens = token_stream.into_iter();
        let mut init_type =
            syn::parse2::<Type>(Self::extract_to_semicolon(&mut tokens, default_span)?)?;
        self.replace_type(&mut init_type)?;
        let (pattern, mut range) = self.parse_pattern_range(&mut tokens, default_span)?;
        if range.is_empty() {
            return Ok(init_type);
        }
        let folded_type = if let Some(last_index) = range.next_back() {
            let fold_ident = Self::parse_fold_ident(&mut tokens, default_span)?;
            let wrapping_type = syn::parse2::<Type>(tokens.collect())?;
            let mut context = self.clone();
            if let Some(ident) = &pattern {
                context.constants.insert(ident.clone(), 0);
            }
            context.retypes.insert(
                fold_ident.clone(),
                Type::Verbatim(init_type.into_token_stream()),
            );
            for index in range {
                if let Some(ident) = &pattern {
                    *context.constants.get_mut(ident).unwrap() = index;
                }
                let mut folded_type = wrapping_type.clone();
                context.replace_type(&mut folded_type)?;
                // retypes get cloned when they are inserted. This type can grow into
                // a large nested type that is expensive to clone. Encoding the type
                // as a verbatim TokenStream allows faster cloning. The type has had
                // replacements done at this point so we do not need the structure.
                *context.retypes.get_mut(&fold_ident).unwrap() =
                    Type::Verbatim(folded_type.into_token_stream());
            }
            if let Some(ident) = &pattern {
                *context.constants.get_mut(ident).unwrap() = last_index;
            }
            let mut folded_type = wrapping_type;
            context.replace_type(&mut folded_type)?;
            folded_type
        } else {
            init_type
        };
        Ok(folded_type)
    }
}
