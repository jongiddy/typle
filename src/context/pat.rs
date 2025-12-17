use crate::context::shared::Replacements;

use super::*;

impl TypleContext {
    pub(super) fn replace_pat(&self, pat: &mut Pat, state: &mut BlockState) -> syn::Result<()> {
        match pat {
            Pat::Macro(m) => {
                if let Some(p) = self.replace_macro_pat(m)? {
                    *pat = p;
                }
            }
            Pat::Or(or) => {
                for pat in &mut or.cases {
                    self.replace_pat(pat, state)?;
                }
            }
            Pat::Paren(paren) => {
                self.replace_attrs(&mut paren.attrs)?;
                match self.replace_pat_in_list(std::mem::replace(
                    &mut paren.pat,
                    Pat::Verbatim(TokenStream::new()),
                )) {
                    Replacements::Singleton(Ok(inner)) => {
                        paren.pat = Box::new(inner);
                    }
                    iter => {
                        *pat = Pat::Tuple(syn::PatTuple {
                            attrs: std::mem::take(&mut paren.attrs),
                            paren_token: paren.paren_token,
                            elems: iter.collect::<syn::Result<_>>()?,
                        });
                    }
                }
            }
            Pat::Path(path) => {
                // State::S::<typle_ident!(i)> -> State::S2
                self.replace_attrs(&mut path.attrs)?;
                if let Some(lit) = self.replace_typle_associated_const(path, state)? {
                    *pat = Pat::Lit(syn::PatLit {
                        attrs: std::mem::take(&mut path.attrs),
                        lit,
                    });
                    return Ok(());
                }
                self.replace_qself_path(&mut path.qself, &mut path.path)?;
                self.replace_path_arguments(&mut path.path)?;
            }
            Pat::Reference(reference) => {
                self.replace_pat(&mut reference.pat, state)?;
            }
            Pat::Slice(slice) => {
                for pat in &mut slice.elems {
                    self.replace_pat(pat, state)?;
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
                    .collect::<syn::Result<_>>()?;
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
                self.replace_pat(&mut pat_type.pat, state)?;
                self.replace_type(&mut pat_type.ty)?;
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn replace_pat_in_list(
        &self,
        mut pat: Pat,
    ) -> Replacements<impl Iterator<Item = syn::Result<Pat>>> {
        let mut state = BlockState::default();
        #[allow(clippy::single_match)]
        match &mut pat {
            Pat::Macro(syn::PatMacro { mac, .. }) => {
                if let Some(ident) = mac.path.get_ident() {
                    if ident == "typle" {
                        let token_stream = std::mem::take(&mut mac.tokens);
                        return self.expand_typle_macro(token_stream, |context, token_stream| {
                            let mut pat = Pat::parse_single.parse2(token_stream)?;
                            let mut state = BlockState::default();
                            context.replace_pat(&mut pat, &mut state)?;
                            Ok(Replacements::<std::iter::Empty<_>>::Singleton(Ok(pat)))
                        });
                    }
                }
            }
            _ => {}
        }
        match self.replace_pat(&mut pat, &mut state) {
            Ok(()) => Replacements::Singleton(Ok(pat)),
            Err(e) => Replacements::Singleton(Err(e)),
        }
    }

    fn replace_macro_pat(&self, m: &mut syn::PatMacro) -> syn::Result<Option<Pat>> {
        self.replace_attrs(&mut m.attrs)?;
        if let Some(macro_name) = m.mac.path.get_ident() {
            if macro_name == "typle" {
                // This is outside a comma-separated sequence so only no-range form is accepted
                // typle!(=> if T::LEN == 0 {} else {})
                let token_stream = std::mem::take(&mut m.mac.tokens);
                return self.expand_typle_macro_singleton(token_stream, |context, token_stream| {
                    let mut pat = Pat::parse_single.parse2(token_stream)?;
                    let mut state = BlockState::default();
                    context.replace_pat(&mut pat, &mut state)?;
                    Ok(Some(pat))
                });
            }
        }
        m.mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut m.mac.tokens))?;
        Ok(None)
    }
}
