use crate::context::shared::Replacement;

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
                    Replacement::Singleton(inner) => {
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

    pub(super) fn replace_pat_in_list(&self, mut pat: Pat) -> Replacement<Pat> {
        let mut state = BlockState::default();
        match &mut pat {
            Pat::Macro(syn::PatMacro { mac, .. }) => {
                if let Some(ident) = mac.path.get_ident() {
                    if ident == "typle" || ident == "typle_args" {
                        let token_stream = std::mem::take(&mut mac.tokens);
                        return self.expand_typle_macro(token_stream, |context, token_stream| {
                            let mut pat = Pat::parse_single.parse2(token_stream)?;
                            let mut state = BlockState::default();
                            context.replace_pat(&mut pat, &mut state)?;
                            Ok(pat)
                        });
                    }
                }
            }
            _ => {}
        }
        match self.replace_pat(&mut pat, &mut state) {
            Ok(()) => Replacement::Singleton(pat),
            Err(e) => Replacement::Error(e),
        }
    }

    fn replace_macro_pat(&self, m: &mut syn::PatMacro) -> syn::Result<Option<Pat>> {
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
                            let mut state = BlockState::default();
                            context.replace_pat(&mut component, &mut state)?;
                            tuple.elems.push(component);
                        }
                    }
                    MacroDelimiter::Brace(_) => {
                        let Ok(expr) = syn::parse2::<Expr>(token_stream) else {
                            return Err(syn::Error::new(
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
                                context.replace_pat(&mut pat, &mut state)?;
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

fn expr_to_pat(expr: Expr) -> syn::Result<Pat> {
    let default_span = expr.span();
    match expr {
        Expr::Block(expr) => {
            let mut iter = expr.block.stmts.into_iter();
            let Some(stmt) = iter.next() else {
                // empty block represents missing pattern
                return Ok(Pat::Verbatim(TokenStream::new()));
            };
            if iter.next().is_some() {
                return Err(syn::Error::new(
                    default_span,
                    "typle requires a block with a single pattern",
                ));
            }
            match stmt {
                Stmt::Local(local) => Err(syn::Error::new(
                    local.span(),
                    "let statement not supported here",
                )),
                Stmt::Item(item) => Err(syn::Error::new(item.span(), "item not supported here")),
                Stmt::Expr(expr, None) => expr_to_pat(expr),
                Stmt::Expr(_, Some(semi)) => {
                    Err(syn::Error::new(semi.span(), "unexpected semicolon"))
                }
                Stmt::Macro(m) => match m.mac.path.get_ident() {
                    Some(ident) if ident == "typle_pat" => Pat::parse_single.parse2(m.mac.tokens),
                    _ => Ok(Pat::Macro(syn::PatMacro {
                        attrs: m.attrs,
                        mac: m.mac,
                    })),
                },
            }
        }
        Expr::Const(expr) => Ok(Pat::Const(expr)),
        Expr::Infer(expr) => Ok(Pat::Wild(syn::PatWild {
            attrs: expr.attrs,
            underscore_token: expr.underscore_token,
        })),
        Expr::Lit(expr) => Ok(Pat::Lit(expr)),
        Expr::Macro(expr) => match expr.mac.path.get_ident() {
            Some(ident) if ident == "typle_pat" => Pat::parse_single.parse2(expr.mac.tokens),
            _ => Ok(Pat::Macro(expr)),
        },
        Expr::Paren(expr) => Ok(Pat::Paren(syn::PatParen {
            attrs: expr.attrs,
            paren_token: expr.paren_token,
            pat: Box::new(expr_to_pat(*expr.expr)?),
        })),
        Expr::Path(expr) => Ok(Pat::Path(expr)),
        Expr::Reference(expr) => Ok(Pat::Reference(syn::PatReference {
            attrs: expr.attrs,
            and_token: expr.and_token,
            mutability: expr.mutability,
            pat: Box::new(expr_to_pat(*expr.expr)?),
        })),
        Expr::Tuple(expr) => Ok(Pat::Tuple(syn::PatTuple {
            attrs: expr.attrs,
            paren_token: expr.paren_token,
            elems: expr
                .elems
                .into_iter()
                .map(expr_to_pat)
                .collect::<syn::Result<_>>()?,
        })),
        Expr::Verbatim(token_stream) => Ok(Pat::Verbatim(token_stream)),
        _ => Err(syn::Error::new(
            default_span,
            "typle does not support this pattern",
        )),
    }
}
