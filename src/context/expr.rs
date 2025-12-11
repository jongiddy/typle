use either::Either;

use crate::context::shared::Replacements;

use super::*;

impl TypleContext {
    pub(super) fn replace_expr(&self, expr: &mut Expr, state: &mut BlockState) -> syn::Result<()> {
        match expr {
            Expr::Array(array) => {
                self.replace_attrs(&mut array.attrs)?;
                array.elems = std::mem::take(&mut array.elems)
                    .into_iter()
                    .flat_map(move |expr| self.replace_expr_in_list(expr))
                    .collect::<syn::Result<_>>()?;
            }
            Expr::Assign(assign) => {
                self.replace_attrs(&mut assign.attrs)?;
                self.replace_expr(&mut assign.left, state)?;
                self.replace_expr(&mut assign.right, state)?;
            }
            Expr::Async(r#async) => {
                self.replace_attrs(&mut r#async.attrs)?;
                self.replace_block(&mut r#async.block, state)?;
            }
            Expr::Await(r#await) => {
                self.replace_attrs(&mut r#await.attrs)?;
                self.replace_expr(&mut r#await.base, state)?;
            }
            Expr::Binary(binary) => {
                self.replace_attrs(&mut binary.attrs)?;
                self.replace_expr(&mut binary.left, state)?;
                self.replace_expr(&mut binary.right, state)?;
            }
            Expr::Block(block) => {
                self.replace_attrs(&mut block.attrs)?;
                self.replace_block(&mut block.block, state)?;
            }
            Expr::Break(brk) => {
                self.replace_attrs(&mut brk.attrs)?;
                if let Some(expr) = &mut brk.expr {
                    self.replace_expr(expr, state)?;
                }
                match &brk.label {
                    Some(lt) => {
                        state.labelled_control_flow.insert(lt.ident.clone());
                    }
                    None => {
                        state.unlabelled_break = true;
                    }
                }
            }
            Expr::Call(call) => {
                self.replace_attrs(&mut call.attrs)?;
                self.replace_expr(&mut call.func, state)?;
                call.args = std::mem::take(&mut call.args)
                    .into_iter()
                    .flat_map(move |expr| self.replace_expr_in_list(expr))
                    .collect::<syn::Result<_>>()?;
            }
            Expr::Cast(cast) => {
                self.replace_attrs(&mut cast.attrs)?;
                self.replace_expr(&mut cast.expr, state)?;
                self.replace_type(&mut cast.ty)?;
            }
            Expr::Closure(closure) => {
                self.replace_attrs(&mut closure.attrs)?;
                for pat in &mut closure.inputs {
                    self.replace_pat(pat, state)?;
                }
                if let ReturnType::Type(_, ret_type) = &mut closure.output {
                    self.replace_type(ret_type)?;
                }
                self.replace_expr(&mut closure.body, state)?;
            }
            Expr::Const(constant) => {
                self.replace_attrs(&mut constant.attrs)?;
                self.replace_block(&mut constant.block, state)?;
            }
            Expr::Continue(cont) => {
                self.replace_attrs(&mut cont.attrs)?;
                match &cont.label {
                    Some(lt) => {
                        state.labelled_control_flow.insert(lt.ident.clone());
                    }
                    None => {
                        state.unlabelled_continue = Some(cont.span());
                    }
                }
            }
            Expr::Field(field) => {
                self.replace_attrs(&mut field.attrs)?;
                self.replace_expr(&mut field.base, state)?;
            }
            Expr::ForLoop(for_loop) => {
                self.replace_attrs(&mut for_loop.attrs)?;
                self.replace_expr(&mut for_loop.expr, state)?;
                // Check for typle_index!(i).
                if let Pat::Macro(pat_macro) = &mut *for_loop.pat {
                    if let Some(macro_ident) = pat_macro.mac.path.get_ident() {
                        if macro_ident == "typle_index" {
                            let span = pat_macro.mac.tokens.span();
                            let brace_token = for_loop.body.brace_token;
                            let mut tokens = std::mem::take(&mut pat_macro.mac.tokens).into_iter();
                            let Some(TokenTree::Ident(pat_ident)) = tokens.next() else {
                                return Err(syn::Error::new(
                                    span,
                                    "expected identifier in typle_index macro",
                                ));
                            };
                            if let Some(tt) = tokens.next() {
                                abort!(tt, "unexpected token in typle_index");
                            };
                            let Some((start, end)) = evaluate_range(&for_loop.expr) else {
                                abort!(for_loop.expr, "expected range");
                            };
                            let start = match start {
                                Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                    abort!(span, "expected integer for start of range");
                                }
                                Bound::Included(Ok(start)) => start,
                                Bound::Excluded(Ok(start)) => start.saturating_add(1),
                                Bound::Unbounded => {
                                    abort!(for_loop.expr, "need an explicit range start")
                                }
                            };
                            let end = match end {
                                Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                    abort!(span, "expected integer for end of range");
                                }
                                Bound::Included(Ok(end)) => end.saturating_add(1),
                                Bound::Excluded(Ok(end)) => end,
                                Bound::Unbounded => {
                                    abort!(for_loop.expr, "need an explicit range end")
                                }
                            };
                            if end <= start {
                                *expr = Expr::Block(syn::ExprBlock {
                                    attrs: std::mem::take(&mut for_loop.attrs),
                                    label: None,
                                    block: Block {
                                        brace_token,
                                        stmts: Vec::new(),
                                    },
                                });
                                return Ok(());
                            }
                            let mut context = self.clone();
                            let mut stmts = Vec::with_capacity(end.saturating_sub(start) + 1);
                            let mut has_typle_break = false;
                            let mut check_for_break = false;
                            context.constants.insert(pat_ident.clone(), 0);
                            for (index, mut block) in (start..end).zip_clone(std::mem::replace(
                                &mut for_loop.body,
                                Block {
                                    brace_token,
                                    stmts: Vec::new(),
                                },
                            )) {
                                if let Some(v) = context.constants.get_mut(&pat_ident) {
                                    *v = index;
                                }
                                let mut inner_state = BlockState::default();
                                // Evaluate the body for this iteration
                                context.replace_block(&mut block, &mut inner_state)?;
                                // If it evaluates to an empty body, ignore it
                                if block.stmts.is_empty() {
                                    continue;
                                }
                                // If the previous iteration body called `break` exit the loop early.
                                if check_for_break {
                                    let stmt = syn::parse_quote! {
                                        if _typle_break {
                                            break;
                                        }
                                    };
                                    stmts.push(stmt);
                                    check_for_break = false;
                                }

                                if let Some(label) =
                                    inner_state.has_labelled_control_flow(for_loop.label.as_ref())
                                {
                                    // Labelled control flow requires a labelled inner loop.
                                    if !has_typle_break {
                                        let stmt = syn::parse_quote! {
                                            let mut _typle_break = false;
                                        };
                                        stmts.push(stmt);
                                        has_typle_break = true;
                                    }
                                    let stmt = syn::parse_quote! {
                                         #label loop {
                                            if _typle_break {
                                                _typle_break = false;
                                                break;
                                            }
                                            _typle_break = true;
                                            #block
                                        }
                                    };
                                    stmts.push(stmt);
                                    check_for_break = true;
                                } else if inner_state.unlabelled_continue.is_some() {
                                    // Unlabelled `continue` needs an inner loop to continue to.
                                    if !has_typle_break {
                                        let stmt = syn::parse_quote! {
                                            let mut _typle_break = false;
                                        };
                                        stmts.push(stmt);
                                        has_typle_break = true;
                                    }
                                    let stmt = syn::parse_quote! {
                                        loop {
                                            if _typle_break {
                                                _typle_break = false;
                                                break;
                                            }
                                            _typle_break = true;
                                            #block
                                        }
                                    };
                                    stmts.push(stmt);
                                    check_for_break = inner_state.unlabelled_break;
                                } else {
                                    // Bodies with no `break` or `continue`, or with only an
                                    // unlabelled `break`, can run without an inner loop.
                                    stmts.push(Stmt::Expr(
                                        Expr::Block(syn::ExprBlock {
                                            attrs: Vec::new(),
                                            label: None,
                                            block,
                                        }),
                                        None,
                                    ));
                                }
                                state.propagate(inner_state, for_loop.label.as_ref());
                            }
                            // End the bodies with an unconditional `break` out of the outer loop.
                            stmts.push(Stmt::Expr(
                                Expr::Break(syn::ExprBreak {
                                    attrs: Vec::new(),
                                    break_token: token::Break::default(),
                                    label: None,
                                    expr: None,
                                }),
                                Some(token::Semi::default()),
                            ));
                            // Enclose all the statements in an outer loop.
                            *expr = Expr::Loop(syn::ExprLoop {
                                attrs: std::mem::take(&mut for_loop.attrs),
                                label: None,
                                loop_token: token::Loop::default(),
                                body: Block { brace_token, stmts },
                            });
                            return Ok(());
                        }
                    }
                }
                // Otherwise it is a standard for loop
                self.replace_pat(&mut for_loop.pat, state)?;
                let mut inner_state = BlockState::default();
                self.replace_block(&mut for_loop.body, &mut inner_state)?;
                state.propagate(inner_state, for_loop.label.as_ref());
            }
            Expr::Group(group) => {
                self.replace_attrs(&mut group.attrs)?;
                self.replace_expr(&mut group.expr, state)?;
            }
            Expr::If(r#if) => {
                self.replace_attrs(&mut r#if.attrs)?;
                // Check for if typle_const!(i == T::LEN) {}
                if let Some(b) = self.evaluate_as_const_if(&mut r#if.cond, state)? {
                    if b {
                        let brace_token = r#if.then_branch.brace_token;
                        *expr = Expr::Block(syn::ExprBlock {
                            attrs: std::mem::take(&mut r#if.attrs),
                            label: None,
                            block: std::mem::replace(
                                &mut r#if.then_branch,
                                Block {
                                    brace_token,
                                    stmts: Vec::new(),
                                },
                            ),
                        });
                        self.replace_expr(expr, state)?;
                    } else {
                        match r#if.else_branch.take() {
                            Some((_, branch)) => {
                                *expr = *branch;
                                self.replace_expr(expr, state)?;
                            }
                            None => {
                                *expr = Expr::Verbatim(
                                    TokenTree::Group(Group::new(
                                        Delimiter::Brace,
                                        TokenStream::new(),
                                    ))
                                    .into(),
                                );
                            }
                        }
                    }
                    return Ok(());
                }

                self.replace_expr(&mut r#if.cond, state)?;
                self.replace_block(&mut r#if.then_branch, state)?;
                if let Some((_, block)) = &mut r#if.else_branch {
                    self.replace_expr(block, state)?;
                }
            }
            Expr::Index(index) => {
                self.replace_attrs(&mut index.attrs)?;
                self.replace_expr(&mut index.expr, state)?;
                if let Expr::Array(array) = &mut *index.index {
                    // t[[0]]
                    let mut iter = array.elems.iter_mut().fuse();
                    let (Some(field), None) = (iter.next(), iter.next()) else {
                        abort!(index.index, "unsupported tuple index1");
                    };
                    self.replace_expr(field, state)?;
                    let deb = field.clone();
                    let Some(i) = evaluate_usize(field) else {
                        abort!(
                            index.index,
                            format!("unsupported tuple index {:?} {:?}", self.constants, deb)
                        );
                    };
                    *expr = Expr::Field(syn::ExprField {
                        attrs: std::mem::take(&mut index.attrs),
                        base: index.expr.clone(),
                        dot_token: token::Dot::default(),
                        member: Member::Unnamed(syn::Index {
                            index: i as u32,
                            span: index.index.span(),
                        }),
                    });
                } else {
                    self.replace_expr(&mut index.index, state)?;
                }
            }
            Expr::Let(r#let) => {
                self.replace_attrs(&mut r#let.attrs)?;
                self.replace_pat(&mut r#let.pat, state)?;
                self.replace_expr(&mut r#let.expr, state)?;
            }
            Expr::Loop(r#loop) => {
                self.replace_attrs(&mut r#loop.attrs)?;
                let mut inner_state = BlockState::default();
                self.replace_block(&mut r#loop.body, &mut inner_state)?;
                state.propagate(inner_state, r#loop.label.as_ref());
            }
            Expr::Macro(r#macro) => {
                if let Some(e) =
                    self.replace_macro_expr(&mut r#macro.mac, &mut r#macro.attrs, state)?
                {
                    *expr = e;
                }
            }
            Expr::Match(mat) => {
                self.replace_attrs(&mut mat.attrs)?;
                self.replace_expr(&mut mat.expr, state)?;

                let mut arms = Vec::new();
                arms.reserve(mat.arms.len());
                for mut arm in std::mem::replace(&mut mat.arms, arms) {
                    if let Pat::Ident(syn::PatIdent {
                        attrs,
                        ident,
                        subpat: Some((_, subpat)),
                        ..
                    }) = &arm.pat
                    {
                        if let Pat::Macro(mac) = &**subpat {
                            if let Some(macro_ident) = mac.mac.path.get_ident() {
                                if macro_ident == "typle_index" {
                                    let mut expr = syn::parse2::<Expr>(mac.mac.tokens.clone())?;
                                    let mut state = BlockState::default();
                                    self.replace_expr(&mut expr, &mut state)?;
                                    let Some((start, end)) = evaluate_range(&expr) else {
                                        abort!(expr, "expected range");
                                    };
                                    let start = match start {
                                        Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                            abort!(span, "expected integer for start of range");
                                        }
                                        Bound::Included(Ok(start)) => start,
                                        Bound::Excluded(Ok(start)) => start.saturating_add(1),
                                        Bound::Unbounded => 0,
                                    };
                                    let end = match end {
                                        Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                            abort!(span, "expected integer for end of range");
                                        }
                                        Bound::Included(Ok(end)) => end.saturating_add(1),
                                        Bound::Excluded(Ok(end)) => end,
                                        Bound::Unbounded => match self.typle_len {
                                            Some(end) => end,
                                            None => {
                                                abort!(expr, "need an explicit range end");
                                            }
                                        },
                                    };
                                    let default_span = ident.span();
                                    let mut context = self.clone();
                                    context.constants.insert(ident.clone(), 0);
                                    for arm in
                                        (start..end).zip_clone(arm.body).map(|(index, mut body)| {
                                            *context.constants.get_mut(ident).unwrap() = index;
                                            let mut state = BlockState::default();
                                            context.replace_expr(&mut body, &mut state)?;
                                            Ok::<_, syn::Error>(syn::Arm {
                                                attrs: attrs.clone(),
                                                pat: Pat::Lit(syn::ExprLit {
                                                    attrs: vec![],
                                                    lit: Lit::Int(syn::LitInt::new(
                                                        &index.to_string(),
                                                        default_span,
                                                    )),
                                                }),
                                                guard: None,
                                                fat_arrow_token: arm.fat_arrow_token,
                                                body,
                                                comma: Some(token::Comma::default()),
                                            })
                                        })
                                    {
                                        match arm {
                                            Ok(arm) => mat.arms.push(arm),
                                            Err(err) => {
                                                abort!(expr, err.to_string())
                                            }
                                        }
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    self.replace_pat(&mut arm.pat, state)?;
                    if let Some((_, expr)) = &mut arm.guard {
                        self.replace_expr(expr, state)?;
                    }
                    self.replace_expr(&mut arm.body, state)?;
                    mat.arms.push(arm);
                }
            }
            Expr::MethodCall(method_call) => {
                self.replace_attrs(&mut method_call.attrs)?;
                self.replace_expr(&mut method_call.receiver, state)?;
                if let Some(args) = &mut method_call.turbofish {
                    // Arc::<T>::new(t)
                    self.replace_generic_arguments(&mut args.args)?;
                    if args.args.is_empty() {
                        method_call.turbofish = None;
                    }
                }
                method_call.args = std::mem::take(&mut method_call.args)
                    .into_iter()
                    .flat_map(move |expr| self.replace_expr_in_list(expr))
                    .collect::<syn::Result<_>>()?;
            }
            Expr::Paren(paren) => {
                self.replace_attrs(&mut paren.attrs)?;
                match self.replace_expr_in_list(std::mem::replace(
                    &mut paren.expr,
                    Expr::Verbatim(TokenStream::new()),
                )) {
                    Replacements::Singleton(Ok(inner)) => {
                        paren.expr = Box::new(inner);
                    }
                    iter => {
                        *expr = Expr::Tuple(syn::ExprTuple {
                            attrs: std::mem::take(&mut paren.attrs),
                            paren_token: paren.paren_token,
                            elems: iter.collect::<syn::Result<_>>()?,
                        });
                    }
                }
            }
            Expr::Path(path) => {
                self.replace_attrs(&mut path.attrs)?;
                if let Some(lit) = self.replace_typle_associated_const(path, state)? {
                    *expr = Expr::Lit(syn::ExprLit {
                        attrs: std::mem::take(&mut path.attrs),
                        lit,
                    });
                    return Ok(());
                }
                self.replace_qself_path(&mut path.qself, &mut path.path)?;
                self.replace_path_arguments(&mut path.path)?;
            }
            Expr::Range(range) => {
                self.replace_attrs(&mut range.attrs)?;
                if let Some(start) = &mut range.start {
                    self.replace_expr(start, state)?;
                }
                if let Some(end) = &mut range.end {
                    self.replace_expr(end, state)?;
                }
            }
            Expr::Reference(reference) => {
                self.replace_attrs(&mut reference.attrs)?;
                self.replace_expr(&mut reference.expr, state)?;
            }
            Expr::Repeat(repeat) => {
                self.replace_attrs(&mut repeat.attrs)?;
                self.replace_expr(&mut repeat.expr, state)?;
                self.replace_expr(&mut repeat.len, state)?;
            }
            Expr::Return(r#return) => {
                self.replace_attrs(&mut r#return.attrs)?;
                if let Some(expr) = &mut r#return.expr {
                    self.replace_expr(expr, state)?;
                }
            }
            Expr::Struct(r#struct) => {
                self.replace_attrs(&mut r#struct.attrs)?;
                self.replace_path_arguments(&mut r#struct.path)?;
                for field in &mut r#struct.fields {
                    self.replace_expr(&mut field.expr, state)?;
                }
                if let Some(expr) = &mut r#struct.rest {
                    self.replace_expr(expr, state)?;
                }
            }
            Expr::Try(r#try) => {
                self.replace_attrs(&mut r#try.attrs)?;
                self.replace_expr(&mut r#try.expr, state)?;
            }
            Expr::TryBlock(try_block) => {
                self.replace_attrs(&mut try_block.attrs)?;
                self.replace_block(&mut try_block.block, state)?;
            }
            Expr::Tuple(tuple) => {
                self.replace_attrs(&mut tuple.attrs)?;
                tuple.elems = std::mem::take(&mut tuple.elems)
                    .into_iter()
                    .flat_map(move |expr| self.replace_expr_in_list(expr))
                    .collect::<syn::Result<_>>()?;
            }
            Expr::Unary(unary) => {
                self.replace_attrs(&mut unary.attrs)?;
                self.replace_expr(&mut unary.expr, state)?;
            }
            Expr::Unsafe(r#unsafe) => {
                self.replace_attrs(&mut r#unsafe.attrs)?;
                self.replace_block(&mut r#unsafe.block, state)?;
            }
            Expr::While(r#while) => {
                self.replace_attrs(&mut r#while.attrs)?;
                self.replace_expr(&mut r#while.cond, state)?;
                let mut inner_state = BlockState::default();
                self.replace_block(&mut r#while.body, &mut inner_state)?;
                state.propagate(inner_state, r#while.label.as_ref());
            }
            Expr::Yield(r#yield) => {
                self.replace_attrs(&mut r#yield.attrs)?;
                if let Some(expr) = &mut r#yield.expr {
                    self.replace_expr(expr, state)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    pub(super) fn replace_expr_in_list(
        &self,
        mut expr: Expr,
    ) -> Replacements<impl Iterator<Item = syn::Result<Expr>>> {
        let mut state = BlockState::default();
        match &mut expr {
            Expr::Macro(syn::ExprMacro { mac, .. }) => {
                if let Some(ident) = mac.path.get_ident() {
                    if ident == "typle" || ident == "typle_args" {
                        let token_stream = std::mem::take(&mut mac.tokens);
                        return self
                            .expand_typle_macro(token_stream, |context, token_stream| {
                                let mut expr = syn::parse2::<Expr>(token_stream)?;
                                let mut state = BlockState::default();
                                context.replace_expr(&mut expr, &mut state)?;
                                Ok(expr)
                            })
                            .map_iterator(Either::Left);
                    }
                }
            }
            Expr::Index(syn::ExprIndex { expr, index, .. }) => {
                if let Expr::Array(array) = &mut **index {
                    // t[[..]]
                    let mut iter = array.elems.iter_mut().fuse();
                    if let (Some(field), None) = (iter.next(), iter.next()) {
                        if let Err(e) = self.replace_expr(field, &mut state) {
                            return Replacements::Singleton(Err(e));
                        }
                        if let Some((start, end)) = evaluate_range(field) {
                            let start = match start {
                                Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                    return Replacements::Singleton(Err(syn::Error::new(
                                        span,
                                        "expected integer for start of range",
                                    )));
                                }
                                Bound::Included(Ok(start)) => start,
                                Bound::Excluded(Ok(start)) => start.saturating_add(1),
                                Bound::Unbounded => 0,
                            };
                            let end = match end {
                                Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                    return Replacements::Singleton(Err(syn::Error::new(
                                        span,
                                        "expected integer for end of range",
                                    )));
                                }
                                Bound::Included(Ok(end)) => end.saturating_add(1),
                                Bound::Excluded(Ok(end)) => end,
                                Bound::Unbounded => match self.typle_len {
                                    Some(end) => end,
                                    None => {
                                        return Replacements::Singleton(Err(syn::Error::new(
                                            expr.span(),
                                            "need an explicit range end",
                                        )));
                                    }
                                },
                            };
                            return Replacements::Iterator(Either::Right({
                                let span = index.span();
                                (start..end).zip_clone(expr.clone()).map(move |(i, base)| {
                                    Ok(Expr::Field(syn::ExprField {
                                        attrs: Vec::new(),
                                        base,
                                        dot_token: token::Dot::default(),
                                        member: Member::Unnamed(syn::Index {
                                            index: i as u32,
                                            span,
                                        }),
                                    }))
                                })
                            }));
                        }
                    }
                }
            }
            _ => {}
        }
        match self.replace_expr(&mut expr, &mut state) {
            Ok(()) => Replacements::Singleton(Ok(expr)),
            Err(e) => Replacements::Singleton(Err(e)),
        }
    }

    // Return Some(bool) if expression is a const-if. Otherwise, return None.
    fn evaluate_as_const_if(
        &self,
        cond: &mut Expr,
        state: &mut BlockState,
    ) -> syn::Result<Option<bool>> {
        if let Expr::Macro(expr_macro) = cond {
            if let Some(macro_ident) = expr_macro.mac.path.get_ident() {
                if macro_ident == "typle_const" {
                    let tokens = std::mem::take(&mut expr_macro.mac.tokens);
                    let mut cond = syn::parse2::<Expr>(tokens)?;
                    self.replace_expr(&mut cond, state)?;
                    let b = evaluate_bool(&cond)?;
                    return Ok(Some(b));
                }
            }
        }
        if self.const_if {
            self.replace_expr(cond, state)?;
            let b = evaluate_bool(&*cond)?;
            return Ok(Some(b));
        }
        Ok(None)
    }
}
