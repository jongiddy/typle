use std::collections::{HashMap, HashSet};
use std::ops::Range;
use std::rc::Rc;

use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::{format_ident, ToTokens};
use syn::parse::Parser as _;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::{
    parse2, parse_quote, token, AttrStyle, Attribute, BinOp, Block, Error, Expr, ExprArray,
    ExprBinary, ExprBlock, ExprField, ExprLit, ExprMacro, ExprParen, ExprPath, ExprTuple, Fields,
    FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Generics, ImplItem, Index, Item,
    ItemImpl, Label, Lit, LitBool, LitInt, Macro, MacroDelimiter, Member, Meta, Pat, PatMacro,
    PatParen, PatReference, PatTuple, PatWild, Path, PathArguments, PathSegment, PredicateType,
    QSelf, RangeLimits, Result, ReturnType, Stmt, Token, Type, TypeInfer, TypeMacro,
    TypeParamBound, TypePath, TypeTuple, Variant, WherePredicate,
};

use crate::constant::{evaluate_bool, evaluate_range, evaluate_usize};
use crate::syn_ext::GeneralFunction;
use crate::TypleMacro;

macro_rules! abort {
    ($spanned:expr, $message:expr) => {
        return Err(Error::new($spanned.span(), $message))
    };
}

#[derive(Clone)]
pub enum Typle {
    Specific(Type),
    Generic(Rc<Vec<String>>),
}

impl Typle {
    pub fn get(&self, i: usize, span: Span) -> Type {
        match self {
            Typle::Specific(r#type) => r#type.clone(),
            Typle::Generic(v) => Type::Path(TypePath {
                qself: None,
                path: ident_to_path(Ident::new(&v[i], span)),
            }),
        }
    }
}

#[derive(Default)]
pub struct BlockState {
    unlabelled_break: bool,
    unlabelled_continue: Option<Span>,
    labelled_control_flow: HashSet<Ident>,
    suspicious_ident: Option<Ident>,
}

impl BlockState {
    // Propagate the labels from an inner loop into the state for this loop.
    // We exclude any unlabelled_control_flow as it is contained by the inner
    // loop. We also exclude any label attached to the inner loop.
    fn propagate(&mut self, inner: Self, label: Option<&Label>) {
        for ident in inner.labelled_control_flow {
            if let Some(label) = label {
                if label.name.ident == ident {
                    continue;
                }
            }
            self.labelled_control_flow.insert(ident);
        }
    }

    // Return whether this loop has any labelled control flow that refers to
    // this loop's label.
    fn has_labelled_control_flow<'a>(&self, label: Option<&'a Label>) -> Option<&'a Label> {
        if let Some(label) = label {
            if self.labelled_control_flow.contains(&label.name.ident) {
                return Some(label);
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct TypleContext<'a> {
    typle_macro: &'a TypleMacro,
    typle_len: Option<usize>,
    constants: HashMap<Ident, usize>,
    typles: HashMap<Ident, Typle>,
    const_if: bool,
    self_replaced: bool,
}

impl<'a> From<&'a TypleMacro> for TypleContext<'a> {
    fn from(typle_macro: &'a TypleMacro) -> Self {
        TypleContext {
            typle_macro,
            typle_len: None,
            constants: HashMap::new(),
            typles: HashMap::new(),
            const_if: false,
            self_replaced: false,
        }
    }
}

impl<'a> TypleContext<'a> {
    pub fn get_type(&self, typle: &Typle, i: usize, span: Span) -> Result<Type> {
        match self.typle_len {
            Some(typle_len) => {
                if i < typle_len {
                    Ok(typle.get(i, span))
                } else if i < self.typle_macro.max_len {
                    Ok(self.typle_macro.never_type.clone())
                } else {
                    Err(Error::new(span, "typle index out of range"))
                }
            }
            None => {
                if i < self.typle_macro.max_len {
                    Ok(typle.get(i, span))
                } else {
                    Err(Error::new(span, "typle index out of range"))
                }
            }
        }
    }

    pub fn extract_typle_constraints(&self, generics: &mut Generics) -> Result<Option<Self>> {
        fn get_type_ident(predicate_type: &PredicateType) -> Option<&Ident> {
            if let Type::Path(type_path) = &predicate_type.bounded_ty {
                if type_path.qself.is_none() {
                    return type_path.path.get_ident();
                }
            }
            None
        }

        let mut context = None;

        for param in &mut generics.params {
            if let GenericParam::Type(type_param) = param {
                let type_ident = &type_param.ident;
                if let Some(typle) = self.typle_bounds(&type_param.bounds, type_ident)? {
                    let context = context.get_or_insert_with(|| self.clone());
                    context.typles.insert(type_ident.clone(), typle);
                    type_param.bounds = Punctuated::new();
                }
            }
        }

        if let Some(where_clause) = generics.where_clause.as_mut() {
            let predicates = std::mem::take(&mut where_clause.predicates);
            for predicate in predicates {
                if let WherePredicate::Type(predicate_type) = &predicate {
                    if let Some(type_ident) = get_type_ident(predicate_type) {
                        if let Some(typle) =
                            self.typle_bounds(&predicate_type.bounds, type_ident)?
                        {
                            let context = context.get_or_insert_with(|| self.clone());
                            context.typles.insert(type_ident.clone(), typle);
                            continue;
                        }
                    }
                }
                where_clause.predicates.push(predicate);
            }
        }
        Ok(context)
    }

    fn typle_bounds(
        &self,
        bounds: &Punctuated<TypeParamBound, token::Plus>,
        type_ident: &Ident,
    ) -> Result<Option<Typle>> {
        let mut bounds = bounds.iter();
        if let Some(TypeParamBound::Trait(trait_bound)) = bounds.next() {
            if bounds.next().is_none() {
                let path = &trait_bound.path;
                if path.leading_colon.is_none() && path.segments.len() == 1 {
                    if let Some(segment) = path.segments.first() {
                        if segment.ident == self.typle_macro.ident {
                            match &segment.arguments {
                                PathArguments::None => {
                                    return Ok(Some(Typle::Generic(Rc::new(
                                        (0..self.typle_len.unwrap_or(self.typle_macro.max_len))
                                            .map(|i| format!("{}{}", &type_ident, i))
                                            .collect(),
                                    ))));
                                }
                                PathArguments::AngleBracketed(arguments) => {
                                    if arguments.args.len() != 1 {
                                        abort!(arguments, "expected single argument");
                                    }
                                    if let Some(GenericArgument::Type(ty)) = arguments.args.first()
                                    {
                                        return Ok(Some(Typle::Specific(ty.clone())));
                                    }
                                    abort!(arguments, "expected type");
                                }
                                PathArguments::Parenthesized(arguments) => {
                                    abort!(arguments, "parenthesized arguments not supported");
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    // Replace #[typle_attr_if(T::LEN == 1, unused_mut)]
    fn replace_attrs(&self, attrs: &mut Vec<Attribute>) -> Result<()> {
        if attrs.iter().any(|attr| {
            if let Some(ident) = attr.path().get_ident() {
                ident == "typle_attr_if"
            } else {
                false
            }
        }) {
            for mut attr in std::mem::replace(attrs, Vec::with_capacity(attrs.len())) {
                if let (AttrStyle::Outer, Meta::List(meta_list)) = (&attr.style, &mut attr.meta) {
                    if let Some(ident) = meta_list.path.get_ident() {
                        if ident == "typle_attr_if" {
                            let mut tokens = std::mem::take(&mut meta_list.tokens).into_iter();
                            let expr_tokens = tokens
                                .by_ref()
                                .take_while(
                                    |tt| !matches!(tt, TokenTree::Punct(p) if p.as_char() == ','),
                                )
                                .collect();
                            let mut expr = parse2::<Expr>(expr_tokens)?;
                            let mut state = BlockState::default();
                            self.replace_expr(&mut expr, &mut state)?;
                            if evaluate_bool(&expr)? {
                                meta_list.tokens = tokens.collect();
                                let nested = attr.parse_args_with(
                                    Punctuated::<Meta, Token![,]>::parse_terminated,
                                )?;
                                for meta in nested {
                                    attrs.push(Attribute {
                                        pound_token: attr.pound_token,
                                        style: attr.style,
                                        bracket_token: attr.bracket_token,
                                        meta,
                                    });
                                }
                            }
                            continue;
                        }
                    }
                }
                attrs.push(attr);
            }
        }
        Ok(())
    }

    fn replace_block(&self, block: &mut Block, state: &mut BlockState) -> Result<()> {
        let stmts_len = block.stmts.len();
        let mut stmts = std::mem::replace(&mut block.stmts, Vec::with_capacity(stmts_len))
            .into_iter()
            .peekable();
        while let Some(stmt) = stmts.next() {
            match stmt {
                Stmt::Local(mut local) => {
                    self.replace_attrs(&mut local.attrs)?;
                    self.replace_pat(&mut local.pat)?;
                    if let Some(init) = &mut local.init {
                        self.replace_expr(&mut init.expr, state)?;
                        if let Some((_, diverge)) = &mut init.diverge {
                            self.replace_expr(diverge, state)?;
                        }
                    }
                    block.stmts.push(Stmt::Local(local));
                }
                Stmt::Item(item) => {
                    let mut items = Vec::new();
                    self.replace_item(item, &mut items)?;
                    block.stmts.extend(items.into_iter().map(Stmt::Item));
                }
                Stmt::Expr(mut expr, semi) => {
                    self.replace_expr(&mut expr, state)?;
                    // Remove empty blocks in blocks to allow control statements in const-for loop
                    // to be eliminated.
                    match &expr {
                        Expr::Block(ExprBlock {
                            attrs,
                            label,
                            block: inner_block,
                        }) => {
                            if attrs.is_empty()
                                && label.is_none()
                                && inner_block.stmts.is_empty()
                                && (block.stmts.is_empty() || stmts.peek().is_some())
                            {
                                // Don't keep empty blocks inside blocks unless it affects the return value.
                                // i.e. only remove if there have been no statements before the empty block or
                                // if there are more statements afterwards.
                                continue;
                            }
                        }
                        Expr::Verbatim(_token_stream) => {
                            if block.stmts.is_empty() || stmts.peek().is_some() {
                                // Verbatim is only used to provide an empty block
                                continue;
                            }
                        }
                        _ => {}
                    }
                    block.stmts.push(Stmt::Expr(expr, semi));
                }
                Stmt::Macro(mut stmt_macro) => {
                    if let Some(stmt) =
                        self.replace_macro_expr(&mut stmt_macro.mac, &mut stmt_macro.attrs, state)?
                    {
                        block.stmts.push(Stmt::Expr(stmt, stmt_macro.semi_token));
                    } else {
                        block.stmts.push(Stmt::Macro(stmt_macro));
                    }
                }
            }
        }
        Ok(())
    }

    // Return Some(bool) if expression is a const-if. Otherwise, return None.
    fn evaluate_as_const_if(
        &self,
        cond: &mut Expr,
        state: &mut BlockState,
    ) -> Result<Option<bool>> {
        if let Expr::Macro(expr_macro) = cond {
            if let Some(macro_ident) = expr_macro.mac.path.get_ident() {
                if macro_ident == "typle_const" {
                    let tokens = std::mem::take(&mut expr_macro.mac.tokens);
                    let mut cond = parse2::<Expr>(tokens)?;
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

    fn replace_expr(&self, expr: &mut Expr, state: &mut BlockState) -> Result<()> {
        match expr {
            Expr::Array(array) => {
                self.replace_attrs(&mut array.attrs)?;
                for expr in &mut array.elems {
                    self.replace_expr(expr, state)?;
                }
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
                for expr in &mut call.args {
                    self.replace_expr(expr, state)?;
                }
            }
            Expr::Cast(cast) => {
                self.replace_attrs(&mut cast.attrs)?;
                self.replace_expr(&mut cast.expr, state)?;
                self.replace_type(&mut cast.ty)?;
            }
            Expr::Closure(closure) => {
                self.replace_attrs(&mut closure.attrs)?;
                for pat in &mut closure.inputs {
                    self.replace_pat(pat)?;
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
                                return Err(Error::new(
                                    span,
                                    "expected identifier in typle_index macro",
                                ));
                            };
                            if let Some(tt) = tokens.next() {
                                abort!(tt, "unexpected token in typle_index");
                            };
                            let Expr::Range(expr_range) = &*for_loop.expr else {
                                abort!(for_loop.expr, "expected range");
                            };
                            let (Some(start_expr), Some(end_expr)) =
                                (&expr_range.start, &expr_range.end)
                            else {
                                abort!(expr_range, "expected bounded range");
                            };
                            let Some(start) = evaluate_usize(start_expr) else {
                                abort!(
                                    start_expr,
                                    "cannot evaluate lower bound in constant context"
                                );
                            };
                            let Some(mut end) = evaluate_usize(end_expr) else {
                                abort!(end_expr, "cannot evaluate upper bound in constant context");
                            };
                            if let RangeLimits::Closed(_) = expr_range.limits {
                                end += 1;
                            }
                            let mut context = self.clone();
                            let mut stmts = Vec::with_capacity(end.saturating_sub(start) + 1);
                            let mut has_typle_break = false;
                            let mut check_for_break = false;
                            context.constants.insert(pat_ident.clone(), 0);
                            for index in start..end {
                                if let Some(v) = context.constants.get_mut(&pat_ident) {
                                    *v = index;
                                }
                                let mut block = if index == end - 1 {
                                    std::mem::replace(
                                        &mut for_loop.body,
                                        Block {
                                            brace_token,
                                            stmts: Vec::new(),
                                        },
                                    )
                                } else {
                                    for_loop.body.clone()
                                };
                                let mut inner_state = BlockState::default();
                                // Evaluate the body for this iteration
                                context.replace_block(&mut block, &mut inner_state)?;
                                // If it evaluates to an empty body, ignore it
                                if block.stmts.is_empty() {
                                    continue;
                                }
                                // If the previous iteration body called `break` exit the loop early.
                                if check_for_break {
                                    let stmt = parse_quote! {
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
                                        let stmt = parse_quote! {
                                            let mut _typle_break = false;
                                        };
                                        stmts.push(stmt);
                                        has_typle_break = true;
                                    }
                                    let stmt = parse_quote! {
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
                                        let stmt = parse_quote! {
                                            let mut _typle_break = false;
                                        };
                                        stmts.push(stmt);
                                        has_typle_break = true;
                                    }
                                    let stmt = parse_quote! {
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
                                        Expr::Block(ExprBlock {
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
                self.replace_pat(&mut for_loop.pat)?;
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
                        *expr = Expr::Block(ExprBlock {
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
                    let mut iter = array.elems.iter_mut();
                    let (Some(field), None) = (iter.next(), iter.next()) else {
                        abort!(index.index, "unsupported tuple index");
                    };
                    self.replace_expr(field, state)?;
                    let Some(i) = evaluate_usize(field) else {
                        abort!(index.index, "unsupported tuple index");
                    };
                    *expr = Expr::Field(ExprField {
                        attrs: std::mem::take(&mut index.attrs),
                        base: index.expr.clone(),
                        dot_token: token::Dot::default(),
                        member: Member::Unnamed(Index {
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
                self.replace_pat(&mut r#let.pat)?;
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
            Expr::Match(r#match) => {
                self.replace_attrs(&mut r#match.attrs)?;
                self.replace_expr(&mut r#match.expr, state)?;
                for arm in &mut r#match.arms {
                    self.replace_pat(&mut arm.pat)?;
                    if let Some((_, expr)) = &mut arm.guard {
                        self.replace_expr(expr, state)?;
                    }
                    self.replace_expr(&mut arm.body, state)?;
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
                for arg in &mut method_call.args {
                    self.replace_expr(arg, state)?;
                }
            }
            Expr::Paren(paren) => {
                self.replace_attrs(&mut paren.attrs)?;
                self.replace_expr(&mut paren.expr, state)?;
            }
            Expr::Path(path) => {
                self.replace_attrs(&mut path.attrs)?;
                if let Some(qself) = &mut path.qself {
                    // <T as Default>::default()
                    // <T::<0> as Default>::default()
                    self.replace_type(&mut qself.ty)?;
                }
                let mut segments = std::mem::take(&mut path.path.segments)
                    .into_iter()
                    .peekable();
                if let Some(first) = segments.peek() {
                    if first.ident == self.typle_macro.ident {
                        let _ = segments.next().unwrap();
                        if let Some(second) = segments.peek() {
                            if second.ident == "LEN" {
                                // Tuple::LEN or <T as Tuple>::LEN
                                // todo: check that any qself is a type tuple of the correct length
                                let Some(typle_len) = self.typle_len else {
                                    abort!(second.ident, "LEN not available outside fn or impl");
                                };
                                *expr = Expr::Lit(ExprLit {
                                    attrs: std::mem::take(&mut path.attrs),
                                    lit: Lit::Int(LitInt::new(&typle_len.to_string(), path.span())),
                                });
                                return Ok(());
                            } else if second.ident == "MAX" {
                                // Tuple::MAX or <T as Tuple>::MAX
                                *expr = Expr::Lit(ExprLit {
                                    attrs: std::mem::take(&mut path.attrs),
                                    lit: Lit::Int(LitInt::new(
                                        &self.typle_macro.max_len.to_string(),
                                        path.span(),
                                    )),
                                });
                                return Ok(());
                            } else if second.ident == "MIN" {
                                // Tuple::MIN or <T as Tuple>::MIN
                                *expr = Expr::Lit(ExprLit {
                                    attrs: std::mem::take(&mut path.attrs),
                                    lit: Lit::Int(LitInt::new(
                                        &self.typle_macro.min_len.to_string(),
                                        path.span(),
                                    )),
                                });
                                return Ok(());
                            }
                        }
                    } else if let Some(typle) = self.typles.get(&first.ident) {
                        let mut first = segments.next().unwrap();
                        match &mut first.arguments {
                            PathArguments::None => {
                                match segments.peek() {
                                    Some(second) => {
                                        if second.ident == "LEN" {
                                            // T::LEN
                                            let Some(typle_len) = self.typle_len else {
                                                abort!(
                                                    second.ident,
                                                    "LEN not available outside fn or impl"
                                                );
                                            };
                                            *expr = Expr::Lit(ExprLit {
                                                attrs: std::mem::take(&mut path.attrs),
                                                lit: Lit::Int(LitInt::new(
                                                    &typle_len.to_string(),
                                                    path.span(),
                                                )),
                                            });
                                            return Ok(());
                                        } else if second.ident == "MAX" {
                                            // T::MAX
                                            *expr = Expr::Lit(ExprLit {
                                                attrs: std::mem::take(&mut path.attrs),
                                                lit: Lit::Int(LitInt::new(
                                                    &self.typle_macro.max_len.to_string(),
                                                    path.span(),
                                                )),
                                            });
                                            return Ok(());
                                        } else if second.ident == "MIN" {
                                            // T::MIN
                                            *expr = Expr::Lit(ExprLit {
                                                attrs: std::mem::take(&mut path.attrs),
                                                lit: Lit::Int(LitInt::new(
                                                    &self.typle_macro.min_len.to_string(),
                                                    path.span(),
                                                )),
                                            });
                                            return Ok(());
                                        }
                                        // T::clone(&t) -> <(T0, T1)>::clone(&t)
                                        let tuple_type = Box::new(Type::Tuple(TypeTuple {
                                            paren_token: token::Paren::default(),
                                            elems: (0..self
                                                .typle_len
                                                .unwrap_or(self.typle_macro.max_len))
                                                .map(|i| self.get_type(typle, i, first.span()))
                                                .collect::<Result<_>>()?,
                                        }));
                                        path.qself = Some(QSelf {
                                            lt_token: token::Lt::default(),
                                            ty: tuple_type,
                                            position: 0,
                                            as_token: None,
                                            gt_token: token::Gt::default(),
                                        });
                                        path.path.leading_colon = Some(token::PathSep::default());
                                    }
                                    None => {
                                        abort!(first, "type in value position");
                                    }
                                }
                            }
                            PathArguments::AngleBracketed(args) => {
                                // T::<0>::default() -> <T0>::default()
                                if args.args.len() != 1 {
                                    abort!(first, "expected one type parameter");
                                }
                                match args.args.first_mut() {
                                    Some(GenericArgument::Const(expr)) => {
                                        // T<{T::LEN - 1}>
                                        self.replace_expr(expr, state)?;
                                        // T<{5 - 1}>
                                        let Some(value) = evaluate_usize(expr) else {
                                            abort!(expr, "unsupported tuple type index");
                                        };
                                        // T<{4}>
                                        let qself = self.get_type(typle, value, first.span())?;
                                        if path.qself.is_some() {
                                            abort!(first, "not a trait");
                                        }
                                        path.qself = Some(QSelf {
                                            lt_token: token::Lt::default(),
                                            ty: Box::new(qself),
                                            position: 0,
                                            as_token: None,
                                            gt_token: token::Gt::default(),
                                        });
                                        path.path.leading_colon = Some(token::PathSep::default());
                                    }
                                    _ => {
                                        abort!(
                                            args,
                                            "Require const parameter (wrap {} around expression)"
                                        );
                                    }
                                }
                            }
                            PathArguments::Parenthesized(_) => {
                                path.path.segments.push(first);
                            }
                        }
                    } else if let Some(value) = self.constants.get(&first.ident) {
                        *expr = Expr::Lit(ExprLit {
                            attrs: std::mem::take(&mut path.attrs),
                            lit: Lit::Int(LitInt::new(&value.to_string(), first.ident.span())),
                        });
                        return Ok(());
                    } else if self.self_replaced && first.ident == "self" {
                        let x = segments.peek_mut().unwrap();
                        *x = PathSegment {
                            ident: Ident::new("_typle_self", x.span()),
                            arguments: PathArguments::None,
                        };
                    }
                    path.path.segments = segments.collect();
                    self.replace_path_arguments(&mut path.path)?;
                    // If a path looks like a typle associated constant, but has not been evaluated,
                    // the caller may have omitted the typle constraint.
                    if path.path.segments.len() == 2 {
                        let mut iter = path.path.segments.iter();
                        if let Some(last) = iter.next_back() {
                            if let PathArguments::None = last.arguments {
                                if last.ident == "LEN" || last.ident == "MAX" || last.ident == "MIN"
                                {
                                    if let Some(segment) = iter.next_back() {
                                        state.suspicious_ident = Some(segment.ident.clone());
                                    }
                                }
                            }
                        }
                    }
                }
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
                for expr in &mut tuple.elems {
                    self.replace_expr(expr, state)?;
                }
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

    fn replace_fields(&self, fields: &mut Fields) -> Result<()> {
        match fields {
            Fields::Named(FieldsNamed { named: fields, .. })
            | Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            }) => {
                for field in fields {
                    self.replace_type(&mut field.ty)?;
                }
            }
            Fields::Unit => {}
        }
        Ok(())
    }

    pub fn replace_generics(&self, generics: &mut Generics) -> Result<()> {
        if let Some(where_clause) = &mut generics.where_clause {
            for mut predicate in std::mem::take(&mut where_clause.predicates) {
                if let WherePredicate::Type(predicate_type) = &mut predicate {
                    match &mut predicate_type.bounded_ty {
                        Type::Path(type_path) => {
                            if let Some(first) = first_path_segment_mut(type_path) {
                                if let Some(Typle::Generic(component_names)) =
                                    self.typles.get(&first.ident)
                                {
                                    // T<{..}>: Copy, T<_>::Output: Copy
                                    // T<0>: Copy gets replaced in the fallback
                                    let PathArguments::AngleBracketed(arguments) = &first.arguments
                                    else {
                                        abort!(first, "expected angle brackets");
                                    };
                                    let mut iter = arguments.args.iter();
                                    let (Some(arg), None) = (iter.next(), iter.next()) else {
                                        abort!(arguments, "expected constant expression");
                                    };
                                    let mut expr = match arg {
                                        GenericArgument::Type(Type::Infer(_)) => {
                                            let typle_len =
                                                self.typle_len.unwrap_or(self.typle_macro.max_len);
                                            parse_quote!(.. #typle_len)
                                        }
                                        GenericArgument::Const(expr) => expr.clone(),
                                        _ => {
                                            abort!(arg, "expected const expression or `_`");
                                        }
                                    };
                                    let mut state = BlockState::default();
                                    self.replace_expr(&mut expr, &mut state)?;
                                    if let Some(range) = evaluate_range(&expr) {
                                        let start = range
                                            .start
                                            .as_deref()
                                            .map(|start| {
                                                evaluate_usize(start).ok_or_else(|| {
                                                    Error::new(start.span(), "expected integer")
                                                })
                                            })
                                            .transpose()?
                                            .unwrap_or(0);
                                        let end = range
                                            .end
                                            .as_deref()
                                            .map(|end| {
                                                evaluate_usize(end).ok_or_else(|| {
                                                    Error::new(start.span(), "expected integer")
                                                })
                                            })
                                            .transpose()?
                                            .unwrap_or(self.typle_macro.max_len);
                                        let end = match range.limits {
                                            RangeLimits::HalfOpen(_) => end,
                                            RangeLimits::Closed(_) => end.saturating_add(1),
                                        };
                                        for bound in &mut predicate_type.bounds {
                                            if let TypeParamBound::Trait(trait_bound) = bound {
                                                self.replace_path_arguments(&mut trait_bound.path)?;
                                            }
                                        }
                                        for component_name in &component_names[start..end] {
                                            let mut type_path = type_path.clone();
                                            let first =
                                                first_path_segment_mut(&mut type_path).unwrap();
                                            let component_ident =
                                                Ident::new(component_name, first.ident.span());
                                            first.ident = component_ident;
                                            first.arguments = PathArguments::None;
                                            where_clause.predicates.push(WherePredicate::Type(
                                                PredicateType {
                                                    lifetimes: None,
                                                    bounded_ty: Type::Path(type_path),
                                                    colon_token: token::Colon::default(),
                                                    bounds: predicate_type.bounds.clone(),
                                                },
                                            ));
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                        Type::Macro(TypeMacro { mac }) => {
                            if let Some(ident) = mac.path.get_ident() {
                                if ident == "typle_bound" {
                                    let token_stream = std::mem::take(&mut mac.tokens);
                                    let default_span = token_stream.span();
                                    let mut tokens = token_stream.into_iter();
                                    let (pattern, range) =
                                        self.parse_pattern_range(&mut tokens, default_span)?;
                                    let token_stream = tokens.collect::<TokenStream>();
                                    let body_span = token_stream.span();
                                    let Ok(r#type) = parse2::<Type>(token_stream) else {
                                        return Err(Error::new(body_span, "expected type"));
                                    };
                                    for index in range {
                                        let mut context = self.clone();
                                        if let Some(ident) = pattern.clone() {
                                            context.constants.insert(ident, index);
                                        }
                                        let mut bounded_ty = r#type.clone();
                                        context.replace_type(&mut bounded_ty)?;
                                        let bounds = predicate_type
                                            .bounds
                                            .iter()
                                            .map(|bound| {
                                                let mut bound = bound.clone();
                                                if let TypeParamBound::Trait(trait_bound) =
                                                    &mut bound
                                                {
                                                    context.replace_path_arguments(
                                                        &mut trait_bound.path,
                                                    )?;
                                                }
                                                Ok(bound)
                                            })
                                            .collect::<Result<_>>()?;
                                        where_clause.predicates.push(WherePredicate::Type(
                                            PredicateType {
                                                lifetimes: None,
                                                bounded_ty,
                                                colon_token: token::Colon::default(),
                                                bounds,
                                            },
                                        ));
                                    }
                                    continue;
                                }
                            }
                        }
                        _ => {}
                    }
                    self.replace_type(&mut predicate_type.bounded_ty)?;
                    for bound in &mut predicate_type.bounds {
                        // substitute any appearances of typles in the constraints
                        // (e.g. T<_>: Extract<Output = S<0>::Output>)
                        if let TypeParamBound::Trait(trait_bound) = bound {
                            self.replace_path_arguments(&mut trait_bound.path)?;
                        }
                    }
                }
                where_clause.predicates.push(predicate);
            }
        }
        for generic_param in std::mem::take(&mut generics.params) {
            match generic_param {
                GenericParam::Type(type_param) => match self.typles.get(&type_param.ident) {
                    Some(Typle::Generic(component_names)) => {
                        let typle_len = self.typle_len.unwrap_or(self.typle_macro.max_len);
                        for component_name in &component_names[..typle_len] {
                            let mut param = type_param.clone();
                            param.ident = Ident::new(component_name, type_param.ident.span());
                            generics.params.push(GenericParam::Type(param));
                        }
                    }
                    Some(Typle::Specific(_)) => {
                        // remove specific types from parameter list
                    }
                    None => {
                        generics.params.push(GenericParam::Type(type_param));
                    }
                },
                p => {
                    generics.params.push(p);
                }
            }
        }
        Ok(())
    }

    fn replace_generic_arguments(
        &self,
        args: &mut Punctuated<GenericArgument, token::Comma>,
    ) -> Result<()> {
        for arg in std::mem::take(args) {
            match arg {
                GenericArgument::Type(Type::Path(TypePath { qself, mut path }))
                    if qself.is_none() && path.leading_colon.is_none() =>
                {
                    let mut segments = path.segments.iter_mut();
                    if let Some(first) = segments.next() {
                        if let (Some(typle), PathArguments::AngleBracketed(arguments), None) = (
                            self.typles.get(&first.ident),
                            &mut first.arguments,
                            segments.next(),
                        ) {
                            let mut iter = arguments.args.iter_mut();
                            if let (Some(GenericArgument::Const(ref mut expr)), None) =
                                (iter.next(), iter.next())
                            {
                                let mut state = BlockState::default();
                                self.replace_expr(expr, &mut state)?;
                                if let Some(range) = evaluate_range(expr) {
                                    // T<{..}>
                                    let start = range
                                        .start
                                        .as_deref()
                                        .map(|start| {
                                            evaluate_usize(start).ok_or_else(|| {
                                                Error::new(start.span(), "expected integer")
                                            })
                                        })
                                        .transpose()?
                                        .unwrap_or(0);
                                    let end = range
                                        .end
                                        .as_deref()
                                        .map(|end| {
                                            evaluate_usize(end).ok_or_else(|| {
                                                Error::new(end.span(), "expected integer")
                                            })
                                        })
                                        .transpose()?
                                        .unwrap_or(self.typle_macro.max_len);
                                    let end = match range.limits {
                                        RangeLimits::HalfOpen(_) => end,
                                        RangeLimits::Closed(_) => end.saturating_add(1),
                                    };
                                    for i in start..end {
                                        args.push(GenericArgument::Type(self.get_type(
                                            typle,
                                            i,
                                            path.span(),
                                        )?));
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    let mut generic_type = Type::Path(TypePath { qself, path });
                    self.replace_type(&mut generic_type)?;
                    args.push(GenericArgument::Type(generic_type));
                }
                GenericArgument::Type(mut generic_type) => {
                    self.replace_type(&mut generic_type)?;
                    args.push(GenericArgument::Type(generic_type));
                }
                GenericArgument::Const(mut generic_expr) => {
                    let mut state = BlockState::default();
                    self.replace_expr(&mut generic_expr, &mut state)?;
                    args.push(GenericArgument::Const(generic_expr));
                }
                GenericArgument::AssocType(mut assoc_type) => {
                    if let Some(generic_args) = &mut assoc_type.generics {
                        self.replace_generic_arguments(&mut generic_args.args)?;
                    }
                    self.replace_type(&mut assoc_type.ty)?;
                    args.push(GenericArgument::AssocType(assoc_type));
                }
                GenericArgument::AssocConst(mut assoc_expr) => {
                    if let Some(generic_args) = &mut assoc_expr.generics {
                        self.replace_generic_arguments(&mut generic_args.args)?;
                    }
                    let mut state = BlockState::default();
                    self.replace_expr(&mut assoc_expr.value, &mut state)?;
                    args.push(GenericArgument::AssocConst(assoc_expr));
                }
                p => {
                    args.push(p);
                }
            }
        }
        Ok(())
    }

    pub fn replace_fn(
        &mut self,
        function: GeneralFunction,
        _receiver_type: Option<&Type>,
        items: &mut Vec<Item>,
    ) -> Result<GeneralFunction> {
        let fn_name = &function.sig.ident;
        let fn_meta = &function.attrs;
        let fn_vis = &function.vis;
        let fn_type_params = &function.sig.generics.params;
        let fn_type_params_no_constraints = remove_constraints(fn_type_params);
        let fn_input_params = &function.sig.inputs;
        let mut type_tuple = syn::TypeTuple {
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let mut pat_tuple = syn::PatTuple {
            attrs: Vec::new(),
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let mut value_tuple = syn::ExprTuple {
            attrs: Vec::new(),
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let fn_body = function.block;
        for arg in fn_input_params {
            match arg {
                syn::FnArg::Receiver(r) => {
                    abort!(r, "did not expect receiver here");
                }
                syn::FnArg::Typed(pat_type) => {
                    type_tuple.elems.push(pat_type.ty.as_ref().clone());
                    pat_tuple.elems.push(pat_type.pat.as_ref().clone());
                    value_tuple
                        .elems
                        .push(pat_to_expr(pat_type.pat.as_ref().clone())?);
                }
            }
        }
        items.reserve(self.typle_macro.max_len - self.typle_macro.min_len + 3);
        // A trait with an apply method to implement for each tuple
        let trait_name = format_ident!("_typle_fn_{}", fn_name);
        let trait_item = parse_quote!(
            #[allow(non_camel_case_types)]
            #fn_vis trait #trait_name {
                type Return;

                fn apply(self) -> Self::Return;
            }
        );
        items.push(trait_item);
        // A function that turns the function argument list into a tuple that
        // implements the trait and calls the apply method from the trait.
        let fn_item = GeneralFunction {
            attrs: Vec::new(),
            vis: fn_vis.clone(),
            sig: parse_quote!(
            fn #fn_name <#fn_type_params_no_constraints>(#fn_input_params) -> <#type_tuple as #trait_name>::Return
            where
                #type_tuple: #trait_name
            ),
            block: parse_quote!({
                <#type_tuple as #trait_name>::apply(#value_tuple)
            }),
        };
        let return_type = match function.sig.output {
            syn::ReturnType::Default => Type::Tuple(TypeTuple {
                paren_token: token::Paren::default(),
                elems: Punctuated::new(),
            }),
            syn::ReturnType::Type(_, t) => *t,
        };
        let typle_trait_name = &self.typle_macro.ident;
        let impl_items = vec![
            syn::ImplItem::Type(syn::ImplItemType {
                attrs: Vec::new(),
                vis: syn::Visibility::Inherited,
                defaultness: None,
                type_token: token::Type::default(),
                ident: Ident::new("Return", Span::call_site()),
                generics: Generics::default(),
                eq_token: token::Eq::default(),
                ty: return_type,
                semi_token: token::Semi::default(),
            }),
            parse_quote!(
                #(#fn_meta)*
                fn apply(self) -> Self::Return {
                    #[typle_attr_if(#typle_trait_name::LEN == 0, allow(unused_variables))]
                    let #pat_tuple = self;
                    #fn_body
                }
            ),
        ];

        let item = ItemImpl {
            attrs: Vec::new(),
            defaultness: None,
            unsafety: None,
            impl_token: token::Impl::default(),
            generics: function.sig.generics,
            trait_: Some((None, ident_to_path(trait_name), token::For::default())),
            self_ty: Box::new(syn::Type::Tuple(type_tuple)),
            brace_token: token::Brace::default(),
            items: impl_items,
        };

        for typle_len in self.typle_macro.min_len..self.typle_macro.max_len {
            self.typle_len = Some(typle_len);
            self.replace_item_impl(item.clone(), items)?;
        }
        self.typle_len = Some(self.typle_macro.max_len);
        self.replace_item_impl(item, items)?;
        Ok(fn_item)
    }

    pub fn replace_item(&self, item: Item, items: &mut Vec<Item>) -> Result<()> {
        match item {
            Item::Const(mut constant) => {
                let context = self.extract_typle_constraints(&mut constant.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut constant.attrs)?;
                context.replace_type(&mut constant.ty)?;
                let mut state = BlockState::default();
                context.replace_expr(&mut constant.expr, &mut state)?;
                items.push(Item::Const(constant));
            }
            Item::Enum(mut enum_item) => {
                let context = self.extract_typle_constraints(&mut enum_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut enum_item.attrs)?;
                context.replace_generics(&mut enum_item.generics)?;
                for mut variant in std::mem::take(&mut enum_item.variants) {
                    if let Fields::Unit = variant.fields {
                        if let Some((_, Expr::Macro(r#macro))) = &mut variant.discriminant {
                            if let Some(ident) = r#macro.mac.path.get_ident() {
                                if ident == "typle_variant" {
                                    let token_stream = std::mem::take(&mut r#macro.mac.tokens);
                                    let default_span = token_stream.span();
                                    let mut tokens = token_stream.into_iter();
                                    let (pattern, range) =
                                        context.parse_pattern_range(&mut tokens, default_span)?;
                                    let token_stream = tokens.collect();
                                    let fields = match r#macro.mac.delimiter {
                                        MacroDelimiter::Paren(_) => {
                                            let group = TokenTree::Group(Group::new(
                                                proc_macro2::Delimiter::Parenthesis,
                                                token_stream,
                                            ));
                                            Fields::Unnamed(parse2::<FieldsUnnamed>(
                                                TokenStream::from(group),
                                            )?)
                                        }
                                        MacroDelimiter::Brace(_) => {
                                            let group = TokenTree::Group(Group::new(
                                                proc_macro2::Delimiter::Brace,
                                                token_stream,
                                            ));
                                            Fields::Named(parse2::<FieldsNamed>(
                                                TokenStream::from(group),
                                            )?)
                                        }
                                        MacroDelimiter::Bracket(_) => {
                                            if !token_stream.is_empty() {
                                                abort!(token_stream, "braces require empty body");
                                            }
                                            Fields::Unit
                                        }
                                    };
                                    let mut context = context.clone();
                                    if let Some(ident) = &pattern {
                                        context.constants.insert(ident.clone(), 0);
                                    }
                                    for index in range {
                                        if let Some(ident) = &pattern {
                                            *context.constants.get_mut(ident).unwrap() = index;
                                        }
                                        let mut fields = fields.clone();
                                        match &mut fields {
                                            Fields::Named(FieldsNamed {
                                                named: fields, ..
                                            })
                                            | Fields::Unnamed(FieldsUnnamed {
                                                unnamed: fields,
                                                ..
                                            }) => {
                                                for field in fields {
                                                    context.replace_type(&mut field.ty)?;
                                                }
                                            }
                                            Fields::Unit => {}
                                        }
                                        let variant = Variant {
                                            attrs: variant.attrs.clone(),
                                            ident: format_ident!("{}{}", &variant.ident, index),
                                            fields,
                                            discriminant: None,
                                        };
                                        enum_item.variants.push(variant);
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    context.replace_fields(&mut variant.fields)?;
                    if let Some((_, discriminant)) = &mut variant.discriminant {
                        let mut state = BlockState::default();
                        context.replace_expr(discriminant, &mut state)?;
                    }
                    enum_item.variants.push(variant);
                }
                items.push(Item::Enum(enum_item));
            }
            Item::Fn(mut function) => {
                if self.typle_len.is_none() {
                    if let Some(mut context) =
                        self.extract_typle_constraints(&mut function.sig.generics)?
                    {
                        let fn_item = context.replace_fn(function.into(), None, items)?;
                        items.push(Item::Fn(fn_item.into()));
                        return Ok(());
                    }
                }
                let context = self.extract_typle_constraints(&mut function.sig.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut function.attrs)?;
                context.replace_signature(&mut function.sig)?;
                let mut state = BlockState::default();
                context.replace_block(&mut function.block, &mut state)?;
                items.push(Item::Fn(function));
            }
            Item::Impl(mut item) => {
                if self.typle_len.is_none() {
                    if let Some(mut context) = self.extract_typle_constraints(&mut item.generics)? {
                        items.reserve(self.typle_macro.max_len - self.typle_macro.min_len + 1);
                        for typle_len in self.typle_macro.min_len..self.typle_macro.max_len {
                            context.typle_len = Some(typle_len);
                            context.replace_item_impl(item.clone(), items)?;
                        }
                        context.typle_len = Some(self.typle_macro.max_len);
                        context.replace_item_impl(item, items)?;
                        return Ok(());
                    }
                }
                let context = self.extract_typle_constraints(&mut item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_item_impl(item, items)?;
            }
            Item::Macro(mut item) => {
                self.replace_attrs(&mut item.attrs)?;
                item.mac.tokens =
                    self.replace_macro_token_stream(std::mem::take(&mut item.mac.tokens))?;
                items.push(Item::Macro(item));
            }
            Item::Mod(mut module) => {
                if let Some((_, inner_items)) = &mut module.content {
                    for item in
                        std::mem::replace(inner_items, Vec::with_capacity(inner_items.len()))
                    {
                        self.replace_item(item, inner_items)?;
                    }
                }
                items.push(Item::Mod(module));
            }
            Item::Struct(mut struct_item) => {
                let context = self.extract_typle_constraints(&mut struct_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut struct_item.attrs)?;
                context.replace_generics(&mut struct_item.generics)?;
                context.replace_fields(&mut struct_item.fields)?;
                items.push(Item::Struct(struct_item));
            }
            Item::Type(mut type_item) => {
                let context = self.extract_typle_constraints(&mut type_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut type_item.attrs)?;
                context.replace_generics(&mut type_item.generics)?;
                context.replace_type(&mut type_item.ty)?;
                items.push(Item::Type(type_item));
            }
            item => {
                items.push(item);
            }
        }
        Ok(())
    }

    fn replace_item_impl(&self, mut item: ItemImpl, items: &mut Vec<Item>) -> Result<()> {
        self.replace_attrs(&mut item.attrs)?;
        self.replace_generics(&mut item.generics)?;
        if let Some((_, path, _)) = &mut item.trait_ {
            self.replace_path_arguments(path)?;
        }
        self.replace_type(&mut item.self_ty)?;
        for subitem in &mut item.items {
            match subitem {
                ImplItem::Const(constant) => {
                    self.replace_attrs(&mut constant.attrs)?;
                    self.replace_type(&mut constant.ty)?;
                    let mut state = BlockState::default();
                    self.replace_expr(&mut constant.expr, &mut state)?;
                }
                ImplItem::Fn(function) => {
                    if self.typle_len.is_none() {
                        if let Some(mut context) =
                            self.extract_typle_constraints(&mut function.sig.generics)?
                        {
                            *function = context
                                .replace_fn(function.clone().into(), Some(&item.self_ty), items)?
                                .into();
                            continue;
                        }
                    }
                    self.replace_attrs(&mut function.attrs)?;
                    let inner_context =
                        self.extract_typle_constraints(&mut function.sig.generics)?;
                    let inner_context = inner_context.as_ref().unwrap_or(self);
                    inner_context.replace_signature(&mut function.sig)?;
                    let mut state = BlockState::default();
                    inner_context.replace_block(&mut function.block, &mut state)?;
                }
                ImplItem::Type(ty) => {
                    self.replace_attrs(&mut ty.attrs)?;
                    self.replace_type(&mut ty.ty)?;
                }
                ImplItem::Macro(subitem) => {
                    self.replace_attrs(&mut subitem.attrs)?;
                    subitem.mac.tokens =
                        self.replace_macro_token_stream(std::mem::take(&mut subitem.mac.tokens))?;
                }
                _ => {}
            }
        }
        items.push(Item::Impl(item));
        Ok(())
    }

    fn replace_typle_for_expr(
        &'a self,
        mac: &mut Macro,
        state: &'a mut BlockState,
        const_if: bool,
    ) -> Result<impl Iterator<Item = Result<Expr>> + 'a> {
        let token_stream = std::mem::take(&mut mac.tokens);
        let default_span = token_stream.span();
        let mut tokens = token_stream.into_iter();
        let (pattern, range) = self.parse_pattern_range(&mut tokens, default_span)?;
        let expr = parse2::<Expr>(tokens.collect())?;
        let mut context = self.clone();
        context.const_if = const_if;
        if let Some(ident) = &pattern {
            context.constants.insert(ident.clone(), 0);
        }
        Ok(range.flat_map(move |index| {
            if let Some(ident) = &pattern {
                *context.constants.get_mut(ident).unwrap() = index;
            }
            let mut expr = expr.clone();
            match context.replace_expr(&mut expr, state) {
                Ok(()) => {
                    if let Expr::Verbatim(_token_stream) = expr {
                        // Verbatim causes typle to omit the component from the tuple
                        None
                    } else {
                        Some(Ok(expr))
                    }
                }
                Err(error) => Some(Err(error)),
            }
        }))
    }

    fn replace_typle_fold(
        &self,
        mac: &mut Macro,
        attrs: Vec<Attribute>,
        state: &mut BlockState,
        default_span: Span,
    ) -> Result<Expr> {
        let mut inner_state = BlockState::default();
        let token_stream = std::mem::take(&mut mac.tokens);
        let mut tokens = token_stream.into_iter();
        let mut init_expr = Self::parse_init_expr(&mut tokens, default_span)?;
        self.replace_expr(&mut init_expr, &mut inner_state)?;
        let (pattern, mut range) = self.parse_pattern_range(&mut tokens, default_span)?;
        let fold_ident = Self::parse_fold_ident(&mut tokens, default_span)?;
        let fold_pat = Pat::Ident(syn::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: fold_ident.clone(),
            subpat: None,
        });
        let expr = parse2::<Expr>(tokens.collect())?;
        let mut stmts = Vec::with_capacity(range.len() + 2);
        stmts.push(Stmt::Local(syn::Local {
            attrs: Vec::new(),
            let_token: token::Let::default(),
            pat: fold_pat.clone(),
            init: Some(syn::LocalInit {
                eq_token: token::Eq::default(),
                expr: Box::new(init_expr),
                diverge: None,
            }),
            semi_token: token::Semi::default(),
        }));
        let mut ctx = pattern.as_ref().map(|ident| {
            let mut context = self.clone();
            context.constants.insert(ident.clone(), 0);
            (context, ident)
        });
        let last = range.next_back();
        for index in range {
            if let Some((ref mut context, ident)) = ctx {
                *context.constants.get_mut(ident).unwrap() = index;
            }
            let context = ctx.as_ref().map_or(self, |c| &c.0);
            let mut expr = expr.clone();
            context.replace_expr(&mut expr, &mut inner_state)?;
            stmts.push(Stmt::Local(syn::Local {
                attrs: Vec::new(),
                let_token: token::Let::default(),
                pat: fold_pat.clone(),
                init: Some(syn::LocalInit {
                    eq_token: token::Eq::default(),
                    expr: Box::new(expr),
                    diverge: None,
                }),
                semi_token: token::Semi::default(),
            }));
        }
        if let Some(index) = last {
            if let Some((context, ident)) = &mut ctx {
                *context.constants.get_mut(ident).unwrap() = index;
            }
            let context = ctx.as_ref().map_or(self, |c| &c.0);
            let mut expr = expr;
            context.replace_expr(&mut expr, &mut inner_state)?;
            stmts.push(Stmt::Local(syn::Local {
                attrs: Vec::new(),
                let_token: token::Let::default(),
                pat: fold_pat,
                init: Some(syn::LocalInit {
                    eq_token: token::Eq::default(),
                    expr: Box::new(expr),
                    diverge: None,
                }),
                semi_token: token::Semi::default(),
            }));
        }
        if let Some(span) = inner_state.unlabelled_continue {
            abort!(
                span,
                "unlabelled `continue` not supported in `typle_fold!` macro"
            );
        }
        state.propagate(inner_state, None);
        stmts.push(Stmt::Expr(
            Expr::Break(syn::ExprBreak {
                attrs: Vec::new(),
                break_token: token::Break::default(),
                label: None,
                expr: Some(Box::new(Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: ident_to_path(fold_ident),
                }))),
            }),
            Some(token::Semi::default()),
        ));
        Ok(Expr::Loop(syn::ExprLoop {
            attrs,
            label: None,
            loop_token: token::Loop::default(),
            body: Block {
                brace_token: token::Brace::default(),
                stmts,
            },
        }))
    }

    fn replace_macro_expr(
        &self,
        mac: &mut Macro,
        attrs: &mut Vec<Attribute>,
        state: &mut BlockState,
    ) -> Result<Option<Expr>> {
        // typle_for!(i in .. => Some<t[[i]]) -> (Some<t.0>, Some<t.1>)
        // typle_for!(i in .. => t.to_string()) -> (t.0.to_string(), t.1.to_string())
        // as opposed to
        // Some(t) -> Some((t.0, t.1))
        // t.to_string() -> (t.0, t.1).to_string()
        self.replace_attrs(attrs)?;
        if let Some(macro_name) = mac.path.get_ident() {
            let default_span = macro_name.span();
            if macro_name == "typle_for" {
                let expr = match &mac.delimiter {
                    MacroDelimiter::Paren(_) => {
                        let elems = self.replace_typle_for_expr(mac, state, false)?;
                        let tuple = ExprTuple {
                            attrs: std::mem::take(attrs),
                            paren_token: token::Paren::default(),
                            elems: elems.collect::<Result<_>>()?,
                        };
                        Expr::Tuple(tuple)
                    }
                    MacroDelimiter::Brace(_) => {
                        let elems = self.replace_typle_for_expr(mac, state, true)?;
                        let tuple = ExprTuple {
                            attrs: std::mem::take(attrs),
                            paren_token: token::Paren::default(),
                            elems: elems.collect::<Result<_>>()?,
                        };
                        Expr::Tuple(tuple)
                    }
                    MacroDelimiter::Bracket(_) => {
                        let elems = self.replace_typle_for_expr(mac, state, false)?;
                        let array = ExprArray {
                            attrs: std::mem::take(attrs),
                            bracket_token: token::Bracket::default(),
                            elems: elems.collect::<Result<_>>()?,
                        };
                        Expr::Array(array)
                    }
                };
                return Ok(Some(expr));
            } else if macro_name == "typle_fold" {
                let expr =
                    self.replace_typle_fold(mac, std::mem::take(attrs), state, default_span)?;
                return Ok(Some(expr));
            } else if macro_name == "typle_all" {
                let token_stream = std::mem::take(&mut mac.tokens);
                let expr = self.anyall(
                    token_stream,
                    default_span,
                    state,
                    syn::BinOp::And(token::AndAnd::default()),
                    true,
                )?;
                return Ok(Some(expr));
            } else if macro_name == "typle_any" {
                let token_stream = std::mem::take(&mut mac.tokens);
                let expr = self.anyall(
                    token_stream,
                    default_span,
                    state,
                    syn::BinOp::Or(token::OrOr::default()),
                    false,
                )?;
                return Ok(Some(expr));
            }
        }
        mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut mac.tokens))?;
        Ok(None)
    }

    fn anyall(
        &self,
        token_stream: TokenStream,
        default_span: Span,
        state: &mut BlockState,
        op: BinOp,
        default: bool,
    ) -> Result<Expr> {
        let mut tokens = token_stream.into_iter();
        let (pattern, mut range) = self.parse_pattern_range(&mut tokens, default_span)?;
        let expr = parse2::<Expr>(tokens.collect())?;
        // Parenthesize low precedence expressions.
        let expr = match expr {
            expr @ Expr::Lit(ExprLit {
                lit: Lit::Bool(_), ..
            }) => expr,
            expr @ Expr::Block(_) => expr,
            expr @ Expr::Paren(_) => expr,
            expr @ Expr::Path(_) => expr,
            expr @ Expr::MethodCall(_) => expr,
            expr @ Expr::Field(_) => expr,
            expr @ Expr::Call(_) => expr,
            expr @ Expr::Index(_) => expr,
            expr @ Expr::Unary(_) => expr,
            expr @ Expr::Cast(_) => expr,
            expr => Expr::Paren(ExprParen {
                attrs: Vec::new(),
                paren_token: token::Paren::default(),
                expr: Box::new(expr),
            }),
        };
        let all = match range.next() {
            Some(index) => {
                let mut context = self.clone();
                if let Some(ident) = &pattern {
                    context.constants.insert(ident.clone(), index);
                }
                let mut all = expr.clone();
                context.replace_expr(&mut all, state)?;
                for index in range {
                    if let Some(ident) = &pattern {
                        *context.constants.get_mut(ident).unwrap() = index;
                    }
                    let mut expr = expr.clone();
                    context.replace_expr(&mut expr, state)?;
                    all = Expr::Binary(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(all),
                        op,
                        right: Box::new(expr),
                    });
                }
                if let Expr::Binary(expr) = all {
                    // Wrap entire expression in parentheses to:
                    // 1. ensure that it is a single expression in a larger expression.
                    // 2. handle ambiguity: `{ true } || { true }` is not a boolean expression.
                    // It is a block followed by a closure that returns a boolean. Adding
                    // parentheses `({ true } || { true })` makes it a boolean expression.
                    all = Expr::Paren(ExprParen {
                        attrs: Vec::new(),
                        paren_token: token::Paren::default(),
                        expr: Box::new(Expr::Binary(expr)),
                    });
                }
                all
            }
            None => Expr::Lit(ExprLit {
                attrs: Vec::new(),
                lit: Lit::Bool(LitBool {
                    value: default,
                    span: expr.span(),
                }),
            }),
        };
        Ok(all)
    }

    fn replace_macro_pat(&self, m: &mut PatMacro) -> Result<Option<Pat>> {
        self.replace_attrs(&mut m.attrs)?;
        if let Some(macro_name) = m.mac.path.get_ident() {
            if macro_name == "typle_for" {
                let token_stream = std::mem::take(&mut m.mac.tokens);
                let default_span = token_stream.span();
                let mut tokens = token_stream.into_iter();
                let (pattern, range) = self.parse_pattern_range(&mut tokens, default_span)?;
                let token_stream = tokens.collect::<TokenStream>();
                let body_span = token_stream.span();
                let mut tuple = PatTuple {
                    attrs: std::mem::take(&mut m.attrs),
                    paren_token: token::Paren::default(),
                    elems: Punctuated::new(),
                };
                match m.mac.delimiter {
                    MacroDelimiter::Paren(_) => {
                        let pat = Pat::parse_single.parse2(token_stream)?;
                        let mut context = self.clone();
                        if let Some(ident) = &pattern {
                            context.constants.insert(ident.clone(), 0);
                        }
                        for index in range {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            let mut component = pat.clone();
                            context.replace_pat(&mut component)?;
                            tuple.elems.push(component);
                        }
                    }
                    MacroDelimiter::Brace(_) => {
                        let Ok(expr) = parse2::<Expr>(token_stream) else {
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
                        for index in range {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            let mut expr = expr.clone();
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

    fn replace_macro_type(&self, m: &mut TypeMacro) -> Result<Option<Type>> {
        // typle_for!(i in .. => Option<T<{i}>) -> (Option<T0>, Option<T1>)
        // typle_for!(i in .. => T::<{i}>::default()) -> (T0::default(), T1::default())
        // as opposed to
        // Option<T> -> Option<(T0, T1)>
        // T::default() -> <(T0, T1)>::default()
        if let Some(macro_name) = m.mac.path.get_ident() {
            if macro_name == "typle_for" {
                let token_stream = std::mem::take(&mut m.mac.tokens);
                let default_span = token_stream.span();
                let mut tokens = token_stream.into_iter();
                let (pattern, range) = self.parse_pattern_range(&mut tokens, default_span)?;
                let token_stream = tokens.collect::<TokenStream>();
                let body_span = token_stream.span();
                let mut tuple = TypeTuple {
                    paren_token: token::Paren::default(),
                    elems: Punctuated::new(),
                };
                match m.mac.delimiter {
                    MacroDelimiter::Paren(_) => {
                        let ty = parse2::<Type>(token_stream)?;
                        let mut context = self.clone();
                        if let Some(ident) = &pattern {
                            context.constants.insert(ident.clone(), 0);
                        }
                        for index in range {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            let mut component = ty.clone();
                            context.replace_type(&mut component)?;
                            tuple.elems.push(component);
                        }
                    }
                    MacroDelimiter::Brace(_) => {
                        let Ok(expr) = parse2::<Expr>(token_stream) else {
                            return Err(Error::new(
                                body_span,
                                "cannot parse, wrap types in `typle_ty!()`",
                            ));
                        };
                        let mut context = self.clone();
                        context.const_if = true;
                        if let Some(ident) = &pattern {
                            context.constants.insert(ident.clone(), 0);
                        }
                        for index in range {
                            if let Some(ident) = &pattern {
                                *context.constants.get_mut(ident).unwrap() = index;
                            }
                            let mut expr = expr.clone();
                            let mut state = BlockState::default();
                            context.replace_expr(&mut expr, &mut state)?;
                            if let Expr::Verbatim(_) = expr {
                                // Verbatim omits the type from the tuple.
                            } else {
                                let mut ty = expr_to_type(expr)?;
                                context.replace_type(&mut ty)?;
                                tuple.elems.push(ty);
                            }
                        }
                    }
                    MacroDelimiter::Bracket(_) => {
                        abort!(m, "expected parentheses or braces");
                    }
                }
                return Ok(Some(Type::Tuple(tuple)));
            }
        }
        m.mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut m.mac.tokens))?;
        Ok(None)
    }

    // Look for `typle_ty!(...)` or `typle_expr!(...)` and evaluate body.
    fn replace_macro_token_stream(&self, input: TokenStream) -> Result<TokenStream> {
        enum TTState {
            Start,
            FoundIdent(Ident),
            TypleTy,
            TypleExpr,
        }

        let mut output = TokenStream::new();
        let mut state = TTState::Start;
        for tt in input {
            match tt {
                TokenTree::Group(group) => {
                    match state {
                        TTState::Start => {
                            output.extend([TokenTree::Group(Group::new(
                                group.delimiter(),
                                self.replace_macro_token_stream(group.stream())?,
                            ))]);
                        }
                        TTState::FoundIdent(ident) => {
                            output.extend([
                                TokenTree::Ident(ident),
                                TokenTree::Group(Group::new(
                                    group.delimiter(),
                                    self.replace_macro_token_stream(group.stream())?,
                                )),
                            ]);
                        }
                        TTState::TypleTy => {
                            let mut ty = syn::parse2(group.stream())?;
                            self.replace_type(&mut ty)?;
                            output.extend(ty.into_token_stream());
                        }
                        TTState::TypleExpr => {
                            let mut expr = syn::parse2(group.stream())?;
                            let mut state = BlockState::default();
                            self.replace_expr(&mut expr, &mut state)?;
                            output.extend(expr.into_token_stream());
                        }
                    }
                    state = TTState::Start;
                }
                TokenTree::Ident(ident) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(prev) => {
                            output.extend([TokenTree::Ident(prev)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(ident, "typle: expected macro body");
                        }
                    }
                    state = TTState::FoundIdent(ident);
                }
                TokenTree::Punct(punct) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(ident) => {
                            if punct.as_char() == '!' {
                                if ident == "typle_ty" {
                                    state = TTState::TypleTy;
                                    continue;
                                } else if ident == "typle_expr" {
                                    state = TTState::TypleExpr;
                                    continue;
                                }
                            }
                            output.extend([TokenTree::Ident(ident)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(punct, "typle: expected macro body");
                        }
                    }
                    output.extend([TokenTree::Punct(punct)]);
                    state = TTState::Start;
                }
                TokenTree::Literal(literal) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(ident) => {
                            output.extend([TokenTree::Ident(ident)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(literal, "typle: expected macro body");
                        }
                    }
                    output.extend([TokenTree::Literal(literal)]);
                    state = TTState::Start;
                }
            }
        }
        match state {
            TTState::Start => {}
            TTState::FoundIdent(ident) => {
                output.extend([TokenTree::Ident(ident)]);
            }
            TTState::TypleTy | TTState::TypleExpr => {
                return Err(Error::new(Span::call_site(), "typle: expected macro body"));
            }
        }
        Ok(output)
    }

    fn parse_init_expr(tokens: &mut impl Iterator<Item = TokenTree>, span: Span) -> Result<Expr> {
        let mut collect = TokenStream::new();
        for token in tokens.by_ref() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == ';' => {
                    return parse2::<Expr>(collect);
                }
                tt => {
                    collect.extend([tt]);
                }
            }
        }
        Err(Error::new(
            span,
            "typle expected init expression terminated by ;",
        ))
    }

    fn parse_fold_ident(tokens: &mut impl Iterator<Item = TokenTree>, span: Span) -> Result<Ident> {
        let Some(TokenTree::Punct(punct)) = tokens.next() else {
            return Err(Error::new(span, "expected `=> |accumulator| expression`"));
        };
        if punct.as_char() != '|' {
            return Err(Error::new(span, "expected `=> |accumulator| expression`"));
        }
        let Some(TokenTree::Ident(mut ident)) = tokens.next() else {
            return Err(Error::new(span, "expected `=> |accumulator| expression`"));
        };
        let Some(TokenTree::Punct(punct)) = tokens.next() else {
            return Err(Error::new(span, "expected `=> |accumulator| expression`"));
        };
        if punct.as_char() != '|' {
            return Err(Error::new(span, "expected `=> |accumulator| expression`"));
        }
        if ident == "_" {
            ident = Ident::new("_typle", ident.span());
        }
        Ok(ident)
    }

    fn parse_pattern_range(
        &self,
        tokens: &mut impl Iterator<Item = TokenTree>,
        span: Span,
    ) -> Result<(Option<Ident>, Range<usize>)> {
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
            return Err(Error::new(span, "expected range"));
        }
        let mut expr = parse2::<Expr>(collect)?;
        let mut state = BlockState::default();
        self.replace_expr(&mut expr, &mut state)?;
        if let Expr::Range(range) = expr {
            let start = range
                .start
                .as_ref()
                .map(|expr| {
                    evaluate_usize(expr).ok_or_else(|| {
                        if let Some(suspicious_ident) = &state.suspicious_ident {
                            Error::new(
                                suspicious_ident.span(),
                                format!(
                                    "range start invalid, possibly missing `{}: {}` bound",
                                    suspicious_ident, self.typle_macro.ident
                                ),
                            )
                        } else {
                            Error::new(expr.span(), "range start invalid")
                        }
                    })
                })
                .transpose()?
                .unwrap_or(0);
            let end = range
                .end
                .as_ref()
                .map(|expr| {
                    evaluate_usize(expr).ok_or_else(|| {
                        if let Some(suspicious_ident) = &state.suspicious_ident {
                            Error::new(
                                suspicious_ident.span(),
                                format!(
                                    "range end invalid, possibly missing `{}: {}` bound",
                                    suspicious_ident, self.typle_macro.ident
                                ),
                            )
                        } else {
                            Error::new(range.end.span(), "range end invalid")
                        }
                    })
                })
                .transpose()?
                .unwrap_or(self.typle_macro.max_len)
                .checked_add(match range.limits {
                    RangeLimits::HalfOpen(_) => 0,
                    RangeLimits::Closed(_) => 1,
                })
                .ok_or_else(|| {
                    Error::new(
                        range.span(),
                        format!(
                            "for length {} range contains no values",
                            self.typle_len.unwrap_or(self.typle_macro.max_len)
                        ),
                    )
                })?;
            Ok((pattern, start..end))
        } else {
            Err(Error::new(expr.span(), "expected range"))
        }
    }

    fn replace_pat(&self, pat: &mut Pat) -> Result<()> {
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
                self.replace_pat(&mut paren.pat)?;
            }
            Pat::Path(path) => {
                // State::S::<typle_ident!(i)> -> State::S2
                if let Some(qself) = &mut path.qself {
                    self.replace_type(&mut qself.ty)?;
                }
                for mut path_segment in std::mem::take(&mut path.path.segments) {
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
                    path.path.segments.push(path_segment);
                }
            }
            Pat::Reference(reference) => {
                self.replace_pat(&mut reference.pat)?;
            }
            Pat::Slice(slice) => {
                for pat in &mut slice.elems {
                    self.replace_pat(pat)?;
                }
            }
            Pat::Tuple(tuple) => {
                for pat in &mut tuple.elems {
                    self.replace_pat(pat)?;
                }
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

    fn replace_path_arguments(&self, path: &mut Path) -> Result<()> {
        for mut path_segment in std::mem::take(&mut path.segments) {
            match &mut path_segment.arguments {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    if let Some(index) = self.typle_ident(args)? {
                        // X::<typle_ident!(3)> -> X3
                        path_segment.ident = format_ident!("{}{}", path_segment.ident, index);
                        path_segment.arguments = PathArguments::None;
                    } else {
                        // WrapperType<T> -> WrapperType<(T0, T1,...)>
                        // WrapperType<T<3>> -> WrapperType<T3>
                        // WrapperType<T<{..}>> -> WrapperType<T0, T1,...>
                        self.replace_generic_arguments(&mut args.args)?;
                        if args.args.is_empty() {
                            path_segment.arguments = PathArguments::None;
                        }
                    }
                }
                PathArguments::Parenthesized(args) => {
                    for input in &mut args.inputs {
                        self.replace_type(input)?;
                    }
                    if let ReturnType::Type(_, ty) = &mut args.output {
                        self.replace_type(ty)?;
                    }
                }
            }
            path.segments.push(path_segment);
        }
        Ok(())
    }

    fn replace_signature(&self, sig: &mut syn::Signature) -> Result<()> {
        self.replace_generics(&mut sig.generics)?;
        for input in &mut sig.inputs {
            if let syn::FnArg::Typed(pat_type) = input {
                self.replace_type(&mut pat_type.ty)?;
            }
        }
        match &mut sig.output {
            syn::ReturnType::Default => {}
            syn::ReturnType::Type(_, ty) => {
                self.replace_type(ty)?;
            }
        }
        Ok(())
    }

    // Replace `T`` with `(T0, T1,...)`` and `T<1>`` with `T1``
    fn replace_type(&self, ty: &mut Type) -> Result<()> {
        match ty {
            Type::Array(array) => {
                self.replace_type(&mut array.elem)?;
                let mut state = BlockState::default();
                self.replace_expr(&mut array.len, &mut state)?;
            }
            Type::BareFn(bare_fn) => {
                for arg in &mut bare_fn.inputs {
                    self.replace_type(&mut arg.ty)?;
                }
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
                self.replace_type(&mut paren.elem)?;
            }
            Type::Path(path) => {
                match &mut path.qself {
                    Some(qself) => {
                        // <T<0> as Mul<M>>::Output
                        self.replace_type(&mut qself.ty)?;
                    }
                    None => {
                        let mut segments = std::mem::take(&mut path.path.segments)
                            .into_iter()
                            .peekable();
                        if let Some(mut first) = segments.next() {
                            if let Some(typle) = self.typles.get(&first.ident) {
                                match &mut first.arguments {
                                    PathArguments::None => {
                                        // T -> (T0, T1,...)
                                        let span = first.ident.span();
                                        *ty = Type::Tuple(TypeTuple {
                                            paren_token: token::Paren::default(),
                                            elems: (0..self
                                                .typle_len
                                                .unwrap_or(self.typle_macro.max_len))
                                                .map(|i| self.get_type(typle, i, span))
                                                .collect::<Result<_>>()?,
                                        });
                                        return Ok(());
                                    }
                                    PathArguments::AngleBracketed(args) => {
                                        // T<3> or T<{i}>
                                        if args.args.len() != 1 {
                                            return Err(Error::new(
                                                first.span(),
                                                "expected one type parameter",
                                            ));
                                        }
                                        match args.args.first_mut() {
                                            Some(GenericArgument::Const(expr)) => {
                                                // T<{T::LEN - 1}>
                                                let mut state = BlockState::default();
                                                self.replace_expr(expr, &mut state)?;
                                                // T<{5 - 1}>
                                                let Some(value) = evaluate_usize(expr) else {
                                                    return Err(Error::new(
                                                        expr.span(),
                                                        "unsupported tuple type index"
                                                    ));
                                                };
                                                let component_type =
                                                    self.get_type(typle, value, first.span())?;
                                                if segments.peek().is_some() {
                                                    // T<3>::Output -> <T3>::Output
                                                    path.qself = Some(QSelf {
                                                        lt_token: token::Lt::default(),
                                                        ty: Box::new(component_type),
                                                        position: 0,
                                                        as_token: None,
                                                        gt_token: token::Gt::default(),
                                                    });
                                                    path.path.leading_colon =
                                                        Some(token::PathSep::default());
                                                    path.path.segments = segments.collect();
                                                } else {
                                                    // T<3> -> T3
                                                    *ty = component_type;
                                                    return Ok(());
                                                }
                                            }
                                            _ => {
                                                return Err(Error::new(
                                                    args.span(),
                                                    "Require const parameter (wrap {} around expression)"
                                                ))
                                            }
                                        }
                                    }
                                    PathArguments::Parenthesized(_) => {
                                        // Parenthesized arguments are not supported for tuple types
                                        return Err(Error::new(
                                            first.arguments.span(),
                                            "unexpected patenthesized args",
                                        ));
                                    }
                                }
                            } else {
                                path.path.segments.push(first);
                                for segment in segments {
                                    path.path.segments.push(segment);
                                }
                            }
                        }
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
                for elem in &mut tuple.elems {
                    self.replace_type(elem)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    // Return Some(usize) if the angled bracketed generic arguments is a `typle_ident!` macro.
    fn typle_ident(&self, args: &mut syn::AngleBracketedGenericArguments) -> Result<Option<usize>> {
        if args.args.len() == 1 {
            if let Some(GenericArgument::Type(Type::Macro(TypeMacro { mac }))) =
                args.args.first_mut()
            {
                if let Some(macro_ident) = mac.path.get_ident() {
                    if macro_ident == "typle_ident" {
                        let mut expr = parse2::<Expr>(std::mem::take(&mut mac.tokens))?;
                        let mut state = BlockState::default();
                        self.replace_expr(&mut expr, &mut state)?;
                        let Some(index) = evaluate_usize(&expr) else {
                            abort!(mac.tokens, "expect constant integer");
                        };
                        return Ok(Some(index));
                    }
                }
            }
        }
        Ok(None)
    }
}

pub fn ident_to_path(ident: Ident) -> Path {
    syn::Path {
        leading_colon: None,
        segments: std::iter::once(PathSegment {
            ident,
            arguments: PathArguments::None,
        })
        .collect(),
    }
}

fn remove_constraints(
    fn_type_params: &Punctuated<GenericParam, token::Comma>,
) -> Punctuated<GenericParam, token::Comma> {
    let mut output = fn_type_params.clone();
    for param in &mut output {
        if let GenericParam::Type(type_param) = param {
            type_param.bounds = Punctuated::new();
        }
    }
    output
}

fn expr_to_pat(expr: Expr) -> Result<Pat> {
    let default_span = expr.span();
    match expr {
        Expr::Block(expr) => {
            let mut iter = expr.block.stmts.into_iter();
            let Some(stmt) = iter.next() else {
                // empty block represents missing pattern
                return Ok(Pat::Verbatim(TokenStream::new()));
            };
            if iter.next().is_some() {
                return Err(Error::new(
                    default_span,
                    "typle requires a block with a single pattern",
                ));
            }
            match stmt {
                Stmt::Local(local) => {
                    Err(Error::new(local.span(), "let statement not supported here"))
                }
                Stmt::Item(item) => Err(Error::new(item.span(), "item not supported here")),
                Stmt::Expr(expr, None) => expr_to_pat(expr),
                Stmt::Expr(_, Some(semi)) => Err(Error::new(semi.span(), "unexpected semicolon")),
                Stmt::Macro(m) => match m.mac.path.get_ident() {
                    Some(ident) if ident == "typle_pat" => Pat::parse_single.parse2(m.mac.tokens),
                    _ => Ok(Pat::Macro(ExprMacro {
                        attrs: m.attrs,
                        mac: m.mac,
                    })),
                },
            }
        }
        Expr::Const(expr) => Ok(Pat::Const(expr)),
        Expr::Infer(expr) => Ok(Pat::Wild(PatWild {
            attrs: expr.attrs,
            underscore_token: expr.underscore_token,
        })),
        Expr::Lit(expr) => Ok(Pat::Lit(expr)),
        Expr::Macro(expr) => match expr.mac.path.get_ident() {
            Some(ident) if ident == "typle_pat" => Pat::parse_single.parse2(expr.mac.tokens),
            _ => Ok(Pat::Macro(expr)),
        },
        Expr::Paren(expr) => Ok(Pat::Paren(PatParen {
            attrs: expr.attrs,
            paren_token: expr.paren_token,
            pat: Box::new(expr_to_pat(*expr.expr)?),
        })),
        Expr::Path(expr) => Ok(Pat::Path(expr)),
        Expr::Reference(expr) => Ok(Pat::Reference(PatReference {
            attrs: expr.attrs,
            and_token: expr.and_token,
            mutability: expr.mutability,
            pat: Box::new(expr_to_pat(*expr.expr)?),
        })),
        Expr::Tuple(expr) => Ok(Pat::Tuple(PatTuple {
            attrs: expr.attrs,
            paren_token: expr.paren_token,
            elems: expr
                .elems
                .into_iter()
                .map(expr_to_pat)
                .collect::<Result<_>>()?,
        })),
        Expr::Verbatim(token_stream) => Ok(Pat::Verbatim(token_stream)),
        _ => Err(Error::new(
            default_span,
            "typle does not support this pattern",
        )),
    }
}

fn pat_to_expr(pat: Pat) -> Result<Expr> {
    match pat {
        Pat::Const(p) => Ok(Expr::Const(p)),
        Pat::Ident(p) => Ok(Expr::Path(ExprPath {
            attrs: p.attrs,
            qself: None,
            path: ident_to_path(p.ident),
        })),
        Pat::Lit(p) => Ok(Expr::Lit(p)),
        Pat::Macro(p) => Ok(Expr::Macro(p)),
        Pat::Tuple(p) => Ok(Expr::Tuple(ExprTuple {
            attrs: p.attrs,
            paren_token: p.paren_token,
            elems: p
                .elems
                .into_iter()
                .map(pat_to_expr)
                .collect::<Result<_>>()?,
        })),
        _ => Err(Error::new(pat.span(), "unsupported pattern")),
    }
}

fn expr_to_type(expr: Expr) -> Result<Type> {
    match expr {
        Expr::Block(expr) => {
            let span = expr.span();
            let mut iter = expr.block.stmts.into_iter();
            let Some(stmt) = iter.next() else {
                // empty block represents missing type
                return Ok(Type::Verbatim(TokenStream::new()));
            };
            if iter.next().is_some() {
                return Err(Error::new(
                    span,
                    "typle requires a block with a single type",
                ));
            }
            match stmt {
                Stmt::Local(local) => {
                    Err(Error::new(local.span(), "let statement not supported here"))
                }
                Stmt::Item(item) => Err(Error::new(item.span(), "item not supported here")),
                Stmt::Expr(expr, None) => expr_to_type(expr),
                Stmt::Expr(_, Some(semi)) => Err(Error::new(semi.span(), "unexpected semicolon")),
                Stmt::Macro(m) => match m.mac.path.get_ident() {
                    Some(ident) if ident == "typle_ty" => parse2(m.mac.tokens),
                    _ => Ok(Type::Macro(TypeMacro { mac: m.mac })),
                },
            }
        }
        Expr::Infer(expr) => Ok(Type::Infer(TypeInfer {
            underscore_token: expr.underscore_token,
        })),
        Expr::Macro(expr) => match expr.mac.path.get_ident() {
            Some(ident) if ident == "typle_ty" => parse2(expr.mac.tokens),
            _ => Ok(Type::Macro(TypeMacro { mac: expr.mac })),
        },
        Expr::Paren(expr) => Ok(Type::Paren(syn::TypeParen {
            paren_token: expr.paren_token,
            elem: Box::new(expr_to_type(*expr.expr)?),
        })),
        Expr::Path(expr) => Ok(Type::Path(TypePath {
            qself: None,
            path: expr.path,
        })),
        Expr::Reference(expr) => Ok(Type::Reference(syn::TypeReference {
            and_token: expr.and_token,
            lifetime: None,
            mutability: expr.mutability,
            elem: Box::new(expr_to_type(*expr.expr)?),
        })),
        Expr::Tuple(expr) => Ok(Type::Tuple(TypeTuple {
            paren_token: expr.paren_token,
            elems: expr
                .elems
                .into_iter()
                .map(expr_to_type)
                .collect::<Result<_>>()?,
        })),
        Expr::Verbatim(token_stream) => Ok(Type::Verbatim(token_stream)),
        _ => Err(Error::new(
            expr.span(),
            "typle does not handle this expression here",
        )),
    }
}

pub fn first_path_segment_mut(mut type_path: &mut TypePath) -> Option<&mut PathSegment> {
    while let Some(qself) = &mut type_path.qself {
        // <<T<_> as Trait>::Value as std::ops::Mul>::Output: Copy
        if let Type::Path(inner_type_path) = &mut *qself.ty {
            type_path = inner_type_path;
        } else {
            return None;
        }
    }
    type_path.path.segments.first_mut()
}
