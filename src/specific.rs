use std::collections::HashMap;
use std::ops::Range;

use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use proc_macro_error::abort;
use quote::{format_ident, ToTokens};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::{
    parse2, token, Block, Expr, ExprArray, ExprBlock, ExprField, ExprLit, ExprTuple, Fields,
    FieldsNamed, FieldsUnnamed, GenericArgument, GenericParam, Generics, ImplItem, Index, Item,
    Lit, LitInt, Macro, MacroDelimiter, Member, Pat, Path, PathArguments, PathSegment,
    PredicateType, QSelf, RangeLimits, ReturnType, Stmt, Type, TypeMacro, TypeParamBound, TypePath,
    TypeTuple, Variant, WhereClause, WherePredicate,
};

use crate::constant::{evaluate_bool, evaluate_usize};

const TYPLE_ASSOC_TYPE_NAME: &str = "Types";

enum EvaluationContext {
    Type,
    Value,
}

#[derive(Clone)]
pub enum Typle {
    Specific(Type),
    Generic(Vec<String>),
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

#[derive(Clone)]
pub struct SpecificContext<'a> {
    pub typle_trait: &'a Ident,
    pub typle_len: usize,
    pub constants: HashMap<Ident, usize>,
    pub typles: HashMap<Ident, Typle>,
}

impl<'a> SpecificContext<'a> {
    pub fn handle_where_clause(&self, where_clause: &mut Option<WhereClause>) -> Option<Self> {
        fn get_type_ident(predicate_type: &PredicateType) -> Option<&Ident> {
            if let Type::Path(type_path) = &predicate_type.bounded_ty {
                if type_path.qself.is_none() {
                    return type_path.path.get_ident();
                }
            }
            None
        }

        let mut context = None;
        if let Some(where_clause) = where_clause.as_mut() {
            let predicates = std::mem::take(&mut where_clause.predicates);
            for predicate in predicates {
                if let WherePredicate::Type(predicate_type) = &predicate {
                    if let Some(type_ident) = get_type_ident(predicate_type) {
                        let mut bounds = predicate_type.bounds.iter();
                        if let Some(TypeParamBound::Trait(trait_bound)) = bounds.next() {
                            if bounds.next().is_none() {
                                let path = &trait_bound.path;
                                if path.leading_colon.is_none() && path.segments.len() == 1 {
                                    if let Some(segment) = path.segments.first() {
                                        if &segment.ident == self.typle_trait {
                                            match &segment.arguments {
                                                PathArguments::None => {
                                                    let context =
                                                        context.get_or_insert_with(|| self.clone());
                                                    context.typles.insert(
                                                        type_ident.clone(),
                                                        Typle::Generic(
                                                            (0..self.typle_len)
                                                                .map(|i| {
                                                                    format!("{}{}", &type_ident, i)
                                                                })
                                                                .collect(),
                                                        ),
                                                    );
                                                    continue;
                                                }
                                                PathArguments::AngleBracketed(arguments) => {
                                                    if arguments.args.len() != 1 {
                                                        abort!(
                                                            arguments,
                                                            "expected single argument"
                                                        );
                                                    }
                                                    if let Some(GenericArgument::AssocType(assoc)) =
                                                        arguments.args.first()
                                                    {
                                                        if assoc.ident == TYPLE_ASSOC_TYPE_NAME {
                                                            let context = context
                                                                .get_or_insert_with(|| {
                                                                    self.clone()
                                                                });
                                                            context.typles.insert(
                                                                type_ident.clone(),
                                                                Typle::Specific(assoc.ty.clone()),
                                                            );
                                                            continue;
                                                        }
                                                    }
                                                    abort!(arguments, "expected Types");
                                                }
                                                PathArguments::Parenthesized(arguments) => {
                                                    abort!(
                                                        arguments,
                                                        "parenthesized arguments not supported"
                                                    )
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                where_clause.predicates.push(predicate);
            }
        }
        context
    }

    fn replace_block(&self, block: &mut Block) {
        for stmt in &mut block.stmts {
            match stmt {
                Stmt::Local(local) => {
                    self.replace_pat(&mut local.pat);
                    if let Some(init) = &mut local.init {
                        self.replace_expr(&mut init.expr);
                        if let Some((_, diverge)) = &mut init.diverge {
                            self.replace_expr(diverge);
                        }
                    }
                }
                Stmt::Item(item) => {
                    self.replace_item(item, false);
                }
                Stmt::Expr(expr, _) => self.replace_expr(expr),
                Stmt::Macro(stmt_macro) => {
                    self.replace_macro(&mut stmt_macro.mac, EvaluationContext::Value)
                }
            }
        }
    }

    fn replace_expr(&self, expr: &mut Expr) {
        match expr {
            Expr::Array(array) => {
                for expr in &mut array.elems {
                    self.replace_expr(expr);
                }
            }
            Expr::Assign(assign) => {
                self.replace_expr(&mut assign.left);
                self.replace_expr(&mut assign.right);
            }
            Expr::Async(r#async) => {
                self.replace_block(&mut r#async.block);
            }
            Expr::Await(r#await) => {
                self.replace_expr(&mut r#await.base);
            }
            Expr::Binary(binary) => {
                self.replace_expr(&mut binary.left);
                self.replace_expr(&mut binary.right);
            }
            Expr::Block(block) => {
                self.replace_block(&mut block.block);
            }
            Expr::Break(r#break) => {
                if let Some(expr) = &mut r#break.expr {
                    self.replace_expr(expr);
                }
            }
            Expr::Call(call) => {
                self.replace_expr(&mut call.func);
                for expr in &mut call.args {
                    self.replace_expr(expr);
                }
            }
            Expr::Cast(cast) => {
                self.replace_expr(&mut cast.expr);
                self.replace_type(&mut cast.ty);
            }
            Expr::Closure(closure) => {
                for pat in &mut closure.inputs {
                    self.replace_pat(pat);
                }
                if let ReturnType::Type(_, ret_type) = &mut closure.output {
                    self.replace_type(ret_type);
                }
                self.replace_expr(&mut closure.body);
            }
            Expr::Const(r#const) => {
                self.replace_block(&mut r#const.block);
            }
            Expr::Continue(_) => {}
            Expr::Field(field) => {
                self.replace_expr(&mut field.base);
            }
            Expr::ForLoop(for_loop) => {
                self.replace_expr(&mut for_loop.expr);
                // Check for typle_const!(i)
                if let Pat::Macro(pat_macro) = &mut *for_loop.pat {
                    if let Some(macro_ident) = pat_macro.mac.path.get_ident() {
                        if macro_ident == "typle_const" {
                            let span = pat_macro.mac.tokens.span();
                            let mut tokens = std::mem::take(&mut pat_macro.mac.tokens).into_iter();
                            let Some(TokenTree::Ident(pat_ident)) = tokens.next() else {
                                abort!(span, "expected identifier in typle_const macro");
                            };
                            if let Some(tt) = tokens.next() {
                                abort!(tt, "unexpected token in typle_const");
                            };
                            let Expr::Range(expr_range) = &*for_loop.expr else {
                                abort!(for_loop.expr, "expected range");
                            };
                            let (Some(start_expr), Some(end_expr)) =
                                (&expr_range.start, &expr_range.end)
                            else {
                                abort!(for_loop.expr, "expected bounded range");
                            };
                            let Some(start) = evaluate_usize(start_expr) else {
                                abort!(
                                    for_loop.expr.span(),
                                    "cannot evaluate lower bound in constant context"
                                );
                            };
                            let Some(mut end) = evaluate_usize(end_expr) else {
                                abort!(
                                    for_loop.expr.span(),
                                    "cannot evaluate upper bound in constant context"
                                );
                            };
                            if let RangeLimits::Closed(_) = expr_range.limits {
                                end += 1;
                            }
                            if start > end || end > self.typle_len {
                                abort!(for_loop.expr, "expected sub-range of tuple size");
                            }
                            let mut context = self.clone();
                            let mut stmts = Vec::new();
                            context.constants.insert(pat_ident.clone(), 0);
                            for index in start..end {
                                context.constants.get_mut(&pat_ident).map(|v| *v = index);
                                let mut block = for_loop.body.clone();
                                context.replace_block(&mut block);
                                stmts.push(Stmt::Expr(
                                    Expr::Block(ExprBlock {
                                        attrs: Vec::new(),
                                        label: None,
                                        block,
                                    }),
                                    None,
                                ));
                            }
                            stmts.push(Stmt::Expr(
                                Expr::Tuple(ExprTuple {
                                    attrs: Vec::new(),
                                    paren_token: token::Paren::default(),
                                    elems: Punctuated::new(),
                                }),
                                None,
                            ));
                            *expr = Expr::Block(ExprBlock {
                                attrs: std::mem::take(&mut for_loop.attrs),
                                label: for_loop.label.take(),
                                block: Block {
                                    brace_token: token::Brace::default(),
                                    stmts,
                                },
                            });
                            return;
                        }
                    }
                }
                // Otherwise it is a standard for loop
                self.replace_pat(&mut for_loop.pat);
                self.replace_block(&mut for_loop.body);
            }
            Expr::Group(group) => {
                self.replace_expr(&mut group.expr);
            }
            Expr::If(r#if) => {
                // Check for if typle_const!(i == T::LEN) {}
                if let Expr::Macro(expr_macro) = &mut *r#if.cond {
                    if let Some(macro_ident) = expr_macro.mac.path.get_ident() {
                        if macro_ident == "typle_const" {
                            let span = expr_macro.mac.tokens.span();
                            let tokens = std::mem::take(&mut expr_macro.mac.tokens);
                            let Ok(mut cond) = parse2::<Expr>(tokens) else {
                                abort!(span, "expected expression");
                            };
                            self.replace_expr(&mut cond);
                            let Some(b) = evaluate_bool(&cond) else {
                                abort!(span, "expected boolean expression");
                            };
                            if b {
                                *expr = Expr::Block(ExprBlock {
                                    attrs: std::mem::take(&mut r#if.attrs),
                                    label: None,
                                    block: r#if.then_branch.clone(),
                                });
                                self.replace_expr(expr);
                            } else {
                                match r#if.else_branch.take() {
                                    Some((_, branch)) => {
                                        *expr = *branch;
                                        self.replace_expr(expr);
                                    }
                                    None => {
                                        *expr = Expr::Block(ExprBlock {
                                            attrs: std::mem::take(&mut r#if.attrs),
                                            label: None,
                                            block: Block {
                                                brace_token: token::Brace::default(),
                                                stmts: Vec::new(),
                                            },
                                        });
                                    }
                                }
                            }
                            return;
                        }
                    }
                }

                self.replace_expr(&mut r#if.cond);
                self.replace_block(&mut r#if.then_branch);
                if let Some((_, block)) = &mut r#if.else_branch {
                    self.replace_expr(block);
                }
            }
            Expr::Index(index) => {
                self.replace_expr(&mut index.expr);
                if let Expr::Array(array) = &mut *index.index {
                    // t[[0]]
                    assert_eq!(array.elems.len(), 1);
                    self.replace_expr(&mut array.elems[0]);
                    let Some(i) = evaluate_usize(&array.elems[0]) else {
                        abort!(index.index, "unsupported tuple index")
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
                    self.replace_expr(&mut index.index);
                }
            }
            Expr::Let(r#let) => {
                self.replace_pat(&mut r#let.pat);
                self.replace_expr(&mut r#let.expr);
            }
            Expr::Loop(r#loop) => {
                self.replace_block(&mut r#loop.body);
            }
            Expr::Macro(r#macro) => {
                self.replace_macro(&mut r#macro.mac, EvaluationContext::Value);
            }
            Expr::Match(r#match) => {
                self.replace_expr(&mut r#match.expr);
                for arm in &mut r#match.arms {
                    self.replace_pat(&mut arm.pat);
                    if let Some((_, expr)) = &mut arm.guard {
                        self.replace_expr(expr);
                    }
                    self.replace_expr(&mut arm.body);
                }
            }
            Expr::MethodCall(method_call) => {
                self.replace_expr(&mut method_call.receiver);
                if let Some(args) = &mut method_call.turbofish {
                    // Arc::<T>::new(t)
                    self.replace_generic_arguments(&mut args.args);
                    if args.args.is_empty() {
                        method_call.turbofish = None;
                    }
                }
                for arg in &mut method_call.args {
                    self.replace_expr(arg);
                }
            }
            Expr::Paren(paren) => {
                self.replace_expr(&mut paren.expr);
            }
            Expr::Path(path) => {
                if let Some(qself) = &mut path.qself {
                    // <T as Default>::default()
                    // <T::<0> as Default>::default()
                    self.replace_type(&mut qself.ty);
                }
                let mut segments = std::mem::take(&mut path.path.segments)
                    .into_iter()
                    .peekable();
                if let Some(first) = segments.peek() {
                    if &first.ident == self.typle_trait {
                        let _ = segments.next().unwrap();
                        match segments.peek() {
                            Some(second) => {
                                if second.ident == "LEN" {
                                    // Tuple::LEN or <T as Tuple>::LEN
                                    // todo: check that any qself is a type tuple of the correct length
                                    *expr = Expr::Lit(ExprLit {
                                        attrs: std::mem::take(&mut path.attrs),
                                        lit: Lit::Int(LitInt::new(
                                            &self.typle_len.to_string(),
                                            path.span(),
                                        )),
                                    });
                                    return;
                                }
                            }
                            None => {}
                        }
                    } else if let Some(typle) = self.typles.get(&first.ident) {
                        let mut first = segments.next().unwrap();
                        match &mut first.arguments {
                            PathArguments::None => {
                                match segments.peek() {
                                    Some(second) => {
                                        if second.ident == "LEN" {
                                            // T::LEN
                                            *expr = Expr::Lit(ExprLit {
                                                attrs: std::mem::take(&mut path.attrs),
                                                lit: Lit::Int(LitInt::new(
                                                    &self.typle_len.to_string(),
                                                    path.span(),
                                                )),
                                            });
                                            return;
                                        }
                                        // T::clone(&t) -> <(T0, T1)>::clone(&t)
                                        let tuple_type = Box::new(Type::Tuple(TypeTuple {
                                            paren_token: token::Paren::default(),
                                            elems: (0..self.typle_len)
                                                .into_iter()
                                                .map(|i| typle.get(i, first.span()))
                                                .collect(),
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
                                        self.replace_expr(expr);
                                        // T<{5 - 1}>
                                        let Some(value) = evaluate_usize(expr) else {
                                            abort!(expr, "unsupported tuple type index");
                                        };
                                        // T<{4}>
                                        let qself = typle.get(value, first.span());
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
                                        )
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
                        return;
                    }
                    for mut path_segment in segments {
                        match &mut path_segment.arguments {
                            PathArguments::None => {}
                            PathArguments::AngleBracketed(args) => {
                                if let Some(index) = self.typle_index(args) {
                                    // X::<typle_index!(3)> -> X3
                                    path_segment.ident =
                                        format_ident!("{}{}", path_segment.ident, index);
                                    path_segment.arguments = PathArguments::None;
                                } else {
                                    // std::option::Some(T> -> std::option::Option<(T0, T1,...)
                                    // std::option::Option<T<3>> -> std::option::Option<T3>
                                    for arg in std::mem::take(&mut args.args) {
                                        match arg {
                                            GenericArgument::Type(mut generic_type) => {
                                                self.replace_type(&mut generic_type);
                                                args.args.push(GenericArgument::Type(generic_type));
                                            }
                                            GenericArgument::AssocType(mut assoc_type) => {
                                                self.replace_type(&mut assoc_type.ty);
                                                args.args
                                                    .push(GenericArgument::AssocType(assoc_type));
                                            }
                                            p => {
                                                args.args.push(p);
                                            }
                                        }
                                    }
                                    if args.args.is_empty() {
                                        path_segment.arguments = PathArguments::None;
                                    }
                                }
                            }
                            PathArguments::Parenthesized(_) => todo!(),
                        }
                        path.path.segments.push(path_segment)
                    }
                }
            }
            Expr::Range(range) => {
                if let Some(start) = &mut range.start {
                    self.replace_expr(start);
                }
                if let Some(end) = &mut range.end {
                    self.replace_expr(end);
                }
            }
            Expr::Reference(reference) => {
                self.replace_expr(&mut reference.expr);
            }
            Expr::Repeat(repeat) => {
                self.replace_expr(&mut repeat.expr);
                self.replace_expr(&mut repeat.len);
            }
            Expr::Return(r#return) => {
                if let Some(expr) = &mut r#return.expr {
                    self.replace_expr(expr);
                }
            }
            Expr::Struct(r#struct) => {
                self.replace_path_arguments(&mut r#struct.path);
                for field in &mut r#struct.fields {
                    self.replace_expr(&mut field.expr);
                }
                if let Some(expr) = &mut r#struct.rest {
                    self.replace_expr(expr);
                }
            }
            Expr::Try(r#try) => {
                self.replace_expr(&mut r#try.expr);
            }
            Expr::TryBlock(try_block) => {
                self.replace_block(&mut try_block.block);
            }
            Expr::Tuple(tuple) => {
                for expr in &mut tuple.elems {
                    self.replace_expr(expr);
                }
            }
            Expr::Unary(unary) => {
                self.replace_expr(&mut unary.expr);
            }
            Expr::Unsafe(r#unsafe) => {
                self.replace_block(&mut r#unsafe.block);
            }
            Expr::While(r#while) => {
                self.replace_expr(&mut r#while.cond);
                self.replace_block(&mut r#while.body);
            }
            Expr::Yield(r#yield) => {
                if let Some(expr) = &mut r#yield.expr {
                    self.replace_expr(expr);
                }
            }
            _ => {}
        }
    }

    fn replace_fields(&self, fields: &mut Fields) {
        match fields {
            Fields::Named(FieldsNamed { named: fields, .. })
            | Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            }) => {
                for field in fields {
                    self.replace_type(&mut field.ty);
                }
            }
            Fields::Unit => {}
        }
    }

    pub fn replace_generics(&self, generics: &mut Generics) {
        if let Some(where_clause) = &mut generics.where_clause {
            for mut predicate in std::mem::take(&mut where_clause.predicates) {
                if let WherePredicate::Type(predicate_type) = &mut predicate {
                    if let Type::Path(type_path) = &predicate_type.bounded_ty {
                        if type_path.qself.is_none() && type_path.path.leading_colon.is_none() {
                            let mut segments = type_path.path.segments.iter();
                            if let Some(first) = segments.next() {
                                if let Some(Typle::Generic(component_names)) =
                                    self.typles.get(&first.ident)
                                {
                                    // T::Types, T::Types::Output, T<i>
                                    match &first.arguments {
                                        PathArguments::None => {
                                            // T::Types: AsRef<str> or T::Types::Output: AsRef<str>
                                            let Some(second) = segments.next() else {
                                                abort!(first, "single typle type unexpected");
                                            };
                                            if second.ident != TYPLE_ASSOC_TYPE_NAME {
                                                abort!(second, "unknown associated item");
                                            }
                                            for component_name in component_names {
                                                let component_ident =
                                                    Ident::new(&component_name, first.ident.span());
                                                let mut path = ident_to_path(component_ident);
                                                for segment in segments.clone() {
                                                    path.segments.push(segment.clone());
                                                }
                                                let bounds = predicate_type
                                                    .bounds
                                                    .iter()
                                                    .map(|bound| {
                                                        let mut bound = bound.clone();
                                                        if let TypeParamBound::Trait(trait_bound) =
                                                            &mut bound
                                                        {
                                                            self.replace_path_arguments(
                                                                &mut trait_bound.path,
                                                            );
                                                        }
                                                        bound
                                                    })
                                                    .collect();
                                                where_clause.predicates.push(WherePredicate::Type(
                                                    PredicateType {
                                                        lifetimes: None,
                                                        bounded_ty: Type::Path(TypePath {
                                                            qself: None,
                                                            path,
                                                        }),
                                                        colon_token: token::Colon::default(),
                                                        bounds,
                                                    },
                                                ));
                                            }
                                        }
                                        PathArguments::AngleBracketed(arguments) => {
                                            // T<{i}>: Mul<M<{i}>>
                                            if let Some(second) = segments.next() {
                                                abort!(second, "path unexpected");
                                            };
                                            let mut iter = arguments.args.iter();
                                            let (
                                                Some(GenericArgument::Const(Expr::Block(expr))),
                                                None,
                                            ) = (iter.next(), iter.next())
                                            else {
                                                abort!(arguments, "expected single const block");
                                            };
                                            let mut iter = expr.block.stmts.iter();
                                            let (Some(Stmt::Expr(Expr::Path(expr), _)), None) =
                                                (iter.next(), iter.next())
                                            else {
                                                abort!(expr, "expected single const block");
                                            };
                                            let (Some(ident), None) =
                                                (expr.path.get_ident(), expr.qself.as_ref())
                                            else {
                                                abort!(expr, "expected single const block");
                                            };
                                            let mut context = self.clone();
                                            context.constants.insert(ident.clone(), 0);
                                            for (index, component_name) in
                                                component_names.into_iter().enumerate()
                                            {
                                                *context.constants.get_mut(ident).unwrap() = index;
                                                let component_ident =
                                                    Ident::new(&component_name, first.ident.span());
                                                let path = ident_to_path(component_ident);
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
                                                            );
                                                        }
                                                        bound
                                                    })
                                                    .collect();
                                                where_clause.predicates.push(WherePredicate::Type(
                                                    PredicateType {
                                                        lifetimes: None,
                                                        bounded_ty: Type::Path(TypePath {
                                                            qself: None,
                                                            path,
                                                        }),
                                                        colon_token: token::Colon::default(),
                                                        bounds,
                                                    },
                                                ));
                                            }
                                        }
                                        PathArguments::Parenthesized(_) => todo!(),
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    for bound in &mut predicate_type.bounds {
                        // substitute any appearances of typles in the constraints
                        // (e.g. T::Types: Extract<Output = S<0>::Output>)
                        if let TypeParamBound::Trait(trait_bound) = bound {
                            self.replace_path_arguments(&mut trait_bound.path);
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
                        for component_name in component_names {
                            let mut param = type_param.clone();
                            param.ident = Ident::new(&component_name, type_param.ident.span());
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
    }

    fn replace_generic_arguments(&self, args: &mut Punctuated<GenericArgument, token::Comma>) {
        for arg in std::mem::take(args) {
            match arg {
                GenericArgument::Type(mut generic_type) => {
                    self.replace_type(&mut generic_type);
                    args.push(GenericArgument::Type(generic_type));
                }
                p => {
                    args.push(p);
                }
            }
        }
    }

    pub fn replace_item(&self, item: &mut Item, top_level: bool) {
        match item {
            Item::Const(r#const) => {
                let context = self.handle_where_clause(&mut r#const.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_type(&mut r#const.ty);
                context.replace_expr(&mut r#const.expr);
            }
            Item::Enum(r#enum) => {
                if top_level {
                    r#enum.ident = format_ident!("{}{}", r#enum.ident, self.typle_len);
                }
                let context = self.handle_where_clause(&mut r#enum.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_generics(&mut r#enum.generics);
                for mut variant in std::mem::take(&mut r#enum.variants) {
                    if let Fields::Unit = variant.fields {
                        if let Some((_, discriminant)) = &mut variant.discriminant {
                            if let Expr::Macro(r#macro) = discriminant {
                                if let Some(ident) = r#macro.mac.path.get_ident() {
                                    if ident == "typle_variant" {
                                        let mut token_stream =
                                            std::mem::take(&mut r#macro.mac.tokens);
                                        let (pattern, range) =
                                            context.parse_pattern_range(&mut token_stream);
                                        let fields = match r#macro.mac.delimiter {
                                            MacroDelimiter::Paren(_) => {
                                                let group = TokenTree::Group(Group::new(
                                                    proc_macro2::Delimiter::Parenthesis,
                                                    token_stream,
                                                ));
                                                Fields::Unnamed(
                                                    parse2::<FieldsUnnamed>(TokenStream::from(
                                                        group,
                                                    ))
                                                    .unwrap_or_else(|err| {
                                                        abort!(r#macro, "{}", err)
                                                    }),
                                                )
                                            }
                                            MacroDelimiter::Brace(_) => {
                                                let group = TokenTree::Group(Group::new(
                                                    proc_macro2::Delimiter::Brace,
                                                    token_stream,
                                                ));
                                                Fields::Named(
                                                    parse2::<FieldsNamed>(TokenStream::from(group))
                                                        .unwrap_or_else(|err| {
                                                            abort!(r#macro, "{}", err)
                                                        }),
                                                )
                                            }
                                            MacroDelimiter::Bracket(_) => {
                                                if !token_stream.is_empty() {
                                                    abort!(
                                                        token_stream,
                                                        "braces require empty body"
                                                    )
                                                }
                                                Fields::Unit
                                            }
                                        };
                                        for index in range {
                                            let mut context = context.clone();
                                            if let Some(ident) = pattern.clone() {
                                                if ident != "_" {
                                                    context.constants.insert(ident, index);
                                                }
                                            }
                                            let mut fields = fields.clone();
                                            match &mut fields {
                                                Fields::Named(FieldsNamed {
                                                    named: fields,
                                                    ..
                                                })
                                                | Fields::Unnamed(FieldsUnnamed {
                                                    unnamed: fields,
                                                    ..
                                                }) => {
                                                    for field in fields {
                                                        context.replace_type(&mut field.ty);
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
                                            r#enum.variants.push(variant);
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                    }
                    context.replace_fields(&mut variant.fields);
                    if let Some((_, discriminant)) = &mut variant.discriminant {
                        context.replace_expr(discriminant);
                    }
                    r#enum.variants.push(variant);
                }
            }
            Item::ExternCrate(_) => abort!(item, "ExternCrate unsupported"),
            Item::Fn(function) => {
                if top_level {
                    function.sig.ident = format_ident!("{}{}", function.sig.ident, self.typle_len);
                }
                let context = self.handle_where_clause(&mut function.sig.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_signature(&mut function.sig);
                context.replace_block(&mut function.block);
            }
            Item::ForeignMod(_) => abort!(item, "ForeignMod unsupported"),
            Item::Impl(r#impl) => {
                let context = self.handle_where_clause(&mut r#impl.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_generics(&mut r#impl.generics);
                if let Some((_, path, _)) = &mut r#impl.trait_ {
                    context.replace_path_arguments(path);
                }
                context.replace_type(&mut *r#impl.self_ty);
                for subitem in &mut r#impl.items {
                    match subitem {
                        ImplItem::Const(constant) => {
                            context.replace_type(&mut constant.ty);
                            context.replace_expr(&mut constant.expr);
                        }
                        ImplItem::Fn(function) => {
                            let inner_context = context
                                .handle_where_clause(&mut function.sig.generics.where_clause);
                            let inner_context = inner_context.as_ref().unwrap_or(context);
                            inner_context.replace_signature(&mut function.sig);
                            inner_context.replace_block(&mut function.block);
                        }
                        ImplItem::Type(ty) => {
                            context.replace_type(&mut ty.ty);
                        }
                        ImplItem::Macro(r#macro) => {
                            context.replace_macro(&mut r#macro.mac, EvaluationContext::Type);
                        }
                        _ => {}
                    }
                }
            }
            Item::Macro(_) => abort!(item, "Macro unsupported"),
            Item::Mod(_) => abort!(item, "Mod unsupported"),
            Item::Static(_) => abort!(item, "Static unsupported"),
            Item::Struct(r#struct) => {
                if top_level {
                    r#struct.ident = format_ident!("{}{}", r#struct.ident, self.typle_len);
                }
                let context = self.handle_where_clause(&mut r#struct.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_generics(&mut r#struct.generics);
                context.replace_fields(&mut r#struct.fields);
            }
            Item::Trait(_) => abort!(item, "Trait unsupported"),
            Item::TraitAlias(_) => abort!(item, "TraitAlias unsupported"),
            Item::Type(r#type) => {
                if top_level {
                    r#type.ident = format_ident!("{}{}", r#type.ident, self.typle_len);
                }
                let context = self.handle_where_clause(&mut r#type.generics.where_clause);
                let context = context.as_ref().unwrap_or(self);
                context.replace_generics(&mut r#type.generics);
                context.replace_type(&mut r#type.ty);
            }
            Item::Union(_) => abort!(item, "Union unsupported"),
            Item::Use(_) => abort!(item, "Use unsupported"),
            Item::Verbatim(_) => abort!(item, "Verbatim unsupported"),
            _ => todo!(),
        }
    }

    fn replace_macro(&self, r#macro: &mut Macro, context: EvaluationContext) {
        // typle_for!(i in .. => Option<T<{i}>) -> (Option<T0>, Option<T1>)
        // typle_for!(i in .. => T::<{i}>::default()) -> (T0::default(), T1::default())
        // as opposed to
        // Option<T> -> Option<(T0, T1)>
        // T::default() -> <(T0, T1)>::default()
        // typle_for!(i in .. => t[[i]]) -> (t.0, t.1)
        // typle_for![i in .. => t[[i]]] -> [t.0, t.1]
        // typle_for!(i in .. => (T::<{i}>, T::<0>)) -> ((T0, T0), (T1, T0))
        // typle_for!(0..2 => None) // (None, None)
        if let Some(macro_name) = r#macro.path.get_ident() {
            if macro_name == "typle_for" {
                let default_span = macro_name.span();
                r#macro.path = Path {
                    leading_colon: None,
                    segments: std::module_path!()
                        .split("::")
                        .take(1)
                        .chain(std::iter::once("typle_identity"))
                        .map(|name| PathSegment {
                            ident: Ident::new(name, macro_name.span()),
                            arguments: PathArguments::None,
                        })
                        .collect(),
                };
                let mut token_stream = std::mem::take(&mut r#macro.tokens);
                let (pattern, range) = self.parse_pattern_range(&mut token_stream);
                let body_span = token_stream.span();
                match context {
                    EvaluationContext::Type => {
                        let Ok(r#type) = parse2::<Type>(token_stream) else {
                            abort!(body_span, "expected type");
                        };
                        let mut tuple = TypeTuple {
                            paren_token: token::Paren::default(),
                            elems: Punctuated::new(),
                        };
                        for index in range {
                            let mut context = self.clone();
                            if let Some(ident) = pattern.clone() {
                                if ident != "_" {
                                    context.constants.insert(ident, index);
                                }
                            }
                            let mut component = r#type.clone();
                            context.replace_type(&mut component);
                            tuple.elems.push(component);
                        }
                        r#macro.tokens = tuple.into_token_stream();
                    }
                    EvaluationContext::Value => {
                        let Ok(expr) = parse2::<Expr>(token_stream.clone()) else {
                            abort!(body_span, "expected value");
                        };
                        let mut elems = Punctuated::new();
                        for index in range {
                            let mut context = self.clone();
                            if let Some(ident) = pattern.clone() {
                                if ident != "_" {
                                    context.constants.insert(ident, index);
                                }
                            }
                            let mut element = expr.clone();
                            context.replace_expr(&mut element);
                            elems.push(element);
                        }
                        r#macro.tokens = match r#macro.delimiter {
                            MacroDelimiter::Paren(_) => {
                                let tuple = ExprTuple {
                                    paren_token: token::Paren::default(),
                                    elems,
                                    attrs: Vec::new(),
                                };
                                tuple.into_token_stream()
                            }
                            MacroDelimiter::Brace(_) => {
                                abort!(default_span, "expected parentheses or brackets");
                            }
                            MacroDelimiter::Bracket(_) => {
                                let array = ExprArray {
                                    attrs: Vec::new(),
                                    bracket_token: token::Bracket::default(),
                                    elems,
                                };
                                array.into_token_stream()
                            }
                        };
                    }
                }
            }
        }
    }

    fn parse_pattern_range(&self, token_stream: &mut TokenStream) -> (Option<Ident>, Range<usize>) {
        let default_span = token_stream.span();
        let mut tokens = std::mem::take(token_stream).into_iter();
        let mut collect = Vec::new();
        let mut pattern = None;
        let mut startend = None;
        let mut equals = None;
        while let Some(token) = tokens.next() {
            match token {
                TokenTree::Ident(ident) if pattern.is_none() && ident == "in" => {
                    if let Some(punct) = equals.take() {
                        collect.push(TokenTree::Punct(punct))
                    }
                    let mut tokens = std::mem::take(&mut collect).into_iter();
                    match tokens.next() {
                        Some(TokenTree::Ident(ident)) => {
                            pattern = Some(ident);
                            if let Some(tt) = tokens.next() {
                                abort!(tt, "unexpected token")
                            }
                        }
                        Some(tt) => abort!(tt, "expected identifier before keyword `in`"),
                        None => abort!(ident, "expected identifier before keyword `in`"),
                    }
                }
                TokenTree::Punct(punct) if punct.as_char() == '=' => {
                    equals = Some(punct);
                }
                TokenTree::Punct(punct) if equals.is_some() && punct.as_char() == '>' => {
                    match parse2::<Expr>(TokenStream::from_iter(collect)) {
                        Ok(mut expr) => {
                            self.replace_expr(&mut expr);
                            if let Expr::Range(range) = expr {
                                let start = range
                                    .start
                                    .as_ref()
                                    .map(|expr| {
                                        evaluate_usize(&expr)
                                            .unwrap_or_else(|| abort!(expr, "range start invalid"))
                                    })
                                    .unwrap_or(0);
                                let end = range
                                    .end
                                    .as_ref()
                                    .map(|expr| {
                                        evaluate_usize(expr).unwrap_or_else(|| {
                                            abort!(range.end, "range end invalid")
                                        })
                                    })
                                    .unwrap_or(self.typle_len)
                                    .checked_add(match range.limits {
                                        RangeLimits::HalfOpen(_) => 0,
                                        RangeLimits::Closed(_) => 1,
                                    })
                                    .unwrap_or_else(|| {
                                        abort!(
                                            range,
                                            "for length {} range contains no values",
                                            self.typle_len
                                        )
                                    });
                                if end < start {
                                    abort!(
                                        range,
                                        "for length {} range contains no values",
                                        self.typle_len
                                    );
                                }
                                startend = Some(start..end);
                                break;
                            } else {
                                abort!(expr, "expected range");
                            }
                        }
                        Err(e) => abort!(default_span, "not an expression: {}", e),
                    }
                }
                tt => {
                    if let Some(punct) = equals.take() {
                        collect.push(TokenTree::Punct(punct))
                    }
                    collect.push(tt);
                }
            }
        }
        let Some(range) = startend else {
            abort!(default_span, "expected `=>`");
        };
        *token_stream = TokenStream::from_iter(tokens);
        (pattern, range)
    }

    fn replace_pat(&self, pat: &mut Pat) {
        match pat {
            Pat::Const(_) => abort!(pat, "Const unsupported"),
            Pat::Ident(_) => {}
            Pat::Lit(_) => abort!(pat, "Lit unsupported"),
            Pat::Macro(_) => abort!(pat, "Macro unsupported"),
            Pat::Or(_) => abort!(pat, "Or unsupported"),
            Pat::Paren(_) => abort!(pat, "Paren unsupported"),
            Pat::Path(path) => {
                // State::S::<typle_index!(i)> -> State::S2
                if let Some(qself) = &mut path.qself {
                    self.replace_type(&mut qself.ty);
                }
                for mut path_segment in std::mem::take(&mut path.path.segments) {
                    match &mut path_segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(args) => {
                            if let Some(index) = self.typle_index(args) {
                                // X::<typle_index!(3)> -> X3
                                path_segment.ident =
                                    format_ident!("{}{}", path_segment.ident, index);
                                path_segment.arguments = PathArguments::None;
                            } else {
                                // std::option::Some(T> -> std::option::Option<(T0, T1,...)
                                // std::option::Option<T<3>> -> std::option::Option<T3>
                                for arg in std::mem::take(&mut args.args) {
                                    match arg {
                                        GenericArgument::Type(mut generic_type) => {
                                            self.replace_type(&mut generic_type);
                                            args.args.push(GenericArgument::Type(generic_type));
                                        }
                                        p => {
                                            args.args.push(p);
                                        }
                                    }
                                }
                                if args.args.is_empty() {
                                    path_segment.arguments = PathArguments::None;
                                }
                            }
                        }
                        PathArguments::Parenthesized(_) => todo!(),
                    }
                    path.path.segments.push(path_segment)
                }
            }
            Pat::Range(_) => abort!(pat, "Range unsupported"),
            Pat::Reference(_) => abort!(pat, "Reference unsupported"),
            Pat::Rest(_) => abort!(pat, "Rest unsupported"),
            Pat::Slice(_) => abort!(pat, "Slice unsupported"),
            Pat::Struct(_) => abort!(pat, "Struct unsupported"),
            Pat::Tuple(_) => {}
            Pat::TupleStruct(tuple_struct) => {
                // State::S::<typle_index!(i)> -> State::S2
                if let Some(qself) = &mut tuple_struct.qself {
                    self.replace_type(&mut qself.ty);
                }
                for mut path_segment in std::mem::take(&mut tuple_struct.path.segments) {
                    match &mut path_segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(args) => {
                            if let Some(index) = self.typle_index(args) {
                                // X::<typle_index!(3)> -> X3
                                path_segment.ident =
                                    format_ident!("{}{}", path_segment.ident, index);
                                path_segment.arguments = PathArguments::None;
                            } else {
                                // std::option::Some(T> -> std::option::Option<(T0, T1,...)
                                // std::option::Option<T<3>> -> std::option::Option<T3>
                                for arg in std::mem::take(&mut args.args) {
                                    match arg {
                                        GenericArgument::Type(mut generic_type) => {
                                            self.replace_type(&mut generic_type);
                                            args.args.push(GenericArgument::Type(generic_type));
                                        }
                                        p => {
                                            args.args.push(p);
                                        }
                                    }
                                }
                                if args.args.is_empty() {
                                    path_segment.arguments = PathArguments::None;
                                }
                            }
                        }
                        PathArguments::Parenthesized(_) => todo!(),
                    }
                    tuple_struct.path.segments.push(path_segment)
                }
            }
            Pat::Type(pat_type) => {
                self.replace_type(&mut pat_type.ty);
            }
            Pat::Verbatim(_) => abort!(pat, "Verbatim unsupported"),
            Pat::Wild(_) => abort!(pat, "Wild unsupported"),
            _ => {}
        }
    }

    fn replace_path_arguments(&self, path: &mut Path) {
        for mut path_segment in std::mem::take(&mut path.segments) {
            match &mut path_segment.arguments {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    // WrapperType<T> -> WrapperType<(T0, T1,...)>
                    // WrapperType<T<3>> -> WrapperType<T3>
                    // WrapperType<T::Types> -> WrapperTypeN<T0, T1,...>
                    let mut typle_args = false;
                    for arg in std::mem::take(&mut args.args) {
                        match arg {
                            GenericArgument::Type(Type::Path(TypePath { qself, path }))
                                if qself.is_none() && path.leading_colon.is_none() =>
                            {
                                let mut segments = path.segments.iter();
                                if let Some(first) = segments.next() {
                                    if let (Some(typle), Some(second)) =
                                        (self.typles.get(&first.ident), segments.next())
                                    {
                                        if second.ident == TYPLE_ASSOC_TYPE_NAME {
                                            for i in 0..self.typle_len {
                                                args.args.push(GenericArgument::Type(
                                                    typle.get(i, path.span()),
                                                ));
                                            }
                                            typle_args = true;
                                        } else {
                                            let mut generic_type =
                                                Type::Path(TypePath { qself, path });
                                            self.replace_type(&mut generic_type);
                                            args.args.push(GenericArgument::Type(generic_type));
                                        }
                                    } else {
                                        let mut generic_type = Type::Path(TypePath { qself, path });
                                        self.replace_type(&mut generic_type);
                                        args.args.push(GenericArgument::Type(generic_type));
                                    }
                                } else {
                                    abort!(path, "empty path");
                                }
                            }
                            GenericArgument::Type(mut generic_type) => {
                                self.replace_type(&mut generic_type);
                                args.args.push(GenericArgument::Type(generic_type));
                            }
                            GenericArgument::AssocType(mut assoc_type) => {
                                self.replace_type(&mut assoc_type.ty);
                                args.args.push(GenericArgument::AssocType(assoc_type));
                            }
                            p => {
                                args.args.push(p);
                            }
                        }
                    }
                    if typle_args {
                        path_segment.ident =
                            format_ident!("{}{}", path_segment.ident, self.typle_len);
                    }
                    if args.args.is_empty() {
                        path_segment.arguments = PathArguments::None;
                    }
                }
                PathArguments::Parenthesized(_) => todo!(),
            }
            path.segments.push(path_segment);
        }
    }

    fn replace_signature(&self, sig: &mut syn::Signature) {
        self.replace_generics(&mut sig.generics);
        for input in &mut sig.inputs {
            if let syn::FnArg::Typed(pat_type) = input {
                self.replace_type(&mut pat_type.ty);
            }
        }
        match &mut sig.output {
            syn::ReturnType::Default => {}
            syn::ReturnType::Type(_, ty) => {
                self.replace_type(ty);
            }
        }
    }

    // Replace `T`` with `(T0, T1,...)`` and `T<1>`` with `T1``
    fn replace_type(&self, ty: &mut Type) {
        match ty {
            Type::Array(array) => {
                self.replace_type(&mut array.elem);
                self.replace_expr(&mut array.len);
            }
            Type::BareFn(bare_fn) => {
                for arg in &mut bare_fn.inputs {
                    self.replace_type(&mut arg.ty);
                }
                if let ReturnType::Type(_, ty) = &mut bare_fn.output {
                    self.replace_type(ty);
                }
            }
            Type::Group(group) => {
                self.replace_type(&mut group.elem);
            }
            Type::Macro(r#macro) => self.replace_macro(&mut r#macro.mac, EvaluationContext::Type),
            Type::Paren(paren) => {
                self.replace_type(&mut paren.elem);
            }
            Type::Path(path) => {
                match &mut path.qself {
                    Some(qself) => {
                        // <T<0> as Mul<M>>::Output
                        self.replace_type(&mut qself.ty);
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
                                            elems: (0..self.typle_len)
                                                .into_iter()
                                                .map(|i| typle.get(i, span))
                                                .collect(),
                                        });
                                        return;
                                    }
                                    PathArguments::AngleBracketed(args) => {
                                        // T<3> or T<i> where i comes from constants
                                        if args.args.len() != 1 {
                                            abort!(first, "expected one type parameter");
                                        }
                                        match args.args.first_mut() {
                                            Some(GenericArgument::Const(expr)) => {
                                                // T<{T::LEN - 1}>
                                                self.replace_expr(expr);
                                                // T<{5 - 1}>
                                                let Some(value) = evaluate_usize(expr) else {
                                                    abort!(expr, "unsupported tuple type index");
                                                };
                                                let component_type = typle.get(value, first.span());
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
                                                    return;
                                                }
                                            }
                                            _ => {
                                                abort!(
                                                    args,
                                                    "Require const parameter (wrap {} around expression)"
                                                )
                                            }
                                        }
                                    }
                                    PathArguments::Parenthesized(_) => {
                                        // Parenthesized arguments are not supported for tuple types
                                        abort!(
                                            first.arguments.span(),
                                            "unexpected patenthesized args"
                                        );
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
                self.replace_path_arguments(&mut path.path);
            }
            Type::Ptr(ptr) => {
                self.replace_type(&mut ptr.elem);
            }
            Type::Reference(reference) => {
                self.replace_type(&mut reference.elem);
            }
            Type::Slice(slice) => {
                self.replace_type(&mut slice.elem);
            }
            Type::Tuple(tuple) => {
                for elem in &mut tuple.elems {
                    self.replace_type(elem);
                }
            }
            _ => {}
        }
    }

    fn typle_index(&self, args: &mut syn::AngleBracketedGenericArguments) -> Option<usize> {
        if args.args.len() == 1 {
            if let Some(GenericArgument::Type(Type::Macro(TypeMacro { mac }))) =
                args.args.first_mut()
            {
                if let Some(macro_ident) = mac.path.get_ident() {
                    if macro_ident == "typle_index" {
                        let Ok(mut expr) = syn::parse2::<Expr>(std::mem::take(&mut mac.tokens))
                        else {
                            abort!(mac.tokens.span(), "expect expression in typle_index macro");
                        };
                        self.replace_expr(&mut expr);
                        let Some(index) = evaluate_usize(&expr) else {
                            abort!(mac.tokens.span(), "expect constant integer");
                        };
                        return Some(index);
                    }
                }
            }
        }
        None
    }
}

fn ident_to_path(ident: Ident) -> Path {
    let mut segments = Punctuated::new();
    segments.push(PathSegment {
        ident,
        arguments: PathArguments::None,
    });
    syn::Path {
        leading_colon: None,
        segments,
    }
}
