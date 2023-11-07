use std::collections::{HashMap, HashSet};

use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site};
use quote::ToTokens;
use syn::spanned::Spanned as _;
use syn::{
    Block, Expr, ExprField, ExprLit, ExprRange, GenericArgument, LitInt, Macro, Pat, Path,
    PathArguments, PathSegment, PredicateType, QSelf, Stmt, Type, TypeParamBound, TypePath,
    TypeTuple, WherePredicate,
};

#[proc_macro_attribute]
pub fn typle(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let iteration_trait = parse_args(TokenStream::from(args));

    let Ok(item) = syn::parse::<syn::Item>(item) else {
        abort_call_site!("unsupported tokens");
    };

    let mut output = Vec::new();

    output.extend(iteration_trait.process_item(item));

    output
        .into_iter()
        .map(syn::Item::into_token_stream)
        .collect::<TokenStream>()
        .into()
}

#[derive(Clone)]
struct IterationTrait {
    ident: Ident,
    min: usize,
    max: usize,
}

fn parse_args(args: TokenStream) -> IterationTrait {
    // #[typle(Tuple for 2..=12)]
    let mut args_iter = args.into_iter();
    let Some(TokenTree::Ident(trait_ident)) = args_iter.next() else {
        abort_call_site!("expected an ident");
    };
    let Some(TokenTree::Ident(for_ident)) = args_iter.next() else {
        abort_call_site!("expected 'for'");
    };
    if for_ident != "for" {
        abort_call_site!("expected 'for'");
    }
    let rest = args_iter.collect();
    let range = syn::parse2::<ExprRange>(rest).expect("expect range");
    let min = range
        .start
        .as_ref()
        .map(|expr| evaluate_usize(&expr).unwrap_or_else(|| abort!(expr, "invalid start")))
        .unwrap_or(0);
    let end = range
        .end
        .as_ref()
        .unwrap_or_else(|| abort!(range, "range cannot be unbounded at end"));
    let max = match range.limits {
        syn::RangeLimits::HalfOpen(_) => evaluate_usize(&end)
            .unwrap_or_else(|| abort!(end, "invalid end"))
            .checked_sub(1)
            .unwrap_or_else(|| abort!(end, "invalid end")),
        syn::RangeLimits::Closed(_) => {
            evaluate_usize(&end).unwrap_or_else(|| abort!(end, "invalid end"))
        }
    };
    if max < min {
        abort!(range, "nothing to expand");
    }
    IterationTrait {
        ident: trait_ident,
        min,
        max,
    }
}

impl IterationTrait {
    fn process_item(&self, input: syn::Item) -> Vec<syn::Item> {
        let mut output = Vec::new();
        match input {
            syn::Item::Const(_) => abort!(input, "Const unsupported"),
            syn::Item::Enum(_) => abort!(input, "Enum unsupported"),
            syn::Item::ExternCrate(_) => abort!(input, "ExternCrate unsupported"),
            syn::Item::Fn(_) => abort!(input, "Fn unsupported"),
            syn::Item::ForeignMod(_) => abort!(input, "ForeignMod unsupported"),
            syn::Item::Impl(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item = specific.process_impl(&item);
                        output.push(syn::Item::Impl(item));
                    }
                } else {
                    output.push(syn::Item::Impl(item));
                }
            }
            syn::Item::Macro(_) => abort!(input, "Macro unsupported"),
            syn::Item::Mod(_) => abort!(input, "Mod unsupported"),
            syn::Item::Static(_) => abort!(input, "Static unsupported"),
            syn::Item::Struct(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item_struct = specific.process_struct(&item);
                        output.push(syn::Item::Struct(item_struct));
                    }
                } else {
                    output.push(syn::Item::Struct(item));
                }
            }
            syn::Item::Trait(_) => abort!(input, "Trait unsupported"),
            syn::Item::TraitAlias(_) => abort!(input, "TraitAlias unsupported"),
            syn::Item::Type(_) => abort!(input, "Type unsupported"),
            syn::Item::Union(_) => abort!(input, "Union unsupported"),
            syn::Item::Use(_) => abort!(input, "Use unsupported"),
            syn::Item::Verbatim(_) => abort!(input, "Verbatim unsupported"),
            item => {
                output.push(item);
            }
        }
        output
    }

    fn generics_contain_trait(&self, generics: &syn::Generics) -> bool {
        let Some(where_clause) = &generics.where_clause else {
            return false;
        };
        for predicate in &where_clause.predicates {
            if let WherePredicate::Type(predicate_type) = predicate {
                for bound in &predicate_type.bounds {
                    if let TypeParamBound::Trait(trait_bound) = bound {
                        let path = &trait_bound.path;
                        if path.leading_colon.is_none()
                            && path.segments.len() == 1
                            && path.segments[0].ident == self.ident
                        {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }
}

// Returns a type if it contains no path segments and has the form `Ident<..>``
fn get_simple_type(r#type: &Type) -> Option<&syn::PathSegment> {
    let Type::Path(type_path) = r#type else {
        return None;
    };
    if type_path.qself.is_some() {
        return None;
    }
    let path = &type_path.path;
    if path.leading_colon.is_some() {
        return None;
    }
    let segments = &path.segments;
    if segments.len() != 1 {
        return None;
    }
    segments.first()
}

#[derive(Clone)]
struct SpecificContext<'a> {
    trait_ident: &'a Ident,
    count: usize,
    constants: HashMap<Ident, usize>,
    tuples: HashMap<Ident, Vec<String>>,
}

impl<'a> SpecificContext<'a> {
    fn process_struct(&mut self, item: &syn::ItemStruct) -> syn::ItemStruct {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            let item_name = format!("{}{}", item.ident, self.count);
            item.ident = Ident::new(&item_name, item.ident.span());
            match &mut item.fields {
                syn::Fields::Named(syn::FieldsNamed { named: fields, .. })
                | syn::Fields::Unnamed(syn::FieldsUnnamed {
                    unnamed: fields, ..
                }) => {
                    for field in fields {
                        self.replace_type(&mut field.ty);
                    }
                }
                syn::Fields::Unit => {}
            }
        }
        item
    }

    fn process_impl(&mut self, item: &syn::ItemImpl) -> syn::ItemImpl {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            // Replace types in type
            // S<T> -> S<(T0, T1>)
            let Type::Path(ty) = &mut *item.self_ty else {
                abort!(item.self_ty, "expected identifier");
            };
            let Some(segment) = ty.path.segments.last_mut() else {
                abort!(item.self_ty, "expected identifier");
            };
            self.replace_path_arguments(&mut segment.arguments);
            // Replace types in functions
            for subitem in &mut item.items {
                match subitem {
                    syn::ImplItem::Const(constant) => {
                        self.replace_type(&mut constant.ty);
                        self.replace_expr(&mut constant.expr);
                    }
                    syn::ImplItem::Fn(function) => {
                        self.replace_signature(&mut function.sig);
                        self.replace_block(&mut function.block);
                    }
                    syn::ImplItem::Type(ty) => {
                        self.replace_type(&mut ty.ty);
                    }
                    syn::ImplItem::Macro(r#macro) => {
                        self.replace_macro(&mut r#macro.mac);
                    }
                    _ => {}
                }
            }
        }
        item
    }

    fn replace_block(&self, block: &mut Block) {
        for stmt in &mut block.stmts {
            match stmt {
                syn::Stmt::Local(_) => todo!(),
                syn::Stmt::Item(_) => todo!(),
                syn::Stmt::Expr(expr, _) => self.replace_expr(expr),
                syn::Stmt::Macro(stmt_macro) => self.replace_macro(&mut stmt_macro.mac),
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
                if let syn::ReturnType::Type(_, ret_type) = &mut closure.output {
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
                self.replace_pat(&mut for_loop.pat);
                self.replace_expr(&mut for_loop.expr);
                self.replace_block(&mut for_loop.body);
            }
            Expr::Group(group) => {
                self.replace_expr(&mut group.expr);
            }
            Expr::If(r#if) => {
                self.replace_expr(&mut r#if.cond);
                self.replace_block(&mut r#if.then_branch);
                if let Some((_, block)) = &mut r#if.else_branch {
                    self.replace_expr(block);
                }
            }
            Expr::Index(index) => {
                self.replace_expr(&mut index.expr);
                self.replace_expr(&mut index.index);
                if let Expr::Array(array) = &*index.index {
                    // t[[0]]
                    assert_eq!(array.elems.len(), 1);
                    let Some(i) = evaluate_usize(&array.elems[0]) else {
                        abort!(index.index, "unsupported tuple index")
                    };
                    *expr = Expr::Field(ExprField {
                        attrs: std::mem::take(&mut index.attrs),
                        base: index.expr.clone(),
                        dot_token: syn::token::Dot::default(),
                        member: syn::Member::Unnamed(syn::Index {
                            index: i as u32,
                            span: index.index.span(),
                        }),
                    });
                }
            }
            Expr::Infer(_) => {}
            Expr::Let(r#let) => {
                self.replace_pat(&mut r#let.pat);
                self.replace_expr(&mut r#let.expr);
            }
            Expr::Loop(r#loop) => {
                self.replace_block(&mut r#loop.body);
            }
            Expr::Macro(r#macro) => {
                self.replace_macro(&mut r#macro.mac);
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
                    // <T>::default()
                    self.replace_type(&mut qself.ty);
                }
                let mut iter = path.path.segments.iter();
                if let Some(first) = iter.next() {
                    if let Some(element_type_idents) = self.tuples.get(&first.ident) {
                        match iter.next() {
                            Some(second) if second.ident == "LEN" => {
                                // T::LEN
                                *expr = Expr::Lit(ExprLit {
                                    attrs: std::mem::take(&mut path.attrs),
                                    lit: syn::Lit::Int(LitInt::new(
                                        &element_type_idents.len().to_string(),
                                        path.span(),
                                    )),
                                });
                            }
                            Some(second) => {
                                // T::clone(&t) -> <(T0, T1)>::clone(&t)
                                let tuple_type = Box::new(Type::Tuple(TypeTuple {
                                    paren_token: syn::token::Paren::default(),
                                    elems: element_type_idents
                                        .iter()
                                        .map(|name| {
                                            Type::Path(TypePath {
                                                qself: None,
                                                path: element_type_to_path(name, path.span()),
                                            })
                                        })
                                        .collect(),
                                }));
                                path.qself = Some(QSelf {
                                    lt_token: syn::token::Lt::default(),
                                    ty: tuple_type,
                                    position: 0,
                                    as_token: None,
                                    gt_token: syn::token::Gt::default(),
                                });
                                let mut segments = syn::punctuated::Punctuated::new();
                                segments.push(second.clone());
                                for segment in iter {
                                    segments.push(segment.clone());
                                }
                                path.path = Path {
                                    leading_colon: Some(syn::token::PathSep::default()),
                                    segments,
                                };
                            }
                            None => {
                                abort!(path, "type in expression position");
                            }
                        }
                    } else if let Some(value) = self.constants.get(&first.ident) {
                        *expr = Expr::Lit(ExprLit {
                            attrs: std::mem::take(&mut path.attrs),
                            lit: syn::Lit::Int(LitInt::new(&value.to_string(), first.ident.span())),
                        });
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
                self.replace_path(&mut r#struct.path, |path_segment| {
                    match &path_segment.arguments {
                        syn::PathArguments::None => {}
                        syn::PathArguments::AngleBracketed(_) => todo!(),
                        syn::PathArguments::Parenthesized(_) => {}
                    }
                    None::<Expr>
                });
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

    fn replace_macro(&self, _macro: &mut Macro) {
        // todo
    }

    fn replace_pat(&self, pat: &mut Pat) {
        if let Pat::Type(arg_type) = pat {
            self.replace_type(&mut arg_type.ty);
        }
    }

    fn replace_path<T, F>(&self, path: &mut Path, mut f: F) -> Option<T>
    where
        F: FnMut(&mut PathSegment) -> Option<T>,
    {
        if path.segments.len() == 1 {
            if let Some(path_segment) = path.segments.first_mut() {
                return (f)(path_segment);
            }
        }
        None
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
                if let syn::ReturnType::Type(_, ty) = &mut bare_fn.output {
                    self.replace_type(ty);
                }
            }
            Type::Group(group) => {
                self.replace_type(&mut group.elem);
            }
            Type::Macro(r#macro) => self.replace_macro(&mut r#macro.mac),
            Type::Paren(paren) => {
                self.replace_type(&mut paren.elem);
            }
            Type::Path(path) => {
                if let Some(qself) = &mut path.qself {
                    self.replace_type(&mut qself.ty);
                }
                // T or T<0> or T<0>::run or Option<T<0>>
                let Some(path_segment) = path.path.segments.first_mut() else {
                    // an empty Path?
                    return;
                };
                if let Some(element_type_idents) = self.tuples.get(&path_segment.ident) {
                    match &mut path_segment.arguments {
                        syn::PathArguments::None => {
                            // T -> (T0, T1,...)
                            let span = path_segment.ident.span();
                            *ty = Type::Tuple(TypeTuple {
                                paren_token: syn::token::Paren::default(),
                                elems: element_type_idents
                                    .iter()
                                    .map(|name| {
                                        Type::Path(TypePath {
                                            qself: None,
                                            path: element_type_to_path(name, span),
                                        })
                                    })
                                    .collect(),
                            });
                        }
                        syn::PathArguments::AngleBracketed(args) => {
                            // T<3> or T<i> where i comes from constants
                            if args.args.len() != 1 {
                                abort!(path_segment, "expected one type parameter");
                            }
                            match args.args.first_mut() {
                                Some(syn::GenericArgument::Type(expr)) => {
                                    match expr {
                                        Type::Path(path) => {
                                            if let Some(ident) = path.path.get_ident() {
                                                if let Some(value) = self.constants.get(ident) {
                                                    // T<i>
                                                    *path_segment = syn::PathSegment {
                                                        ident: Ident::new(
                                                            &element_type_idents[*value],
                                                            path_segment.ident.span(),
                                                        ),
                                                        arguments: syn::PathArguments::None,
                                                    };
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                                Some(syn::GenericArgument::Const(expr)) => {
                                    // T<{T::LEN - 1}>
                                    self.replace_expr(expr);
                                    // T<{5 - 1}>
                                    // if Block {} = expr {

                                    // }
                                    let Some(value) = evaluate_usize(expr) else {
                                        abort!(expr, "unsupported tuple type index");
                                    };
                                    *path_segment = syn::PathSegment {
                                        ident: Ident::new(
                                            &element_type_idents[value],
                                            path_segment.ident.span(),
                                        ),
                                        arguments: syn::PathArguments::None,
                                    };
                                }
                                _ => {}
                            }
                        }
                        syn::PathArguments::Parenthesized(_) => {
                            // Parenthesized arguments are not supported for tuple types
                        }
                    }
                } else {
                    // Option<T> or Option<T<3>> or Arc<Mutex<T<3>>> or Fn(T) -> T
                    self.replace_path_arguments(&mut path_segment.arguments);
                }
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

    fn predicate_to_tuple<'p>(
        &self,
        predicate: &'p WherePredicate,
    ) -> Option<(&'p Ident, &'p Type)> {
        if let WherePredicate::Type(predicate_type) = predicate {
            if let Type::Path(type_path) = &predicate_type.bounded_ty {
                if type_path.qself.is_none() {
                    if let Some(tuple_type_ident) = type_path.path.get_ident() {
                        if let Some(path) = predicate_type
                            .bounds
                            .iter()
                            .filter_map(|bound| match bound {
                                TypeParamBound::Trait(trait_bound) => {
                                    let path = &trait_bound.path;
                                    if path.leading_colon.is_none()
                                        && path.segments.len() == 1
                                        && &path.segments[0].ident == self.trait_ident
                                    {
                                        assert!(predicate_type.bounds.len() == 1);
                                        Some(path)
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .next()
                        {
                            let PathArguments::AngleBracketed(args) = &path.segments[0].arguments
                            else {
                                abort!(path, "expected angle-bracketed type arguments");
                            };
                            if args.args.len() != 1 {
                                abort!(path, "expected 1 type argument");
                            }
                            let Some(GenericArgument::Type(tuple_element_bound)) =
                                args.args.first()
                            else {
                                abort!(path, "expected type or trait");
                            };
                            return Some((tuple_type_ident, tuple_element_bound));
                        }
                    }
                }
            }
        }
        None
    }

    // Replace `<T>` with `<T0, T1,...>` and `where T: Tuple<impl Sized>` with
    // `where T0: Sized, T1: Sized,...`.  Return a mapping from ident to tuple elementtypes, e.g.
    // T -> vec!["T0", "T1",...] or T -> vec!["u32", "u32",...]
    fn process_where_clause(&self, generics: &mut syn::Generics) -> HashMap<Ident, Vec<String>> {
        let mut output = HashMap::new();
        if let Some(mut where_clause) = generics.where_clause.take() {
            let mut predicates =
                syn::punctuated::Punctuated::<WherePredicate, syn::token::Comma>::new();
            let mut tuples_with_generic_bounds = HashSet::new();
            for predicate in where_clause.predicates {
                if let Some((tuple_type_ident, element_constraint)) =
                    self.predicate_to_tuple(&predicate)
                {
                    match element_constraint {
                        Type::ImplTrait(impl_trait) => {
                            // T: Tuple<impl Sized> -> T0: impl Sized, T1: impl Sized
                            let mut element_type_idents = Vec::with_capacity(self.count);
                            for i in 0..self.count {
                                let type_ident = format!("{}{}", tuple_type_ident, i);
                                predicates.push(WherePredicate::Type(PredicateType {
                                    lifetimes: None,
                                    bounded_ty: Type::Path(TypePath {
                                        qself: None,
                                        path: element_type_to_path(
                                            &type_ident,
                                            tuple_type_ident.span(),
                                        ),
                                    }),
                                    colon_token: syn::token::Colon::default(),
                                    bounds: impl_trait.bounds.clone(),
                                }));
                                element_type_idents.push(type_ident);
                            }
                            if output
                                .insert(tuple_type_ident.clone(), element_type_idents)
                                .is_some()
                            {
                                abort!(tuple_type_ident, "type defined twice");
                            }
                            tuples_with_generic_bounds.insert(tuple_type_ident.clone());
                        }
                        Type::Path(type_path) => {
                            // T: Tuple<u8>
                            let mut element_type_idents = Vec::with_capacity(self.count);
                            let type_ident = type_path.path.get_ident().expect("single ident");
                            for _ in 0..self.count {
                                element_type_idents.push(type_ident.to_string());
                            }
                            if output
                                .insert(tuple_type_ident.clone(), element_type_idents)
                                .is_some()
                            {
                                abort!(tuple_type_ident, "type defined twice");
                            }
                        }
                        _ => {
                            abort!(element_constraint, "Unsupported arg type");
                        }
                    }
                } else {
                    predicates.push(predicate);
                }
            }
            if !predicates.is_empty() {
                where_clause.predicates = predicates;
                generics.where_clause = Some(where_clause);
            }
            for generic_param in std::mem::take(&mut generics.params) {
                match generic_param {
                    syn::GenericParam::Type(type_param) => match output.get(&type_param.ident) {
                        Some(element_type_idents) => {
                            if tuples_with_generic_bounds.contains(&type_param.ident) {
                                for type_ident in element_type_idents {
                                    let mut param = type_param.clone();
                                    param.ident = Ident::new(&type_ident, type_param.ident.span());
                                    generics.params.push(syn::GenericParam::Type(param));
                                }
                            }
                        }
                        None => {
                            generics.params.push(syn::GenericParam::Type(type_param));
                        }
                    },
                    p => {
                        generics.params.push(p);
                    }
                }
            }
        }
        output
    }

    fn replace_path_arguments(&self, path_arguments: &mut syn::PathArguments) {
        match path_arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(args) => {
                self.replace_generic_arguments(&mut args.args);
                if args.args.is_empty() {
                    *path_arguments = syn::PathArguments::None;
                }
            }
            syn::PathArguments::Parenthesized(_) => todo!(),
        }
    }

    fn replace_generic_arguments(
        &self,
        args: &mut syn::punctuated::Punctuated<GenericArgument, syn::token::Comma>,
    ) {
        for arg in std::mem::take(args) {
            match arg {
                syn::GenericArgument::Type(generic_type) => {
                    let Some(path_segment) = get_simple_type(&generic_type) else {
                        args.push(syn::GenericArgument::Type(generic_type));
                        continue;
                    };
                    match self.tuples.get(&path_segment.ident) {
                        Some(element_type_idents) => {
                            let replacement_type = Type::Tuple(TypeTuple {
                                paren_token: syn::token::Paren::default(),
                                elems: element_type_idents
                                    .iter()
                                    .map(|name| {
                                        Type::Path(TypePath {
                                            qself: None,
                                            path: element_type_to_path(
                                                name,
                                                path_segment.ident.span(),
                                            ),
                                        })
                                    })
                                    .collect(),
                            });
                            args.push(syn::GenericArgument::Type(replacement_type));
                        }
                        None => {
                            args.push(syn::GenericArgument::Type(generic_type));
                        }
                    }
                }
                p => {
                    args.push(p);
                }
            }
        }
    }

    fn replace_signature(&self, sig: &mut syn::Signature) {
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
}

fn evaluate_usize(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Block(block) => {
            if block.block.stmts.len() == 1 {
                if let Stmt::Expr(expr, _) = &block.block.stmts[0] {
                    return evaluate_usize(expr);
                }
            }
        }
        Expr::Lit(literal) => {
            if let syn::Lit::Int(int) = &literal.lit {
                return int.base10_parse().ok();
            }
        }
        Expr::Binary(binary) => match binary.op {
            syn::BinOp::Add(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_add(right);
                    }
                }
            }
            syn::BinOp::Sub(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_sub(right);
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }
    None
}

fn element_type_to_path(name: &str, span: Span) -> Path {
    syn::Path {
        leading_colon: None,
        segments: [syn::PathSegment {
            ident: Ident::new(name, span),
            arguments: syn::PathArguments::None,
        }]
        .into_iter()
        .collect(),
    }
}
