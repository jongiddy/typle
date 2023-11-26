//! A proc macro to generate multiple items for tuples. Example code:
//!
//! ```
//! use typle::typle;
//!
//! struct MyStruct<T> {
//!     pub t: T,
//! }
//!
//! #[typle(Tuple for 1..=3)]
//! impl<T> MyStruct<T>
//! where
//!     T: Tuple<u32>,
//! {
//!     fn max(&self) -> Option<u32> {
//!         let mut max = self.t[[0]];
//!         for typle_const!(i) in 1..T::LEN {
//!             if self.t[[i]] > max {
//!                 max = self.t[[i]];
//!             }
//!         }
//!         Some(max)
//!     }
//! }
//! ```
//!
//! This code creates implementations for 1-, 2-, and 3-tuples:
//!
//! ```
//! # struct MyStruct<T> {t: T}
//! impl MyStruct<(u32,)> {
//!     fn max(&self) -> Option<u32> {
//!         let mut max = self.t.0;
//!         {}
//!         Some(max)
//!     }
//! }
//! impl MyStruct<(u32, u32)> {
//!     fn max(&self) -> Option<u32> {
//!         let mut max = self.t.0;
//!         {
//!             {
//!                 if self.t.1 > max {
//!                     max = self.t.1;
//!                 }
//!             }
//!         }
//!         Some(max)
//!     }
//! }
//! impl MyStruct<(u32, u32, u32)> {
//!     fn max(&self) -> Option<u32> {
//!         let mut max = self.t.0;
//!         {
//!             {
//!                 if self.t.1 > max {
//!                     max = self.t.1;
//!                 }
//!             }
//!             {
//!                 if self.t.2 > max {
//!                     max = self.t.2;
//!                 }
//!             }
//!         }
//!         Some(max)
//!     }
//! }
//! ```
//!
//! The macro arguments `Tuple for 1..=3` consist of an identifier `Tuple`, to use as a pseudo-trait
//! in the `where` clause, and a range of tuple lengths `1..=3` for which the item will be created.
//!
//! If the `where` clause constrains a generic type using the pseudo-trait then the generic type
//! must be a tuple with a length within the macro range and where each element is constrained by
//! the argument to the trait. The element constraint can either be an explicit type (`Tuple<u32>`)
//! or other traits (`Tuple<impl Clone + Debug>`).
//!
//! The elements of a tuple can be iterated over using a `for` loop with an iteration variable
//! enclosed in `typle_const!` macro. As shown above, this executes the `for` loop body for each
//! element in the tuple.
//!
//! Note that `T::LEN` is a special path that is available on each tuple type to provide the number
//! of elements.
//!
//! Tuple elements are referenced using a double-bracketed index and a constant value, including an
//! index created using `typle_const!`. Hence `self.t[[i]]` will be replaced by
//! `self.t.0, self.t.1,...`.
//!
//! Other features include using `T<i>` to name element types, a `typle_expand!` macro to perform
//! element-by-element operations, support for enums with a `typle_variants!()` macro, and
//! constant-if for conditional compilation based on constant values including a `typle_const!`
//! iteration variable. See the [README](https://github.com/jongiddy/typle#readme), and
//! [this example](https://github.com/jongiddy/typle/blob/main/tests/expand/enum.rs) and its
//! [expanded form](https://github.com/jongiddy/typle/blob/main/tests/expand/enum.expanded.rs).
//!
//! Also, see how `typle` is used in the [`hefty` crate](https://github.com/jongiddy/hefty/blob/main/src/tuple.rs).

use std::collections::{HashMap, HashSet};

use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, ToTokens};
use syn::spanned::Spanned as _;
use syn::{
    Block, Expr, ExprBlock, ExprField, ExprLit, ExprParen, ExprPath, ExprRange, ExprTuple, Fields,
    FieldsNamed, FieldsUnnamed, GenericArgument, Item, ItemEnum, ItemImpl, ItemStruct, ItemType,
    LitInt, Macro, Pat, Path, PathArguments, PathSegment, PredicateType, QSelf, RangeLimits, Stmt,
    Type, TypeMacro, TypeParamBound, TypeParen, TypePath, TypeTuple, Variant, WherePredicate,
};

#[doc(hidden)]
#[proc_macro]
pub fn typle_identity(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    item
}

#[doc(hidden)]
#[proc_macro_error]
#[proc_macro_attribute]
pub fn typle(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let iteration_trait = parse_args(TokenStream::from(args));

    let Ok(item) = syn::parse::<Item>(item) else {
        abort_call_site!("unsupported tokens");
    };

    let mut output = Vec::new();

    output.extend(iteration_trait.process_item(item));

    output
        .into_iter()
        .map(Item::into_token_stream)
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
    match args_iter.next() {
        Some(TokenTree::Ident(for_ident)) if for_ident == "for" => {}
        _ => {
            abort_call_site!("expected 'for'");
        }
    }
    let rest = args_iter.collect();
    let range = syn::parse2::<ExprRange>(rest).expect("expect range");
    let min = range
        .start
        .as_ref()
        .map(|expr| evaluate_usize(&expr).unwrap_or_else(|| abort!(expr, "invalid start")))
        .unwrap_or_else(|| abort!(range, "range cannot be unbounded at start"));
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
    fn process_item(&self, input: Item) -> Vec<Item> {
        let mut output = Vec::new();
        match input {
            Item::Const(_) => abort!(input, "Const unsupported"),
            Item::Enum(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            mode: SpecificMode::Tuple,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item_struct = specific.process_enum(&item);
                        output.push(Item::Enum(item_struct));
                    }
                } else {
                    output.push(Item::Enum(item));
                }
            }
            Item::ExternCrate(_) => abort!(input, "ExternCrate unsupported"),
            Item::Fn(_) => abort!(input, "Fn unsupported"),
            Item::ForeignMod(_) => abort!(input, "ForeignMod unsupported"),
            Item::Impl(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            mode: SpecificMode::Tuple,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item = specific.process_impl(&item);
                        output.push(Item::Impl(item));
                    }
                } else {
                    output.push(Item::Impl(item));
                }
            }
            Item::Macro(_) => abort!(input, "Macro unsupported"),
            Item::Mod(_) => abort!(input, "Mod unsupported"),
            Item::Static(_) => abort!(input, "Static unsupported"),
            Item::Struct(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            mode: SpecificMode::Tuple,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item_struct = specific.process_struct(&item);
                        output.push(Item::Struct(item_struct));
                    }
                } else {
                    output.push(Item::Struct(item));
                }
            }
            Item::Trait(_) => abort!(input, "Trait unsupported"),
            Item::TraitAlias(_) => abort!(input, "TraitAlias unsupported"),
            Item::Type(item) => {
                if self.generics_contain_trait(&item.generics) {
                    for count in self.min..=self.max {
                        let mut specific = SpecificContext {
                            trait_ident: &self.ident,
                            count,
                            mode: SpecificMode::Tuple,
                            constants: HashMap::new(),
                            tuples: HashMap::new(),
                        };
                        let item = specific.process_type(&item);
                        output.push(Item::Type(item));
                    }
                } else {
                    output.push(Item::Type(item));
                }
            }
            Item::Union(_) => abort!(input, "Union unsupported"),
            Item::Use(_) => abort!(input, "Use unsupported"),
            Item::Verbatim(_) => abort!(input, "Verbatim unsupported"),
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

#[derive(Clone)]
enum SpecificMode {
    // T expands to (T0, T1,...)
    Tuple,
    // T expands to element type, e.g T3
    Index(usize),
}

#[derive(Clone)]
struct SpecificContext<'a> {
    trait_ident: &'a Ident,
    count: usize,
    mode: SpecificMode,
    constants: HashMap<Ident, usize>,
    tuples: HashMap<Ident, Vec<String>>,
}

impl<'a> SpecificContext<'a> {
    fn process_enum(&mut self, item: &ItemEnum) -> ItemEnum {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            item.ident = format_ident!("{}{}", item.ident, self.count);
            for mut variant in std::mem::take(&mut item.variants) {
                if let Some((_, discriminant)) = &mut variant.discriminant {
                    if let Expr::Macro(r#macro) = discriminant {
                        if let Some(ident) = r#macro.mac.path.get_ident() {
                            if ident == "typle_variants" {
                                for index in 0..self.count {
                                    let context = SpecificContext {
                                        mode: SpecificMode::Index(index),
                                        ..self.clone()
                                    };
                                    let mut fields = variant.fields.clone();
                                    match &mut fields {
                                        Fields::Named(FieldsNamed { named: fields, .. })
                                        | Fields::Unnamed(FieldsUnnamed {
                                            unnamed: fields, ..
                                        }) => {
                                            for field in fields {
                                                context.replace_type(&mut field.ty);
                                            }
                                        }
                                        Fields::Unit => {}
                                    }
                                    let element = Variant {
                                        attrs: variant.attrs.clone(),
                                        ident: format_ident!("{}{}", &variant.ident, index),
                                        fields,
                                        discriminant: None,
                                    };
                                    item.variants.push(element);
                                }
                                continue;
                            }
                        }
                    }
                    self.replace_expr(discriminant);
                }
                match &mut variant.fields {
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
                item.variants.push(variant);
            }
        }
        item
    }

    fn process_struct(&mut self, item: &ItemStruct) -> ItemStruct {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            item.ident = format_ident!("{}{}", item.ident, self.count);
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

    fn process_impl(&mut self, item: &ItemImpl) -> ItemImpl {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            // Replace types in type
            // S<T> -> S<(T0, T1>)
            self.replace_type(&mut *item.self_ty);
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

    // type TSO1<T0> where T0: Extract = (Option<T0::Output>);
    // type TSO2<T0, T1> where T0: Extract, T1: Extract = (Option<T0::Output>, Option<T1::Output>);

    // pub enum TupleSequenceState2<T0, T1>
    // where
    //     T0: Extract,
    //     T1: Extract,
    // {
    //     S0(Option<T0::State>, TSO2<T0, T1>),
    //     S1(Option<T1::State>, TSO2<T0, T1>),
    // }

    // #[typle(Tuple for 1..=12)]
    // type TSO<T> where T: Extract = typle_expand!(Option<T::Output>);
    fn process_type(&mut self, item: &ItemType) -> ItemType {
        let mut item = item.clone();
        self.tuples = self.process_where_clause(&mut item.generics);
        if !self.tuples.is_empty() {
            item.ident = format_ident!("{}{}", item.ident, self.count);
            self.replace_type(&mut item.ty);
        }
        item
    }

    fn replace_block(&self, block: &mut Block) {
        for stmt in &mut block.stmts {
            match stmt {
                syn::Stmt::Local(local) => {
                    self.replace_pat(&mut local.pat);
                    if let Some(init) = &mut local.init {
                        self.replace_expr(&mut init.expr);
                        if let Some((_, diverge)) = &mut init.diverge {
                            self.replace_expr(diverge);
                        }
                    }
                }
                syn::Stmt::Item(item) => {
                    self.replace_item(item);
                }
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
                self.replace_expr(&mut for_loop.expr);
                // Check for typle_const!(i)
                if let Pat::Macro(pat_macro) = &mut *for_loop.pat {
                    if let Some(macro_ident) = pat_macro.mac.path.get_ident() {
                        if macro_ident == "typle_const" {
                            let span = pat_macro.mac.tokens.span();
                            let mut tokens = std::mem::take(&mut pat_macro.mac.tokens).into_iter();
                            let Some(TokenTree::Ident(pat_ident)) = tokens.next() else {
                                abort!(span, "unexpected token in typle_const");
                            };
                            if let Some(tt) = tokens.next() {
                                abort!(tt.span(), "unexpected token in typle_const");
                            };
                            let Expr::Range(expr_range) = &*for_loop.expr else {
                                abort!(for_loop.expr.span(), "expected range");
                            };
                            let (Some(start_expr), Some(end_expr)) =
                                (&expr_range.start, &expr_range.end)
                            else {
                                abort!(for_loop.expr.span(), "expected explicit bounds");
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
                            if start > end || end > self.count {
                                abort!(for_loop.expr.span(), "expected sub-range of tuple size");
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
                            *expr = Expr::Block(ExprBlock {
                                attrs: std::mem::take(&mut for_loop.attrs),
                                label: for_loop.label.take(),
                                block: Block {
                                    brace_token: syn::token::Brace::default(),
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
                            let Ok(mut cond) = syn::parse2::<Expr>(tokens) else {
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
                            } else {
                                match &r#if.else_branch {
                                    Some((_, branch)) => {
                                        *expr = branch.as_ref().clone();
                                        self.replace_expr(expr);
                                    }
                                    None => {
                                        *expr = Expr::Block(ExprBlock {
                                            attrs: std::mem::take(&mut r#if.attrs),
                                            label: None,
                                            block: Block {
                                                brace_token: syn::token::Brace::default(),
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
                        dot_token: syn::token::Dot::default(),
                        member: syn::Member::Unnamed(syn::Index {
                            index: i as u32,
                            span: index.index.span(),
                        }),
                    });
                } else {
                    self.replace_expr(&mut index.index);
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
                    // <T as Default>::default()
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
                                return;
                            }
                            Some(second) if second.ident == "INDEX" => {
                                // T::INDEX
                                match self.mode {
                                    SpecificMode::Tuple => {
                                        abort!(second.span(), "INDEX not valid in this context");
                                    }
                                    SpecificMode::Index(index) => {
                                        *expr = Expr::Lit(ExprLit {
                                            attrs: std::mem::take(&mut path.attrs),
                                            lit: syn::Lit::Int(LitInt::new(
                                                &index.to_string(),
                                                path.span(),
                                            )),
                                        });
                                        return;
                                    }
                                }
                            }
                            Some(second) => {
                                // T::clone(&t) -> <(T0, T1)>::clone(&t)
                                // todo: T::<0>::default() -> T0::default()
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
                                match self.mode {
                                    SpecificMode::Tuple => {
                                        // T -> (T0, T1,...)
                                        let span = path.span();
                                        *expr = Expr::Tuple(ExprTuple {
                                            attrs: Vec::new(),
                                            paren_token: syn::token::Paren::default(),
                                            elems: element_type_idents
                                                .iter()
                                                .map(|name| {
                                                    Expr::Path(ExprPath {
                                                        attrs: Vec::new(),
                                                        qself: None,
                                                        path: element_type_to_path(name, span),
                                                    })
                                                })
                                                .collect(),
                                        });
                                        return;
                                    }
                                    SpecificMode::Index(index) => {
                                        // T -> T0
                                        path.path = element_type_to_path(
                                            &element_type_idents[index],
                                            first.span(),
                                        );
                                    }
                                }
                            }
                        }
                    } else if let Some(value) = self.constants.get(&first.ident) {
                        *expr = Expr::Lit(ExprLit {
                            attrs: std::mem::take(&mut path.attrs),
                            lit: syn::Lit::Int(LitInt::new(&value.to_string(), first.ident.span())),
                        });
                        return;
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
                                            syn::GenericArgument::Type(mut generic_type) => {
                                                self.replace_type(&mut generic_type);
                                                args.args
                                                    .push(syn::GenericArgument::Type(generic_type));
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
                self.replace_path(&mut r#struct.path, |path_segment| {
                    match &path_segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(_) => todo!(),
                        PathArguments::Parenthesized(_) => {}
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
    fn replace_item(&self, item: &mut Item) {
        match item {
            Item::Const(r#const) => {
                self.replace_type(&mut r#const.ty);
                self.replace_expr(&mut r#const.expr);
            }
            Item::Enum(r#enum) => {
                for variant in &mut r#enum.variants {
                    self.replace_fields(&mut variant.fields);
                }
            }
            Item::ExternCrate(_) => abort!(item, "ExternCrate unsupported"),
            Item::Fn(function) => {
                self.replace_signature(&mut function.sig);
                self.replace_block(&mut function.block);
            }
            Item::ForeignMod(_) => abort!(item, "ForeignMod unsupported"),
            Item::Impl(_) => abort!(item, "Impl unsupported"),
            Item::Macro(_) => abort!(item, "Macro unsupported"),
            Item::Mod(_) => abort!(item, "Mod unsupported"),
            Item::Static(_) => abort!(item, "Static unsupported"),
            Item::Struct(_) => abort!(item, "Struct unsupported"),
            Item::Trait(_) => abort!(item, "Trait unsupported"),
            Item::TraitAlias(_) => abort!(item, "TraitAlias unsupported"),
            Item::Type(_) => abort!(item, "Type unsupported"),
            Item::Union(_) => abort!(item, "Union unsupported"),
            Item::Use(_) => abort!(item, "Use unsupported"),
            Item::Verbatim(_) => abort!(item, "Verbatim unsupported"),
            _ => todo!(),
        }
    }
    fn replace_macro(&self, r#macro: &mut Macro) {
        // typle_expand!(Option<T>) -> (Option<T0>, Option<T1>)
        // typle_expand!(T::default()) -> (T0::default(), T1::default())
        // as opposed to
        // Option<T> -> Option<(T0, T1)>
        // T::default() -> <(T0, T1)>::default()
        // typle_expand!((T, T::<0>)) -> ((T0, T0), (T1, T0))
        if let Some(macro_name) = r#macro.path.get_ident() {
            if macro_name == "typle_expand" {
                let default_span = macro_name.span();
                r#macro.path = Path {
                    leading_colon: None,
                    segments: std::module_path!()
                        .split("::")
                        .chain(std::iter::once("typle_identity"))
                        .map(|name| PathSegment {
                            ident: Ident::new(name, macro_name.span()),
                            arguments: PathArguments::None,
                        })
                        .collect(),
                };
                let tokens = std::mem::take(&mut r#macro.tokens);
                if let Ok(expr) = syn::parse2::<Expr>(tokens.clone()) {
                    let mut tuple = ExprTuple {
                        paren_token: syn::token::Paren::default(),
                        elems: syn::punctuated::Punctuated::new(),
                        attrs: Vec::new(),
                    };
                    for index in 0..self.count {
                        let context = SpecificContext {
                            mode: SpecificMode::Index(index),
                            ..self.clone()
                        };
                        let mut element = expr.clone();
                        context.replace_expr(&mut element);
                        tuple.elems.push(element);
                    }
                    r#macro.tokens = tuple.into_token_stream();
                    return;
                }
                if let Ok(r#type) = syn::parse2::<Type>(tokens) {
                    let mut tuple = TypeTuple {
                        paren_token: syn::token::Paren::default(),
                        elems: syn::punctuated::Punctuated::new(),
                    };
                    for index in 0..self.count {
                        let context = SpecificContext {
                            mode: SpecificMode::Index(index),
                            ..self.clone()
                        };
                        let mut element = r#type.clone();
                        context.replace_type(&mut element);
                        tuple.elems.push(element);
                    }
                    r#macro.tokens = tuple.into_token_stream();
                    return;
                }
                abort!(default_span, "expected type or expression");
            }
        }
    }

    fn replace_pat(&self, pat: &mut Pat) {
        match pat {
            Pat::Const(_) => abort!(pat.span(), "Const unsupported"),
            Pat::Ident(_) => {}
            Pat::Lit(_) => abort!(pat.span(), "Lit unsupported"),
            Pat::Macro(_) => abort!(pat.span(), "Macro unsupported"),
            Pat::Or(_) => abort!(pat.span(), "Or unsupported"),
            Pat::Paren(_) => abort!(pat.span(), "Paren unsupported"),
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
                                        syn::GenericArgument::Type(mut generic_type) => {
                                            self.replace_type(&mut generic_type);
                                            args.args
                                                .push(syn::GenericArgument::Type(generic_type));
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
            Pat::Range(_) => abort!(pat.span(), "Range unsupported"),
            Pat::Reference(_) => abort!(pat.span(), "Reference unsupported"),
            Pat::Rest(_) => abort!(pat.span(), "Rest unsupported"),
            Pat::Slice(_) => abort!(pat.span(), "Slice unsupported"),
            Pat::Struct(_) => abort!(pat.span(), "Struct unsupported"),
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
                                        syn::GenericArgument::Type(mut generic_type) => {
                                            self.replace_type(&mut generic_type);
                                            args.args
                                                .push(syn::GenericArgument::Type(generic_type));
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
            Pat::Verbatim(_) => abort!(pat.span(), "Verbatim unsupported"),
            Pat::Wild(_) => abort!(pat.span(), "Wild unsupported"),
            _ => {}
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
                let mut first = true;
                for mut path_segment in std::mem::take(&mut path.path.segments) {
                    if first {
                        if let Some(element_type_names) = self.tuples.get(&path_segment.ident) {
                            match &mut path_segment.arguments {
                                PathArguments::None => {
                                    match self.mode {
                                        SpecificMode::Tuple => {
                                            // T -> (T0, T1,...)
                                            let span = path_segment.ident.span();
                                            *ty = Type::Tuple(TypeTuple {
                                                paren_token: syn::token::Paren::default(),
                                                elems: element_type_names
                                                    .iter()
                                                    .map(|name| {
                                                        Type::Path(TypePath {
                                                            qself: None,
                                                            path: element_type_to_path(name, span),
                                                        })
                                                    })
                                                    .collect(),
                                            });
                                            return;
                                        }
                                        SpecificMode::Index(index) => {
                                            // T -> T0, T::State -> T0::State
                                            path_segment.ident = Ident::new(
                                                &element_type_names[index],
                                                path_segment.span(),
                                            );
                                        }
                                    }
                                }
                                PathArguments::AngleBracketed(args) => {
                                    // T<3> or T<i> where i comes from constants
                                    if args.args.len() != 1 {
                                        abort!(path_segment, "expected one type parameter");
                                    }
                                    match args.args.first_mut() {
                                        Some(syn::GenericArgument::Type(expr)) => {
                                            match expr {
                                                Type::Path(path) => {
                                                    if let Some(ident) = path.path.get_ident() {
                                                        if let Some(value) =
                                                            self.constants.get(ident)
                                                        {
                                                            // T<i>
                                                            path_segment = syn::PathSegment {
                                                                ident: Ident::new(
                                                                    &element_type_names[*value],
                                                                    path_segment.ident.span(),
                                                                ),
                                                                arguments: PathArguments::None,
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
                                            let Some(value) = evaluate_usize(expr) else {
                                                abort!(expr, "unsupported tuple type index");
                                            };
                                            path_segment = syn::PathSegment {
                                                ident: Ident::new(
                                                    &element_type_names[value],
                                                    path_segment.ident.span(),
                                                ),
                                                arguments: PathArguments::None,
                                            };
                                        }
                                        _ => {}
                                    }
                                }
                                PathArguments::Parenthesized(_) => {
                                    // Parenthesized arguments are not supported for tuple types
                                    abort!(
                                        path_segment.arguments.span(),
                                        "unexpected patenthesized args"
                                    );
                                }
                            }
                        }
                    } else {
                        first = false;
                    }
                    // Replace arguments
                    match &mut path_segment.arguments {
                        PathArguments::None => {}
                        PathArguments::AngleBracketed(args) => {
                            let mut star = None;
                            // std::option::Option<T> -> std::option::Option<(T0, T1,...)
                            // std::option::Option<T<3>> -> std::option::Option<T3>
                            // std::option::Option<*T> -> std::option::Option2<T0, T1>
                            // Enum::Variant<(i)> -> Enum::Variant0
                            for arg in std::mem::take(&mut args.args) {
                                match arg {
                                    syn::GenericArgument::Type(Type::Paren(TypeParen {
                                        elem,
                                        ..
                                    })) => {
                                        if let Type::Path(TypePath { path, .. }) = &*elem {
                                            if let Some(ident) = path.get_ident() {
                                                if let Some(element_names) = self.tuples.get(ident)
                                                {
                                                    star = Some(self.count);
                                                    for name in element_names {
                                                        args.args.push(syn::GenericArgument::Type(
                                                            Type::Path(TypePath {
                                                                qself: None,
                                                                path: element_type_to_path(
                                                                    name,
                                                                    path.span(),
                                                                ),
                                                            }),
                                                        ));
                                                    }
                                                } else if let Some(value) =
                                                    self.constants.get(ident)
                                                {
                                                    star = Some(*value);
                                                }
                                            }
                                        }
                                    }
                                    syn::GenericArgument::Const(Expr::Paren(ExprParen {
                                        mut expr,
                                        ..
                                    })) => {
                                        self.replace_expr(&mut expr);
                                        if let Some(value) = evaluate_usize(&expr) {
                                            star = Some(value);
                                        }
                                    }
                                    syn::GenericArgument::Type(mut generic_type) => {
                                        self.replace_type(&mut generic_type);
                                        args.args.push(syn::GenericArgument::Type(generic_type));
                                    }
                                    p => {
                                        args.args.push(p);
                                    }
                                }
                            }
                            if let Some(value) = star {
                                path_segment.ident =
                                    format_ident!("{}{}", path_segment.ident, value);
                            }
                            if args.args.is_empty() {
                                path_segment.arguments = PathArguments::None;
                            }
                        }
                        PathArguments::Parenthesized(_) => todo!(),
                    }
                    path.path.segments.push(path_segment);
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
                            // T: Tuple<u8> - todo: support T: Tuple<Option<u8>>
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

    fn replace_generic_arguments(
        &self,
        args: &mut syn::punctuated::Punctuated<GenericArgument, syn::token::Comma>,
    ) {
        for arg in std::mem::take(args) {
            match arg {
                syn::GenericArgument::Type(mut generic_type) => {
                    self.replace_type(&mut generic_type);
                    args.push(syn::GenericArgument::Type(generic_type));
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

fn evaluate_bool(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Array(_) => todo!(),
        Expr::Assign(_) => todo!(),
        Expr::Async(_) => todo!(),
        Expr::Await(_) => todo!(),
        Expr::Binary(binary) => match binary.op {
            syn::BinOp::Eq(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Some(left == right);
                    }
                }
            }
            syn::BinOp::Ne(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Some(left != right);
                    }
                }
            }
            _ => {}
        },
        Expr::Block(_) => todo!(),
        Expr::Break(_) => todo!(),
        Expr::Call(_) => todo!(),
        Expr::Cast(_) => todo!(),
        Expr::Closure(_) => todo!(),
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(_) => todo!(),
        Expr::ForLoop(_) => todo!(),
        Expr::Group(_) => todo!(),
        Expr::If(_) => todo!(),
        Expr::Index(_) => todo!(),
        Expr::Infer(_) => todo!(),
        Expr::Let(_) => todo!(),
        Expr::Lit(_) => todo!(),
        Expr::Loop(_) => todo!(),
        Expr::Macro(_) => todo!(),
        Expr::Match(_) => todo!(),
        Expr::MethodCall(_) => todo!(),
        Expr::Paren(_) => todo!(),
        Expr::Path(_) => todo!(),
        Expr::Range(_) => todo!(),
        Expr::Reference(_) => todo!(),
        Expr::Repeat(_) => todo!(),
        Expr::Return(_) => todo!(),
        Expr::Struct(_) => todo!(),
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Unary(_) => todo!(),
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
    None
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
            arguments: PathArguments::None,
        }]
        .into_iter()
        .collect(),
    }
}
