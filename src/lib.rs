//! The `typle!` macro generates code for multiple tuple lengths:
//!
//! ```rust
//! use typle::typle;
//!
//! struct MyStruct<T> {
//!     t: T,
//! }
//!
//! #[typle(Tuple for 0..=3)]
//! impl<T: Tuple> From<T> for MyStruct<T>
//! {
//!     fn from(t: T) -> Self {
//!         MyStruct { t }
//!     }
//! }
//! ```
//!
//! This generates implementations of the `From` trait for tuples with 0 to 3 components:
//! ```rust
//! # struct MyStruct<T> {
//! #     t: T,
//! # }
//! impl From<()> for MyStruct<()> {
//!     fn from(t: ()) -> Self {
//!         MyStruct { t }
//!     }
//! }
//!
//! impl<T0> From<(T0,)> for MyStruct<(T0,)> {
//!     fn from(t: (T0,)) -> Self {
//!         MyStruct { t }
//!     }
//! }
//!
//! impl<T0, T1> From<(T0, T1)> for MyStruct<(T0, T1)> {
//!     fn from(t: (T0, T1)) -> Self {
//!         MyStruct { t }
//!     }
//! }
//!
//! impl<T0, T1, T2> From<(T0, T1, T2)> for MyStruct<(T0, T1, T2)> {
//!     fn from(t: (T0, T1, T2)) -> Self {
//!         MyStruct { t }
//!     }
//! }
//! ```
//!
//! Inside `typle` code, individual components of a tuple can be selected using
//! `<{i}>` for types and `[[i]]` for values. The value `i` must be a constant.
//!
//! The [`typle_for!`] macro creates a new variable-length tuple type or value.
//! Inside the macro the iteration variable is a constant for each component.
//!
//! ```rust
//! # use typle::typle;
//! // Split off the first component
//! #[typle(Tuple for 1..=3)]
//! fn split<T: Tuple>(
//!     t: T  // t: (T0, T1, T2,...)
//! ) -> (T<0>, typle_for!(i in 1.. => T<{i}>))   // (T0, (T1, T2,...))
//! {
//!     (t[[0]], typle_for!(i in 1.. => t[[i]]))  // (t.0, (t.1, t.2,...))
//! }
//!
//! assert_eq!(split((1, 2, 3)), (1, (2, 3)));
//! assert_eq!(split((2, 3)), (2, (3,)));
//! assert_eq!(split((3,)), (3, ()));
//! ```

//! Specify constraints on the tuple components using one of the following
//! forms. Except for the first form, these constraints can only appear in the
//! `where` clause.
//! - `T: Tuple<C>` - each component of the tuple has type `C`
//! - `T<_>: Copy` - each component of the tuple implements `Copy`
//! - `T<0>: Copy` - the first component of the tuple implements `Copy`
//! - `T<{1..=2}>: Copy` - the second and third components implement `Copy`
//! - `typle_bound!` - the most general way to bound components,
//! allowing the iteration variable to be used in the trait bounds, as shown below:
//!
//! ```rust
//! # use typle::typle;
//! use std::ops::Mul;
//!
//! // Return the product of the components of two tuples
//! #[typle(Tuple for 0..=3)]
//! fn multiply<S: Tuple, T: Tuple>(
//!     s: S,  // s: (S0,...)
//!     t: T,  // t: (T0,...)
//! ) -> typle_for!(i in .. => <S<{i}> as Mul<T<{i}>>>::Output)  // (<S0 as Mul<T0>>::Output,...)
//! where
//!     typle_bound!(i in .. => S<{i}>): Mul<T<{i}>>,  // S0: Mul<T0>,...
//! {
//!     typle_for!(i in .. => s[[i]] * t[[i]])  // (s.0 * t.0,...)
//! }
//!
//! assert_eq!(
//!     multiply((std::time::Duration::from_secs(5), 2), (4, 3)),
//!     (std::time::Duration::from_secs(20), 6)
//! )
//! ```
//!
//! The associated constant `LEN` provides the length of the tuple in each
//! generated item.
//!
//! Use the `typle_const!` macro to perform const-for, iterating over a block of
//! statements.
//!
//! ```rust
//! # use typle::typle;
//! # struct MyStruct<T> {
//! #     t: T,
//! # }
//! # impl<T0, T1, T2> From<(T0, T1, T2)> for MyStruct<(T0, T1, T2)> {
//! #     fn from(t: (T0, T1, T2)) -> Self {
//! #         MyStruct { t }
//! #     }
//! # }
//! #[typle(Tuple for 1..=3)]
//! impl<T, C> MyStruct<T>
//! where
//!     T: Tuple<C>,
//!     C: for<'a> std::ops::AddAssign<&'a C> + Default,
//! {
//!     // Return the sums of all even positions and all odd positions, using
//!     // 0-indexing so the first position is even and second position is odd.
//!     fn interleave(&self) -> (C, C) {
//!         let mut even_odd = (C::default(), C::default());
//!         for typle_const!(i) in 0..T::LEN {
//!             even_odd[[i % 2]] += &self.t[[i]];
//!         }
//!         even_odd
//!     }
//! }
//!
//! let m = MyStruct::from((3, 9, 11));
//! assert_eq!(m.interleave(), (14, 9));
//! ```
//!
//! The next example is simplified from code in the
//! [`hefty` crate](https://github.com/jongiddy/hefty/blob/main/src/tuple.rs) and
//! demonstrates the use of `typle`` with `enum`s.
//!
//! The [`typle_variant!`] macro creates multiple enum variants by looping
//! similarly to `typle_for!`.
//!
//! Typled `enum`s and `struct`s require a separate identifier for each tuple
//! length. The `typle` macro adds the tuple length to their original name. For
//! example `enum TupleSequenceState<T>` expands to `enum TupleSequenceState3<T0, T1, T2>`
//! for 3-tuples. When referring to these types from other typled items, use
//! `TupleSequenceState<T<{..}>>`.
//!
//! Use the `typle_index!` macro to concatenate a number to an identifier. For
//! example `S::<typle_index!(3)>` becomes the identifer `S3`.
//!
//! The `typle_const!` macro supports const-if on an expression that evaluates
//! to a `bool`. const-if allows branches that do not compile, as long as they are
//! `false` at compile-time. For example, this code compiles when `i + 1 == T::LEN`
//! even though the identifier `S::<typle_index!(T::LEN)>` (`S3` for 3-tuples) is not
//! defined.
//!
//! ```rust
//! use typle::typle;
//!
//! #[typle(Tuple for 1..=3)]
//! mod tuple {
//!     pub trait Extract {
//!         type State;
//!         type Output;
//!
//!         fn extract(&self, state: Option<Self::State>) -> Self::Output;
//!     }
//!
//!     pub enum TupleSequenceState<T>
//!     where
//!         T: Tuple,
//!         T<_>: Extract,
//!     {
//!         S = typle_variant!(i in .. =>
//!             typle_for!(j in ..i => T::<{j}>::Output),
//!             Option<T<{i}>::State>
//!         ),
//!     }
//!
//!     pub struct TupleSequence<T> {
//!         tuple: T,
//!     }
//!
//!     impl<T> Extract for TupleSequence<T>
//!     where
//!         T: Tuple,
//!         T<_>: Extract,
//!     {
//!         type State = TupleSequenceState<T<{..}>>;
//!         type Output = typle_for!(i in .. => T<{i}>::Output);
//!
//!         fn extract(&self, state: Option<Self::State>) -> Self::Output {
//!             #[allow(unused_mut)]  // For LEN = 1 `state` is never mutated
//!             let mut state = state.unwrap_or(Self::State::S::<typle_index!(0)>((), None));
//!             for typle_const!(i) in 0..T::LEN {
//!                 // For LEN = 1 there is only one state and the initial `output` variable is unused
//!                 #[allow(irrefutable_let_patterns, unused_variables)]
//!                 if let Self::State::S::<typle_index!(i)>(output, inner_state) = state {
//!                     let matched = self.tuple[[i]].extract(inner_state);
//!                     let output = typle_for!(j in ..=i =>
//!                         if typle_const!(j != i) { output[[j]] } else { matched }
//!                     );
//!                     if typle_const!(i + 1 == T::LEN) {
//!                         return output;
//!                     } else {
//!                         state = Self::State::S::<typle_index!(i + 1)>(output, None);
//!                     }
//!                 }
//!             }
//!             unreachable!();
//!         }
//!     }
//! }
//! ```
//!
//! Generated implementation for 3-tuples:
//! ```rust
//! # pub trait Extract {
//! #     type State;
//! #     type Output;
//! #     fn extract(&self, state: Option<Self::State>) -> Self::Output;
//! # }
//! # pub struct TupleSequence<T> {
//! #     tuple: T,
//! # }
//! pub enum TupleSequenceState3<T0, T1, T2>
//! where
//!     T0: Extract,
//!     T1: Extract,
//!     T2: Extract,
//! {
//!     S0((), Option<<T0>::State>),
//!     S1((<T0>::Output,), Option<<T1>::State>),
//!     S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
//! }
//!
//! impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
//! where
//!     T0: Extract,
//!     T1: Extract,
//!     T2: Extract,
//! {
//!     type State = TupleSequenceState3<T0, T1, T2>;
//!     type Output = (<T0>::Output, <T1>::Output, <T2>::Output);
//!     fn extract(&self, state: Option<Self::State>) -> Self::Output {
//!         #[allow(unused_mut)]
//!         let mut state = state.unwrap_or(Self::State::S0((), None));
//!         {
//!             {
//!                 #[allow(irrefutable_let_patterns, unused_variables)]
//!                 if let Self::State::S0(output, inner_state) = state {
//!                     let matched = self.tuple.0.extract(inner_state);
//!                     let output = ({ matched },);
//!                     {
//!                         state = Self::State::S1(output, None);
//!                     }
//!                 }
//!             }
//!             {
//!                 #[allow(irrefutable_let_patterns, unused_variables)]
//!                 if let Self::State::S1(output, inner_state) = state {
//!                     let matched = self.tuple.1.extract(inner_state);
//!                     let output = ({ output.0 }, { matched });
//!                     {
//!                         state = Self::State::S2(output, None);
//!                     }
//!                 }
//!             }
//!             {
//!                 #[allow(irrefutable_let_patterns, unused_variables)]
//!                 if let Self::State::S2(output, inner_state) = state {
//!                     let matched = self.tuple.2.extract(inner_state);
//!                     let output = ({ output.0 }, { output.1 }, { matched });
//!                     {
//!                         return output;
//!                     }
//!                 }
//!             }
//!             ()
//!         }
//!         unreachable!();
//!     }
//! }
//! ```
//!
//! # Limitations
//!
//! - The typle trait (`Tuple` in the examples) cannot be combined with other constraints. To
//! support `?Sized` tuples constrain the last component using `T<{T::LEN - 1}>: ?Sized`.
//! - Standalone `async` and `unsafe` functions are not supported.
//! - Standalone functions require explicit lifetimes on references
//! ```rust ignore
//! #[typle(Tuple for 1..=3)]
//! pub fn hash<'a, T, S: Hasher>(tuple: &'a T, state: &'a mut S)
//! where
//!     T: Tuple,
//!     T<_>: Hash,
//!     T<{T::LEN - 1}>: ?Sized,
//! {
//!     for typle_const!(i) in 0..T::LEN {
//!         tuple[[i]].hash(state);
//!     }
//! }
//! ```
//! - The `const` values used to index tuples can only be created in const-for expressions or other
//! typle macros. Other `const` values cannot be used.
//! ```rust ignore
//! const i: usize = 3;
//! let a = self.tuple[[i]];  // compile error
//! ```
//! - Shadowing of const variables introduced using typle macros is not supported. For example, in:
//! ```rust ignore
//! for typle_const!(i) in 2..=3 {
//!     let i = 1;
//!     func(i)
//! }
//! ```
//! `func` will be called with 2 and 3, never with 1. The same is true for other places where const
//! values are introduced. For example in a `typle_for!` macro.
//! - const-for loops do not support labelled continue.
//! ```rust ignore
//! 'label: for typle_const!(i) in 2..=3 {
//!     loop {
//!         if typle_const!(i == 2) {
//!             continue 'label;  // compile error
//!         } else {
//!             break 'label;  // works
//!         }
//!     }
//! }
//! ```

mod constant;
mod specific;

use std::collections::HashMap;

use constant::evaluate_usize;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, ToTokens};
use specific::{ident_to_path, BlockState, SpecificContext};
use syn::punctuated::Punctuated;
use syn::{parse_quote, token, Expr, GenericParam, Generics, Item, ItemImpl, Pat, TypeParam};

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
    min_len: usize,
    max_len: usize,
}

fn parse_args(args: TokenStream) -> IterationTrait {
    // #[typle(Tuple for 2..=12)]
    let mut args_iter = args.into_iter();
    // Tuple
    let Some(TokenTree::Ident(trait_ident)) = args_iter.next() else {
        abort_call_site!("expected identifier");
    };
    // for
    match args_iter.next() {
        Some(TokenTree::Ident(for_ident)) if for_ident == "for" => {}
        _ => {
            abort_call_site!("expected for keyword");
        }
    }
    // 2..=12
    let rest = args_iter.collect();
    let range = syn::parse2::<syn::ExprRange>(rest).unwrap_or_else(|e| abort_call_site!("{}", e));
    let min = range
        .start
        .as_ref()
        .map(|expr| evaluate_usize(&expr).unwrap_or_else(|| abort!(expr, "range start invalid")))
        .unwrap_or_else(|| abort!(range, "range start must be bounded"));
    let end = range
        .end
        .as_ref()
        .unwrap_or_else(|| abort!(range, "range end must be bounded"));
    let max = match range.limits {
        syn::RangeLimits::HalfOpen(_) => evaluate_usize(&end)
            .and_then(|max| max.checked_sub(1))
            .unwrap_or_else(|| abort!(end, "range end invalid")),
        syn::RangeLimits::Closed(_) => {
            evaluate_usize(&end).unwrap_or_else(|| abort!(end, "range end invalid"))
        }
    };
    if max < min {
        abort!(range, "range contains no values");
    }
    IterationTrait {
        ident: trait_ident,
        min_len: min,
        max_len: max,
    }
}

impl IterationTrait {
    fn process_item(&self, item: Item) -> Vec<Item> {
        let mut output = Vec::new();
        match item {
            Item::Const(syn::ItemConst { ref generics, .. })
            | Item::Enum(syn::ItemEnum { ref generics, .. })
            | Item::Impl(syn::ItemImpl { ref generics, .. })
            | Item::Struct(syn::ItemStruct { ref generics, .. })
            | Item::Trait(syn::ItemTrait { ref generics, .. })
            | Item::TraitAlias(syn::ItemTraitAlias { ref generics, .. })
            | Item::Type(syn::ItemType { ref generics, .. })
            | Item::Union(syn::ItemUnion { ref generics, .. }) => {
                if self.has_typles(generics) {
                    for typle_len in self.min_len..=self.max_len {
                        let context = SpecificContext {
                            typle_trait: &self.ident,
                            typle_len,
                            constants: HashMap::new(),
                            typles: HashMap::new(),
                        };
                        let mut item = item.clone();
                        let mut state = BlockState::default();
                        context.replace_item(&mut item, true, &mut state);
                        output.push(item);
                    }
                } else {
                    output.push(item);
                }
            }
            Item::Fn(function) => {
                let generics = &function.sig.generics;
                if self.has_typles(generics) {
                    let fn_name = &function.sig.ident;
                    let fn_meta = &function.attrs;
                    let fn_vis = &function.vis;
                    let trait_name = format_ident!("_typle_fn_{}", fn_name);
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
                    for arg in fn_input_params {
                        match arg {
                            syn::FnArg::Receiver(_) => abort!(arg, "unexpected self"),
                            syn::FnArg::Typed(pat_type) => {
                                type_tuple.elems.push(pat_type.ty.as_ref().clone());
                                pat_tuple.elems.push(pat_type.pat.as_ref().clone());
                                value_tuple
                                    .elems
                                    .push(pat_to_tuple(pat_type.pat.as_ref().clone()));
                            }
                        }
                    }
                    let trait_item = parse_quote!(
                        #[allow(non_camel_case_types)]
                        #fn_vis trait #trait_name {
                            type Return;

                            fn apply(self) -> Self::Return;
                        }
                    );
                    output.push(trait_item);
                    let fn_item = parse_quote!(
                        #(#fn_meta)*
                        #fn_vis fn #fn_name <#fn_type_params_no_constraints>(#fn_input_params) -> <#type_tuple as #trait_name>::Return
                        where
                            #type_tuple: #trait_name,
                        {
                            <#type_tuple as #trait_name>::apply(#value_tuple)
                        }
                    );
                    output.push(fn_item);
                    let return_type = match function.sig.output {
                        syn::ReturnType::Default => parse_quote!(()),
                        syn::ReturnType::Type(_, t) => *t,
                    };
                    let fn_body = function.block;
                    let let_stmt: syn::Stmt = if self.min_len == 0 {
                        parse_quote!(
                            #[allow(unused_variables)]
                            let #pat_tuple = self;
                        )
                    } else {
                        parse_quote!(let #pat_tuple = self;)
                    };
                    let items = vec![
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
                            fn apply(self) -> Self::Return {
                                #let_stmt
                                #fn_body
                            }
                        ),
                    ];

                    let item = Item::Impl(ItemImpl {
                        attrs: Vec::new(),
                        defaultness: None,
                        unsafety: None,
                        impl_token: token::Impl::default(),
                        generics: function.sig.generics,
                        trait_: Some((None, ident_to_path(trait_name), token::For::default())),
                        self_ty: Box::new(syn::Type::Tuple(type_tuple)),
                        brace_token: token::Brace::default(),
                        items,
                    });

                    for typle_len in self.min_len..=self.max_len {
                        let context = SpecificContext {
                            typle_trait: &self.ident,
                            typle_len,
                            constants: HashMap::new(),
                            typles: HashMap::new(),
                        };
                        let mut item = item.clone();
                        let mut state = BlockState::default();
                        context.replace_item(&mut item, true, &mut state);
                        output.push(item);
                    }
                } else {
                    output.push(Item::Fn(function));
                }
            }
            Item::Mod(mut module) => {
                if let Some((_, items)) = &mut module.content {
                    for item in std::mem::take(items) {
                        items.extend(self.process_item(item));
                    }
                }
                output.push(Item::Mod(module));
            }
            item => {
                output.push(item);
            }
        }
        output
    }

    fn has_typles(&self, generics: &Generics) -> bool {
        for param in &generics.params {
            if let GenericParam::Type(type_param) = param {
                for bound in &type_param.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        let trait_path = &trait_bound.path;
                        if trait_path.leading_colon.is_none()
                            && trait_path.segments.len() == 1
                            && trait_path.segments[0].ident == self.ident
                        {
                            return true;
                        }
                    }
                }
            }
        }

        if let Some(where_clause) = &generics.where_clause {
            for predicate in &where_clause.predicates {
                if let syn::WherePredicate::Type(predicate_type) = predicate {
                    for bound in &predicate_type.bounds {
                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                            let trait_path = &trait_bound.path;
                            if trait_path.leading_colon.is_none()
                                && trait_path.segments.len() == 1
                                && trait_path.segments[0].ident == self.ident
                            {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        false
    }
}

fn remove_constraints(
    fn_type_params: &Punctuated<GenericParam, token::Comma>,
) -> Punctuated<GenericParam, token::Comma> {
    let mut output = Punctuated::new();
    for param in fn_type_params.into_iter() {
        if let GenericParam::Type(ref type_param) = param {
            output.push(GenericParam::Type(TypeParam {
                bounds: Punctuated::new(),
                ..type_param.clone()
            }));
        } else {
            output.push(param.clone());
        }
    }
    output
}

fn pat_to_tuple(pat: Pat) -> Expr {
    match pat {
        Pat::Const(p) => Expr::Const(p),
        Pat::Ident(p) => Expr::Path(syn::ExprPath {
            attrs: p.attrs,
            qself: None,
            path: ident_to_path(p.ident),
        }),
        Pat::Lit(p) => Expr::Lit(p),
        Pat::Macro(p) => Expr::Macro(p),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(_) => todo!(),
        Pat::Struct(_) => todo!(),
        Pat::Tuple(p) => Expr::Tuple(syn::ExprTuple {
            attrs: p.attrs,
            paren_token: p.paren_token,
            elems: p.elems.into_iter().map(pat_to_tuple).collect(),
        }),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(_) => todo!(),
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

/// Create a tuple or array.
///
/// Loop over the indices of the tuple, performing the expression for each index.
///
/// If the macro uses parentheses, the returned value is a tuple. If the macro uses brackets, the
/// returned value is an array.
///
/// With parentheses, this macro can be used in type or value position.
///
/// Examples:
/// ```ignore
/// #[typle(Tuple for 0..=2)]
/// impl<T> S<T<{..}>>
/// where
///     T: Tuple<u32>
/// {
///     fn new(t: typle_for!(i in .. => &T<{i}>)) {
///         // Square brackets create an array
///         let a = typle_for![i in 0..T::LEN => *t[[i]] * 2];
///         // Parentheses create a tuple
///         // The default bounds of the range are 0..Tuple::LEN
///         let b = typle_for!(i in .. => *t[[i]] * 2);
///         // Arbitrary expressions can be used for the indices and
///         // the iterator variable can be left out if not needed
///         let init: [Option<u32>; T::LEN] = typle_for![T::LEN * 2..T::LEN * 3 => None];
///     }
/// }
/// ```
/// generates
/// ```ignore
/// impl S0 {
///     fn new(t: ()) {
///         let a = [];
///         let b = ();
///         let init: [Option<u32>; 0] = [];
///     }
/// }
/// impl S1<u32> {
///     fn new(t: (&u32,)) {
///         let a = [*t.0 * 2];
///         let b = (*t.0 * 2,);
///         let init: [Option<u32>; 1] = [None];
///     }
/// }
/// impl S2<u32, u32> {
///     fn new(t: (&u32, &u32)) {
///         let a = [*t.0 * 2, *t.1 * 2];
///         let b = (*t.0 * 2, *t.1 * 2);
///         let init: [Option<u32>; 2] = [None, None];
///     }
/// }
/// ```
#[proc_macro_error]
#[proc_macro]
pub fn typle_for(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    abort_call_site!("typle_variant macro only available in item with typle attribute");
}

/// Create variants in an enum.
///
/// In an enum, the `typle_variant` macro allows the creation of variants for each component.
///
/// A variant is created for each index in the range provided. The default range is `0..Tuple::LEN`.
///
/// The variants will start with the variant name given before the `=` character, followed by the
/// index.
///
/// If the macro uses parentheses the variant will be use unnamed fields. If the macro uses braces
/// the variant will use named fields. If the macro uses brackets the variant will have no fields.
///
/// Examples:
///
/// ```
/// # use typle::typle;
/// # trait Process {
/// #     type State;
/// # }
/// #[typle(Tuple for 2..=2)]
/// pub enum ProcessState<T>
/// where
///     T: Tuple,
///     T<_>: Process,
/// {
///     Q = typle_variant![..],
///     R = typle_variant!{i in 0..T::LEN => r: T<{i}>},
///     S = typle_variant!(i in .. => Option<T<{i}>::State>, [u64; i]),
///     Done([u64; Tuple::LEN])
/// }
/// ```
/// creates
/// ```
/// # trait Process {
/// #     type State;
/// # }
/// pub enum ProcessState2<T0, T1>
/// where
///     T0: Process,
///     T1: Process,
/// {
///     Q0,
///     Q1,
///     R0 { r: T0 },
///     R1 { r: T1 },
///     S0(Option<<T0>::State>, [u64; 0]),
///     S1(Option<<T1>::State>, [u64; 1]),
///     Done([u64; 2]),
/// }
/// ```
#[proc_macro_error]
#[proc_macro]
pub fn typle_variant(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    abort_call_site!("typle_variant macro only available in item with typle attribute");
}
