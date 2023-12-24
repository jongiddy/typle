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
//! This code creates implementations for 1-, 2-, and 3-tuples where each component of the tuple is
//! a `u32`.
//!
//! ```
//! # struct MyStruct<T> {t: T}
//! impl MyStruct<(u32,)> {
//!     fn max(&self) -> Option<u32> {
//!         let mut max = self.t.0;
//!         { () }
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
//!             ()
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
//!             ()
//!         }
//!         Some(max)
//!     }
//! }
//! ```
//!
//! The macro arguments `Tuple for 1..=3` consist of an identifier (`Tuple`) to use as a pseudo-trait
//! in the `where` clause, and a range of tuple lengths `1..=3` for which the item will be created.
//!
//! The `Tuple` pseudo-trait is similar to a trait defined as
//! ```
//! trait Tuple<Types> {
//!     const LEN: usize;
//! }
//! ```
//!
//! `Tuple::LEN` or `T::LEN` provides the number of components for the tuple in the current item.
//!
//! If the `where` clause constrains a generic type using the pseudo-trait then the generic type
//! must be a tuple with a length `Tuple::LEN` and where each component is constrained by the
//! argument to the trait. The component can either be an explicit type (`where T: Tuple<u32>`),
//! or each component can be constrained by other traits using `T<_>`:
//! ```ignore
//! impl<T> MyStruct<T>
//! where
//!     T: Tuple,
//!     T<_>: Extract,
//!     T<_>::Output: AsRef<str>,
//! ```
//!
//! Constants (`T<0>`, `T<{T::LEN - 1}>`) and ranges (`T<{1..}>`) are also supported.
//!
//! The `typle_bound!` macro allows trait bounds to use the index of the bound type:
//! ```ignore
//! impl<T> MyStruct<T>
//! where
//!     T: Tuple,
//!     typle_bound!(i for 1.. => T<{i}): Mul<M<{i - 1}>>,
//! ```
//!
//! The components of a tuple can be iterated over using a `for` loop with an iteration variable
//! enclosed in `typle_const!` macro. As shown above, this executes the `for` loop body for each
//! component in the tuple.
//!
//! Tuple components are referenced using a double-bracketed index and a constant value, including an
//! index created using `typle_const!`. Hence `self.t[[i]]` will be replaced by
//! `self.t.0, self.t.1,...`.
//!
//! Other features include using `T<{i}>` to name component types, a `typle_for!` macro to perform
//! component-by-component operations, support for enums with a `typle_variant!()` macro, and
//! constant-if for conditional compilation based on constant values including a `typle_const!`
//! iteration variable. See the [README](https://github.com/jongiddy/typle#readme) and
//! [the test directory](https://github.com/jongiddy/typle/blob/main/tests/expand/).
//!
//! Also, see how `typle` is used in the [`hefty` crate](https://github.com/jongiddy/hefty/blob/main/src/tuple.rs).
//!
//! # Limitations
//!
//! - Shadowing of const variables introduced using typle macros is not supported. For example, in:
//! ```rust ignore
//! for typle_const!(i) in 2..4 {
//!     let i = 1;
//!     func(i)
//! }
//! ```
//! `func` will be called with 2 and 3, never with 1. The same is true for other places where const
//! values are introduced. For example in a `typle_for!` macro.
//! - const-for loops do not support labelled continue.
//! ```rust ignore
//! 'label: for typle_const!(i) in 2..4 {
//!     loop {
//!         if typle_const!(i == 2) {
//!             continue 'label;  // compile error
//!         } else {
//!             break 'label;  // works
//!         }
//!     }
//! }
//! ```
//! - Standalone functions require explicit lifetimes on references and cannot accept tuples with
//! unsized components.
//! ```rust ignore
//! #[typle(Tuple for 1..=3)]
//! fn hash<'a, T, S>(tuple: T, state: &'a mut S)
//! where
//!     T: Tuple,
//!     T<_>: Hash,
//!     // T<{T::LEN - 1}>: ?Sized,
//!     S: Hasher,
//! {
//!     for typle_const!(i) in 0..T::LEN {
//!         tuple[[i]].hash(state);
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
use syn::{parse_quote, token, Expr, Generics, Item, ItemImpl, Pat};

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
                    let trait_name = format_ident!("_typle_fn_{}", fn_name);
                    let fn_type_params = &function.sig.generics.params;
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
                        trait #trait_name {
                            type Return;

                            fn apply(self) -> Self::Return;
                        }
                    );
                    output.push(trait_item);
                    let fn_item = parse_quote!(
                        #(#fn_meta)*
                        fn #fn_name <#fn_type_params>(#fn_input_params) -> <#type_tuple as #trait_name>::Return
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
                        // Following uses value_tuple instead of pat_tuple because
                        // https://github.com/dtolnay/syn/issues/1553
                        parse_quote!(
                            fn apply(self) -> Self::Return {
                                let #value_tuple = self;
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
        let Some(where_clause) = &generics.where_clause else {
            return false;
        };

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

        false
    }
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
///     Q = typle_variant![.. =>],
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
///     S0(Option<T0::State>, [u64; 0]),
///     S1(Option<T1::State>, [u64; 1]),
///     Done([u64; 2]),
/// }
/// ```
#[proc_macro_error]
#[proc_macro]
pub fn typle_variant(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    abort_call_site!("typle_variant macro only available in item with typle attribute");
}
