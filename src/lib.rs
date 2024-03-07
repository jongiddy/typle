//! The `typle` macro generates code for multiple tuple lengths. This code
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
//! generates implementations of the `From` trait for tuples with 0 to 3 components:
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
//! `<{i}>` for types and `[[i]]` for values. The value `i` must be a *typle
//! index expression*, an expression that only uses literal `usize` values or
//! *typle index variables* created by one of several macros.
//!
//! The [`typle_for!`] macro creates a new tuple type or value. Inside the macro
//! the typle index variable can provide access to each component of an existing
//! tuple type or value.
//!
//! The associated constant `LEN` provides the length of the tuple in each
//! generated item. This value can be used in typle index expressions.
//!
//! ```rust
//! # use typle::typle;
//! // Split off the first component
//! #[typle(Tuple for 1..=12)]
//! fn split<T: Tuple>(
//!     t: T  // t: (T0, T1, T2,...)
//! ) -> (T<0>, typle_for!(i in 1..T::LEN => T<{i}>))   // (T0, (T1, T2,...))
//! {
//!     (t[[0]], typle_for!(i in 1..T::LEN => t[[i]]))  // (t.0, (t.1, t.2,...))
//! }
//!
//! assert_eq!(split(('1', 2, 3.0)), ('1', (2, 3.0)));
//! assert_eq!(split((2, 3.0)), (2, (3.0,)));
//! assert_eq!(split((3.0,)), (3.0, ()));
//! ```
//!
//! Specify constraints on the tuple components using one of the following
//! forms. Except for the first form, these constraints can only appear in the
//! `where` clause.
//! - `T: Tuple<C>` - each component of the tuple has type `C`
//! - `T<_>: Copy` - each component of the tuple implements `Copy`
//! - `T<0>: Copy` - the first component of the tuple implements `Copy`
//! - `T<{1..=2}>: Copy` - the second and third components implement `Copy`
//! - `typle_bound!` - the most general way to bound components,
//! allowing the typle index variable to be used in the trait bounds, as shown
//! below:
//!
//! ```rust
//! # use typle::typle;
//! use std::{ops::Mul, time::Duration};
//!
//! // Multiply the components of two tuples
//! #[typle(Tuple for 0..=12)]
//! fn multiply<S: Tuple, T: Tuple>(
//!     s: S,  // s: (S0,...)
//!     t: T,  // t: (T0,...)
//! ) -> typle_for!(i in ..T::LEN => <S<{i}> as Mul<T<{i}>>>::Output)  // (<S0 as Mul<T0>>::Output,...)
//! where
//!     typle_bound!(i in ..T::LEN => S<{i}>): Mul<T<{i}>>,  // S0: Mul<T0>,...
//! {
//!     typle_for!(i in ..T::LEN => s[[i]] * t[[i]])  // (s.0 * t.0,...)
//! }
//!
//! assert_eq!(
//!     multiply((Duration::from_secs(5), 2), (4, 3)),
//!     (Duration::from_secs(20), 6)
//! )
//! ```
//!
//! Use the `typle_index!` macro in a `for` loop to iterate over a range bounded
//! by typle index expressions.
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
//!     // Return the sums of all odd positions and all even positions.
//!     fn interleave(&self) -> [C; 2] {
//!         let mut odd_even = [C::default(), C::default()];
//!         for typle_index!(i) in 0..T::LEN {
//!             odd_even[i % 2] += &self.t[[i]];
//!         }
//!         odd_even
//!     }
//! }
//!
//! let m = MyStruct::from((3, 9, 11));
//! assert_eq!(m.interleave(), [14, 9]);
//! ```
//!
//! The next example is simplified from code in the
//! [`hefty`](https://github.com/jongiddy/hefty/blob/main/src/tuple.rs) crate and
//! demonstrates the use of `typle` with `enum`s.
//!
//! Typled `enum`s are implemented for the maximum length. When referring to
//! these types from other typled items, use `TupleSequenceState<T<{ .. }>>`.
//! For typle range expressions the default lower bound is 0 and the default
//! upper bound is `T::MAX`, the maximum length for the tuple. This will fill in
//! unused type parameters with the `never` type provided for the `typle` macro.
//! The default type is [`!`] but this is not available in stable Rust.
//! [`std::convert::Infallible`] is an uninhabited type that is available in
//! stable Rust but any type is permissible.
//!
//! The [`typle_variant!`] macro creates multiple enum variants by looping
//! similarly to `typle_for!`.
//!
//! The `typle_ident!` macro concatenates a number to an identifier. For
//! example `S::<typle_ident!(3)>` becomes the identifer `S3`.
//!
//! The `typle_attr_if` attribute allows conditional inclusion of attributes. It works similarly to
//! [`cfg_attr`](https://doc.rust-lang.org/reference/conditional-compilation.html#the-cfg_attr-attribute)
//! except that the first argument is a boolean typle index expression.
//!
//! The `typle_const!` macro supports const-if on a boolean typle index
//! expression. const-if allows branches that do not compile, as long as they
//! are `false` at compile-time. For example, this code compiles when
//! `j` is the length of the `output` tuple even though the identifier `output[[j]]`
//! is not valid because it only exists in the false branch of the `if`.
//!
//! ```rust
//! # use typle::typle;
//! #[typle(Tuple for 1..=4, never=std::convert::Infallible)]
//! mod tuple {
//!     pub trait Extract {
//!         type State;
//!         type Output;
//!
//!         fn extract(&self, state: Option<Self::State>) -> Self::Output;
//!     }
//!
//!     impl Extract for std::convert::Infallible {
//!         type State = std::convert::Infallible;
//!         type Output = ();
//!
//!         fn extract(
//!             &self,
//!             _state: Option<Self::State>,
//!         ) -> Self::Output {
//!             ()
//!         }
//!     }
//!
//!     pub enum TupleSequenceState<T>
//!     where
//!         T: Tuple,
//!         T<_>: Extract,
//!     {
//!         S = typle_variant!(i in .. =>
//!             typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
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
//!         // The state contains the output from all previous components and the state
//!         // of the current component.
//!         type State = TupleSequenceState<T<{ .. }>>;
//!         type Output = typle_for!(i in ..T::LEN => <T<{i}> as Extract>::Output);
//!
//!         fn extract(&self, state: Option<Self::State>) -> Self::Output {
//!             #[typle_attr_if(T::LEN == 1, allow(unused_mut))]
//!             let mut state = state.unwrap_or(Self::State::S::<typle_ident!(0)>((), None));
//!             for typle_index!(i) in 0..T::LEN {
//!                 // For LEN = 1 there is only one variant (S0) so `let` is irrefutable
//!                 #[typle_attr_if(T::LEN == 1, allow(irrefutable_let_patterns))]
//!                 // For i == 0, the `output` state variable does not get used
//!                 #[typle_attr_if(i == 0, allow(unused_variables))]
//!                 if let Self::State::S::<typle_ident!(i)>(output, inner_state) = state {
//!                     let matched = self.tuple[[i]].extract(inner_state);
//!                     let output = typle_for!(j in ..=i =>
//!                         if typle_const!(j < i) { output[[j]] } else { matched }
//!                     );
//!                     if typle_const!(i + 1 == T::LEN) {
//!                         return output;
//!                     } else {
//!                         state = Self::State::S::<typle_ident!(i + 1)>(output, None);
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
//! # impl Extract for std::convert::Infallible {
//! #     type State = std::convert::Infallible;
//! #     type Output = ();
//! #     fn extract(&self, _state: Option<Self::State>) -> Self::Output {
//! #         ()
//! #     }
//! # }
//! # pub struct TupleSequence<T> {
//! #     tuple: T,
//! # }
//! // enum implemented only for maximum size
//! pub enum TupleSequenceState<T0, T1, T2, T3>
//! where
//!     T0: Extract,
//!     T1: Extract,
//!     T2: Extract,
//!     T3: Extract,
//! {
//!     S0((), Option<<T0>::State>),
//!     S1((<T0>::Output,), Option<<T1>::State>),
//!     S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
//!     S3((<T0>::Output, <T1>::Output, <T2>::Output), Option<<T3>::State>),
//! }
//!
//! impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
//! where
//!     T0: Extract,
//!     T1: Extract,
//!     T2: Extract,
//! {
//!     // reference to enum uses `never` type for unused type parameters.
//!     type State = TupleSequenceState<T0, T1, T2, std::convert::Infallible>;
//!     type Output = (
//!         <T0 as Extract>::Output,
//!         <T1 as Extract>::Output,
//!         <T2 as Extract>::Output,
//!     );
//!     fn extract(&self, state: Option<Self::State>) -> Self::Output {
//!         let mut state = state.unwrap_or(Self::State::S0((), None));
//!         {
//!             {
//!                 #[allow(unused_variables)]
//!                 if let Self::State::S0(output, inner_state) = state {
//!                     let matched = self.tuple.0.extract(inner_state);
//!                     let output = ({ matched },);
//!                     {
//!                         state = Self::State::S1(output, None);
//!                     }
//!                 }
//!             }
//!             {
//!                 if let Self::State::S1(output, inner_state) = state {
//!                     let matched = self.tuple.1.extract(inner_state);
//!                     let output = ({ output.0 }, { matched });
//!                     {
//!                         state = Self::State::S2(output, None);
//!                     }
//!                 }
//!             }
//!             {
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
//! - Standalone functions require explicit lifetimes on references:
//! ```rust
//! # use std::hash::{Hash, Hasher};
//! # use typle::typle;
//! #[typle(Tuple for 1..=3)]
//! pub fn hash<'a, T, S: Hasher>(tuple: &'a T, state: &'a mut S)
//! where
//!     T: Tuple,
//!     T<_>: Hash,
//!     T<{T::LEN - 1}>: ?Sized,
//! {
//!     for typle_index!(i) in 0..T::LEN {
//!         tuple[[i]].hash(state);
//!     }
//! }
//! ```
//! - Typle index variables cannot be shadowed:
//! ```rust
//! # use typle::typle;
//! # #[typle(Tuple for 1..=1)]
//! # fn test<T>(t: T) where T: Tuple {
//! let mut v = vec![];
//! for typle_index!(i) in 2..=3 {
//!     let i = 1;
//!     v.push(i);
//! }
//! assert_eq!(v, [2, 3]);
//! # }
//! ```
//! - A `continue` referencing a label on a `for` loop using `typle_index!` works but displays an
//! [unsuppressible warning](https://github.com/rust-lang/rust/issues/31745) during compilation.
//! ```rust
//! # use typle::typle;
//! # #[typle(Tuple for 1..=1)]
//! # fn test<T>(t: T) where T: Tuple {
//! // warning: label name `'cont` shadows a label name that is already in scope
//! 'cont: for typle_index!(i) in 2..=3 {
//!     loop {
//!         if i == 2 {
//!             continue 'cont;
//!         }
//!         break;
//!     }
//! }
//! # }
//! ```
//! - Due to interaction of `typle` with other macros, passing some types and
//! expressions to a macro may produce unexpected results. To help work around
//! this, inside a macro invocation the `typle_ty!` macro expands types and the
//! `typle_expr!` macro expands expressions.
//!
//! ```rust
//! # use typle::typle;
//! # #[typle(Tuple for 3..=3)]
//! # fn test1<T: Tuple>(t: T) {
//! assert_eq!(
//!     stringify!([T, typle_ty!(T), T::LEN, typle_expr!(T::LEN)]),
//!     "[T, (T0, T1, T2), T :: LEN, 3]"
//! );
//! # }
//! # test1((1, 2, 3));
//! ```
//!

mod constant;
mod context;

use constant::evaluate_usize;
use context::TypleContext;
use proc_macro2::{Ident, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::ToTokens;
use syn::{Item, Type, TypeNever};

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
    let typle_macro = TypleMacro::from(TokenStream::from(args));

    let Ok(item) = syn::parse::<Item>(item) else {
        abort_call_site!("unsupported tokens");
    };

    let context = TypleContext::from(&typle_macro);

    let mut items = Vec::new();

    context.replace_item(item, &mut items);

    items
        .into_iter()
        .map(Item::into_token_stream)
        .collect::<TokenStream>()
        .into()
}

#[derive(Clone)]
struct TypleMacro {
    ident: Ident,
    min_len: usize,
    max_len: usize,
    never_type: Type,
}

impl From<TokenStream> for TypleMacro {
    fn from(args: TokenStream) -> Self {
        // #[typle(Tuple for 2..=12, never=std::convert::Infallible)]
        let mut never_type = Type::Never(TypeNever {
            bang_token: syn::token::Not::default(),
        });
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

        let mut range_tokens = Vec::new();
        let mut never_tokens = Vec::new();
        let mut comma_seen = false;
        for token in args_iter {
            if comma_seen {
                never_tokens.push(token);
            } else {
                if let TokenTree::Punct(punct) = &token {
                    if punct.as_char() == ',' {
                        comma_seen = true;
                        continue;
                    }
                }
                range_tokens.push(token);
            }
        }
        // 2..=12
        let range_stream = range_tokens.into_iter().collect();
        let range = syn::parse2::<syn::ExprRange>(range_stream)
            .unwrap_or_else(|e| abort_call_site!("{}", e));
        let min = range
            .start
            .as_ref()
            .map(|expr| {
                evaluate_usize(&expr).unwrap_or_else(|| abort!(expr, "range start invalid"))
            })
            .unwrap_or_else(|| abort!(range, "range start must be bounded"));
        let end = range
            .end
            .as_ref()
            .unwrap_or_else(|| abort!(range, "range end must be bounded"));
        let max = match range.limits {
            syn::RangeLimits::HalfOpen(_) => evaluate_usize(&end)
                .and_then(|max| max.checked_sub(1))
                .unwrap_or_else(|| abort!(end, "range end invalid1")),
            syn::RangeLimits::Closed(_) => {
                evaluate_usize(&end).unwrap_or_else(|| abort!(end, "range end invalid2"))
            }
        };
        if max < min {
            abort!(range, "range contains no values");
        }
        if !never_tokens.is_empty() {
            // never=some::Type
            let mut iter = never_tokens.into_iter();
            let Some(TokenTree::Ident(ident)) = iter.next() else {
                abort_call_site!("expected identifier after comma");
            };
            if ident != "never" {
                abort_call_site!("expected identifier 'never' after comma");
            }
            let Some(TokenTree::Punct(punct)) = iter.next() else {
                abort_call_site!("expected equals after never");
            };
            if punct.as_char() != '=' {
                abort_call_site!("expected equals after never");
            }
            let type_stream = iter.collect();
            never_type =
                syn::parse2::<Type>(type_stream).unwrap_or_else(|e| abort_call_site!("{}", e));
        }
        TypleMacro {
            ident: trait_ident,
            min_len: min,
            max_len: max,
            never_type,
        }
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
/// ```
/// # use typle::typle;
/// struct S<T>
/// {
///     t: T
/// }
///
/// #[typle(Tuple for 0..=2)]
/// impl<T: Tuple<u32>> S<T>
/// {
///     fn new(t: typle_for!(i in ..T::LEN => &T<{i}>)) {
///         // Square brackets create an array
///         let a: [u32; T::LEN] = typle_for![i in 0..T::LEN => *t[[i]] * 2];
///         // Parentheses create a tuple
///         // The default bounds of the range are 0..Tuple::LEN
///         let b = typle_for!(i in ..T::LEN => *t[[i]] * 2);
///         // Arbitrary expressions can be used for the indices and
///         // the iterator variable can be left out if not needed
///         let init: [Option<u32>; T::LEN] = typle_for![T::LEN * 2..T::LEN * 3 => None];
///     }
/// }
/// ```
/// generates
/// ```
/// # struct S<T>
/// # {
/// #     t: T
/// # }
/// impl S<()> {
///     fn new(t: ()) {
///         let a: [u32; 0] = [];
///         let b = ();
///         let init: [Option<u32>; 0] = [];
///     }
/// }
/// impl S<(u32,)> {
///     fn new(t: (&u32,)) {
///         let a: [u32; 1] = [*t.0 * 2];
///         let b = (*t.0 * 2,);
///         let init: [Option<u32>; 1] = [None];
///     }
/// }
/// impl S<(u32, u32)> {
///     fn new(t: (&u32, &u32)) {
///         let a: [u32; 2] = [*t.0 * 2, *t.1 * 2];
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
/// If the macro uses parentheses the variant will use unnamed fields. If the macro uses braces the
/// variant will use named fields. If the macro uses brackets the variant will have no fields.
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
///     Q = typle_variant![..Tuple::MAX],
///     R = typle_variant!{i in 0..T::MAX => r: T<{i}>},
///     S = typle_variant!(i in ..T::MAX => Option<T<{i}>::State>, [u64; i]),
///     Done([u64; Tuple::MAX])
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
