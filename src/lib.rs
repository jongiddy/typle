//! The `typle` macro generates code for multiple tuple lengths. This code
//!
//! ```rust
//! use typle::typle;
//!
//! struct MyStruct<T> {
//!     t: T,
//! }
//!
//! #[typle(Tuple for 2..=3)]
//! impl<T: Tuple> From<T> for MyStruct<T>
//! {
//!     fn from(t: T) -> Self {
//!         MyStruct { t }
//!     }
//! }
//! ```
//!
//! generates implementations of the `From` trait for tuples with 2 to 3 components:
//! ```rust
//! # struct MyStruct<T> {
//! #     t: T,
//! # }
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
//! *typle index variables* created by one of several macros, and reduces to a
//! single value or a range.
//!
//! ```rust
//! # use typle::typle;
//! // Split off the first component
//! #[typle(Tuple for 1..=12)]
//! fn split<T: Tuple>(
//!     t: T  // t: (T<0>, T<1>, T<2>,...)
//! ) -> (T<0>, (T<{1..}>,))  // (T<0>, (T<1>, T<2>,...))
//! {
//!     (t[[0]], (t[[1..]],))  // (t.0, (t.1, t.2,...))
//! }
//!
//! assert_eq!(split(('1', 2, 3.0)), ('1', (2, 3.0)));
//! assert_eq!(split((2, 3.0)), (2, (3.0,)));
//! assert_eq!(split((3.0,)), (3.0, ()));
//! ```
//!
//! The `typle_for!` macro creates a new tuple type or expression. Inside
//! the macro the typle index variable provides access to each component of
//! an existing tuple type or expression.
//!
//! The associated constant `LEN` provides the length of the tuple in each
//! generated item. This value can be used in typle index expressions.
//!
//! ```
//! # use typle::typle;
//! #[typle(Tuple for 0..=12)]
//! fn reverse<T: Tuple>(t: T) -> typle_for!(i in 1..=T::LEN => T<{T::LEN - i}>) {
//!     typle_for!(i in 1..=T::LEN => t[[T::LEN - i]])
//! }
//!
//! assert_eq!(reverse((Some(3), "four", 5)), (5, "four", Some(3)));
//! ```
//!
//! The [`typle_fold!`] macro reduces a tuple to a single value.
//!
//! The default bounds for a macro range are `0..Tuple::LEN`, that is, for all
//! components of the tuple.
//!
//! ```rust
//! # use typle::typle;
//! #[typle(Tuple for 0..=12)]
//! fn sum<T: Tuple<u32>>(t: T) -> u32 {
//!     typle_fold!(0; i in .. => |total| total + t[[i]])
//! }
//!
//! assert_eq!(sum(()), 0);
//! assert_eq!(sum((1, 4, 9, 16)), 30);
//! ```
//!
//! Specify constraints on the tuple components using one of the following
//! forms. Except for the first form, these constraints can only appear in the
//! `where` clause.
//! - `T: Tuple<C>` - each component of the tuple has type `C`
//! - `T<_>: Copy` - each component of the tuple implements `Copy`
//! - `T<0>: Copy` - the first component of the tuple implements `Copy`
//! - `T<{1..=2}>: Copy` - the second and third components implement `Copy`
//! - `typle_bound!` - the most general way to bound components, allowing
//!   arbitrary expressions using the typle index variable on both sides of
//!   the colon, as shown below:
//!
//! ```rust
//! # use typle::typle;
//! use std::{ops::Mul, time::Duration};
//!
//! // Multiply the components of two tuples
//! #[typle(Tuple for 0..=12)]
//! fn multiply<S: Tuple, T: Tuple>(
//!     s: S,  // s: (S<0>,...)
//!     t: T,  // t: (T<0>,...)
//! ) -> typle_for!(i in .. => <S<{i}> as Mul<T<{i}>>>::Output)  // (<S<0> as Mul<T<0>>>::Output,...)
//! where
//!     typle_bound!(i in .. => S<{i}>): Mul<T<{i}>>,  // S<0>: Mul<T<0>>,...
//! {
//!     typle_for!(i in .. => s[[i]] * t[[i]])  // (s.0 * t.0,...)
//! }
//!
//! assert_eq!(
//!     multiply((Duration::from_secs(5), 2), (4, 3)),
//!     (Duration::from_secs(20), 6)
//! )
//! ```
//!
//! The `typle!` macro allows components of a tuple to be inserted into an existing sequence.
//! ```
//! # use typle::typle;
//! #[typle(Tuple for 0..=12)]
//! fn coalesce_some<S: Tuple, T: Tuple>(
//!     s: S,
//!     t: T
//! ) -> (typle!(i in .. => Option<S<{i}>>), typle!(i in .. => Option<T<{i}>>))
//! where
//!     T: Tuple,
//! {
//!     (typle!(i in .. => Some(s[[i]])), typle!(i in .. => Some(t[[i]])))
//! }
//!
//! assert_eq!(
//!     coalesce_some((1, 2), (3, 4)),
//!     (Some(1), Some(2), Some(3), Some(4))
//! );
//! ```
//!
//! Note that this behaves differently to `typle_for!`. The `typle_for!` macro
//! produces a new tuple. The `typle!` macro produces a sequence of components
//! that must appear inside an existing tuple, array, or argument list.
//!
//! For types, `T<{start..end}>` is a shorthand for `typle!(i in start..end => T<{i}>)`.
//! For expressions, `t[[start..end]]` is a shorthand for `typle!(i in start..end => t[[i]])`.
//!
//! ```
//! # use typle::typle;
//! #[typle(Tuple for 0..12)]
//! fn append<T: Tuple, A>(t: T, a: A) -> (T<{..}>, A) {
//!     (t[[..]], a)
//! }
//!
//! assert_eq!(append((1, 2, 3), 4), (1, 2, 3, 4));
//! ```
//!
//! # Conditionals
//!
//! The `typle!`, `typle_for!`, and `typle_bound!` macros accept an `if` statement with an optional
//! `else` clause. If there is no `else` clause the macro filters out components that do not match
//! the condition.
//!
//! The `typle_attr_if` attribute allows conditional inclusion of attributes. It works similarly to
//! [`cfg_attr`](https://doc.rust-lang.org/reference/conditional-compilation.html#the-cfg_attr-attribute)
//! except that the first argument is a boolean typle index expression.
//!
//! ```
//! # use typle::typle;
//! #[typle(Tuple for 0..=12)]
//! fn even_string_odd<T: Tuple>(
//!     t: T,
//! ) -> typle_for!(i in .. => if i % 2 == 0 { String } else { T<{i}> })
//! where
//!     typle_bound!(i in .. => if i % 2 == 0 { T<{i}> }): ToString,
//! {
//!     #[typle_attr_if(T::LEN == 0, allow(clippy::unused_unit))]
//!     typle_for!(i in .. => if i % 2 == 0 { t[[i]].to_string() } else { t[[i]] })
//! }
//!
//! assert_eq!(even_string_odd((0, 1, 2, 3)), ("0".to_owned(), 1, "2".to_owned(), 3));
//! ```
//!
//! # Iteration
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
//! # Selection
//!
//! Indexing using `[[i]]` only works with tuple index expressions. To select a component from a
//! tuple value using a variable, use `typle_index!` in a `match` expression:
//!
//! ```rust
//! # use typle::typle;
//! #[typle(Tuple for 1..=12)]
//! fn get_component<'t, C, T>(t: &'t T, i: usize) -> Option<&'t C>
//! where
//!     T: Tuple<C>,
//! {
//!     // `i` is a variable, `j` is a typle index variable.
//!     match i {
//!         j @ typle_index!(0..T::LEN) => Some(&t[[j]]),
//!         _ => None,
//!     }
//! }
//!
//! let t = ('a', 'b', 'c');
//! assert_eq!(get_component(&t, 1), Some(&'b'));
//! assert_eq!(get_component(&t, 4), None);
//! ```
//!
//! # enums
//!
//! Applying `typle` to an `enum` implements the `enum` for the maximum length
//! and allows use of typle index variables to define the variants.
//!
//! The [`typle_variant!`] macro creates multiple enum variants by looping
//! similarly to `typle_for!`.
//!
//! ```rust
//! # use typle::typle;
//! pub trait Extract {
//!     type State;
//!     type Output;
//!
//!     fn extract(&self, state: Option<Self::State>) -> Self::Output;
//! }
//!
//! #[typle(Tuple for 1..=4)]
//! pub enum TupleSequenceState<T>
//! where
//!     T: Tuple,
//!     T<_>: Extract,
//! {
//!     // The output of all previous components plus the state of the current component.
//!     S = typle_variant!(i in ..T::MAX =>
//!         typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
//!     ),
//! }
//! ```
//!
//! The single generated implementation:
//! ```rust
//! # pub trait Extract {
//! #     type State;
//! #     type Output;
//! #     fn extract(&self, state: Option<Self::State>) -> Self::Output;
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
//! ```
//!
//! Other `typle` implementations can refer to this enum using
//! `TupleSequenceState<T<{ ..T::MAX }>>`. This will fill in unused type
//! parameters with the `never` type provided for the `typle` macro. The default
//! type is [`!`] but this is not available in stable Rust.
//! [`std::convert::Infallible`] is an uninhabited type that is available in
//! stable Rust, but any type is permissible.
//!
//! The `typle_ident!` macro concatenates a number to an identifier. For
//! example `S::<typle_ident!(3)>` becomes the identifier `S3`. This is mainly
//! used to refer to enum variants.
//!
//! The `typle_const!` macro supports const-if on a boolean typle index
//! expression. const-if allows branches that do not compile, as long as they
//! are `false` at compile-time. For example, this code compiles for `T::LEN == 4`
//! even though the variant `TupleSequenceState::S4` does not exist because the
//! branch that refers to it is `false` when `(i + 1 == T::LEN)`
//!
//! ```rust
//! # use typle::typle;
//! # pub trait Extract {
//! #     type State;
//! #     type Output;
//! #     fn extract(&self, state: Option<Self::State>) -> Self::Output;
//! # }
//! # #[typle(Tuple for 1..=4)]
//! # pub enum TupleSequenceState<T>
//! # where
//! #     T: Tuple,
//! #     T<_>: Extract,
//! # {
//! #     S = typle_variant!(i in ..T::MAX =>
//! #         typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
//! #     ),
//! # }
//! // Relevant traits may need to be implemented for the never type.
//! impl Extract for std::convert::Infallible {
//!     type State = std::convert::Infallible;
//!     type Output = ();
//!
//!     fn extract(
//!         &self,
//!         _state: Option<Self::State>,
//!     ) -> Self::Output {
//!         ()
//!     }
//! }
//!
//! pub struct TupleSequence<T> {
//!     tuple: T,
//! }
//!
//! #[typle(Tuple for 1..=4, never=std::convert::Infallible)]
//! impl<T> Extract for TupleSequence<T>
//! where
//!     T: Tuple,
//!     T<_>: Extract,
//! {
//!     // The state contains the output from all previous components and
//!     // the state of the current component.
//!     type State = TupleSequenceState<T<{ ..T::MAX }>>;
//!     // The final output is a tuple of outputs from all components.
//!     type Output = typle_for!(i in .. => <T<{i}> as Extract>::Output);
//!
//!     fn extract(&self, state: Option<Self::State>) -> Self::Output {
//!         // When LEN == 1 the code never changes `state`
//!         #[typle_attr_if(T::LEN == 1, allow(unused_mut))]
//!         let mut state = state.unwrap_or(Self::State::S::<typle_ident!(0)>((), None));
//!         for typle_index!(i) in 0..T::LEN {
//!             // When i == 0, the `output` state variable does not get used
//!             #[typle_attr_if(i == 0, allow(unused_variables))]
//!             if let Self::State::S::<typle_ident!(i)>(output, inner_state) = state {
//!                 let matched = self.tuple[[i]].extract(inner_state);
//!                 let output = (output[[..i]], matched);
//!                 if typle_const!(i + 1 == T::LEN) {
//!                     return output;
//!                 } else {
//!                     state = Self::State::S::<typle_ident!(i + 1)>(output, None);
//!                 }
//!             }
//!         }
//!         unreachable!();
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
//! # pub enum TupleSequenceState<T0, T1, T2, T3>
//! # where
//! #     T0: Extract,
//! #     T1: Extract,
//! #     T2: Extract,
//! #     T3: Extract,
//! # {
//! #     S0((), Option<<T0>::State>),
//! #     S1((<T0>::Output,), Option<<T1>::State>),
//! #     S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
//! #     S3((<T0>::Output, <T1>::Output, <T2>::Output), Option<<T3>::State>),
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
//!         loop {
//!             {
//!                 #[allow(unused_variables)]
//!                 if let Self::State::S0(output, inner_state) = state {
//!                     let matched = self.tuple.0.extract(inner_state);
//!                     let output = (matched,);
//!                     {
//!                         state = Self::State::S1(output, None);
//!                     }
//!                 }
//!             }
//!             {
//!                 if let Self::State::S1(output, inner_state) = state {
//!                     let matched = self.tuple.1.extract(inner_state);
//!                     let output = (output.0, matched);
//!                     {
//!                         state = Self::State::S2(output, None);
//!                     }
//!                 }
//!             }
//!             {
//!                 if let Self::State::S2(output, inner_state) = state {
//!                     let matched = self.tuple.2.extract(inner_state);
//!                     let output = (output.0, output.1, matched);
//!                     {
//!                         return output;
//!                     }
//!                 }
//!             }
//!             break;
//!         }
//!         unreachable!();
//!     }
//! }
//! ```
//!
//! # Limitations
//!
//! - The typle trait bound (`Tuple` in the examples) can only be applied to an
//!   unqualified type identifier, not to non-path types or associated types.
//! - `typle` does not work when the tuple types are only associated types
//!   because [associated types cannot distinguish implementations](https://github.com/rust-lang/rust/issues/20400).
//!   See [this file](https://github.com/jongiddy/typle/blob/main/tests/compile/unzip.rs)
//!   for workarounds.
//! ```rust ignore
//! // ERROR: conflicting implementations of trait `TryUnzip`
//! # use typle::typle;
//! # trait TryUnzip {}
//! #[typle(Tuple for 2..=3)]
//! impl<I, T, E> TryUnzip for I
//! where
//!     I: Iterator<Item = Result<T, E>>, // T only appears as associated type of Self
//!     T: Tuple,
//! {}
//! ```
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
//! Explicit lifetimes are also required for methods bound by a typle trait
//! inside an impl that is not bound by a typle trait:
//! ```rust
//! # use typle::typle;
//! # struct A {}
//! #[typle(Tuple for 1..=3)]
//! impl A {
//!     fn identity<'a, T: Tuple>(&'a self, t: &'a T) -> &'a T {
//!         t
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
//! - Due to interaction of `typle` with other macros, passing some types and
//!   expressions to a macro may produce unexpected results. To help work around
//!   this, inside a macro invocation the `typle_ty!` macro expands types and the
//!   `typle_expr!` macro expands expressions.
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
mod iterator;
mod syn_ext;

use constant::evaluate_usize;
use context::TypleContext;
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::ToTokens;
use syn::spanned::Spanned as _;
use syn::{Error, Item, Type, TypeNever};

#[doc(hidden)]
#[proc_macro_attribute]
pub fn typle(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let typle_macro = match TypleMacro::try_from(TokenStream::from(args)) {
        Ok(typle_macro) => typle_macro,
        Err(err) => {
            return err.into_compile_error().into();
        }
    };

    let item = match syn::parse2::<Item>(item.into()) {
        Ok(item) => item,
        Err(err) => {
            return err.into_compile_error().into();
        }
    };

    let context = TypleContext::from(&typle_macro);

    let mut items = Vec::new();

    if let Err(err) = context.replace_item(item, &mut items) {
        return err.into_compile_error().into();
    }

    items
        .into_iter()
        .map(Item::into_token_stream)
        .collect::<TokenStream>()
        .into()
}

struct TypleMacro {
    ident: Ident,
    min_len: usize,
    max_len: usize,
    never_type: Type,
}

impl TryFrom<TokenStream> for TypleMacro {
    type Error = Error;

    fn try_from(args: TokenStream) -> Result<Self, Self::Error> {
        // #[typle(Tuple for 2..=12, never=std::convert::Infallible)]
        let default_span = args.span();
        let mut never_type = Type::Never(TypeNever {
            bang_token: syn::token::Not::default(),
        });
        let mut args_iter = args.into_iter();
        // Tuple
        let Some(TokenTree::Ident(trait_ident)) = args_iter.next() else {
            return Err(Error::new(default_span, "expected identifier"));
        };
        // for
        match args_iter.next() {
            Some(TokenTree::Ident(for_ident)) if for_ident == "for" => {}
            _ => {
                return Err(Error::new(default_span, "expected for keyword"));
            }
        }

        let mut range_tokens = TokenStream::new();
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
                range_tokens.extend([token]);
            }
        }
        // 2..=12
        let range = syn::parse2::<syn::ExprRange>(range_tokens)?;
        let min = range
            .start
            .as_ref()
            .map(|expr| {
                evaluate_usize(expr).ok_or_else(|| Error::new(expr.span(), "range start invalid"))
            })
            .transpose()?
            .ok_or_else(|| Error::new(range.span(), "range start must be bounded"))?;
        let end = range
            .end
            .as_ref()
            .ok_or_else(|| Error::new(range.span(), "range end must be bounded"))?;
        let max = match range.limits {
            syn::RangeLimits::HalfOpen(_) => evaluate_usize(end)
                .ok_or_else(|| Error::new(end.span(), "range end invalid"))?
                .checked_sub(1)
                .ok_or_else(|| Error::new(end.span(), "range end invalid"))?,
            syn::RangeLimits::Closed(_) => {
                evaluate_usize(end).ok_or_else(|| Error::new(end.span(), "range end invalid"))?
            }
        };
        if max < min {
            return Err(Error::new(range.span(), "range contains no values"));
        }
        if !never_tokens.is_empty() {
            // never=some::Type
            let mut iter = never_tokens.into_iter();
            let Some(TokenTree::Ident(ident)) = iter.next() else {
                return Err(Error::new(default_span, "expected identifier after comma"));
            };
            if ident != "never" {
                return Err(Error::new(
                    default_span,
                    "expected identifier 'never' after comma",
                ));
            }
            let Some(TokenTree::Punct(punct)) = iter.next() else {
                return Err(Error::new(default_span, "expected equals after never"));
            };
            if punct.as_char() != '=' {
                return Err(Error::new(default_span, "expected equals after never"));
            }
            let type_stream = iter.collect();
            never_type = syn::parse2::<Type>(type_stream)?;
        }
        Ok(TypleMacro {
            ident: trait_ident,
            min_len: min,
            max_len: max,
            never_type,
        })
    }
}

/// Short-circuiting check that all values are true.
///
/// ```rust
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// fn all_long<T: Tuple<&str>>(t: T) -> bool {
///     typle_all!(i in .. => t[[i]].len() > 5)
/// }
/// // Return `true` if all words meet the criteria.
/// assert_eq!(all_long(("longest", "phrase")), true);
/// // Return `false` if any words fail to meet the criteria.
/// assert_eq!(all_long(("the", "longest", "word")), false);
/// // Return `true` for an empty tuple as no words fail to meet the criteria.
/// assert_eq!(all_long(()), true);
/// ```
///
#[proc_macro]
pub fn typle_all(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_all macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}

/// Short-circuiting check that any values are true.
///
/// ```rust
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// fn any_long<T: Tuple<&str>>(t: T) -> bool {
///     typle_any!(i in .. => t[[i]].len() > 5)
/// }
/// // Return `true` if any word meets the criteria.
/// assert_eq!(any_long(("the", "longest", "word")), true);
/// // Return `false` if no words meet the criteria.
/// assert_eq!(any_long(("short", "words")), false);
/// // Return `false` for an empty tuple as no words meet the criteria.
/// assert_eq!(any_long(()), false);
/// ```
///
#[proc_macro]
pub fn typle_any(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_any macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}

/// Reduce a tuple to a single value.
///
/// The `typle_fold!` macro repeatedly applies an expression to an accumulator
/// to collect tuple components into a single value. The macro starts with an
/// initial value. It then loops through a typle index variable, modifying an
/// accumulator with an expression on each iteration.
///
/// Examples:
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// pub fn sum<T: Tuple<u32>>(t: T) -> u32 {
///     typle_fold!(0; i in .. => |total| total + t[[i]])
/// }
/// // An empty tuple uses the initial value.
/// assert_eq!(sum(()), 0);
/// // Otherwise the accumulator is passed to the expression to create a new
/// // value, which is then passed to the next iteration.
/// assert_eq!(sum((1, 4, 9, 16)), 30);
/// ```
///
/// The name of the accumulator is provided between vertical bars (`|total|`)
/// followed by the expression. This makes it look similar to a closure, which
/// it usually acts like. But there are some differences:
/// - the "closure parameter" naming the accumulator can only contain a single
///   identifier;
/// - a `break` in the expression terminates the fold early with the value of
///   the `break`;
/// - a `return` in the expression returns from the enclosing function (since
///   the expression is not actually in a closure).
///
/// The previous example could have been implemented using a `for` loop.
/// However, unlike a `for` loop, the `typle_fold!` macro allows the accumulator
/// to change type on each iteration.
///
/// In the next example, the type of the accumulator after each iteration is a
/// tuple with one extra component from the prior iteration. The `..=` range
/// provides an extra iteration that wraps the tuple in `Some`.
///
/// ```rust
/// # use typle::typle;
/// trait CoalesceSome<T> {
///     type Output;
///
///     /// Coalesce a tuple of Options into an Option of tuple that is `Some`
///     /// only if all the components are `Some`.
///     fn coalesce_some(self) -> Option<Self::Output>;
/// }
///
/// #[typle(Tuple for 0..=12)]
/// impl<T: Tuple> CoalesceSome<T> for typle_for!(i in .. => Option<T<{i}>>) {
///     type Output = T;
///
///     fn coalesce_some(self) -> Option<Self::Output>
///     {
///         typle_fold!(
///             ();  // Initially an empty tuple
///             i in ..=T::LEN => |acc| if typle_const!(i == T::LEN) {
///                 // Final iteration: wrap accumulated tuple in `Some`
///                 Some(acc)
///             } else if let Some(curr) = self[[i]] {
///                 // Append the current value to the prior tuple to create a
///                 // new accumulator with the type `Some(T<0>,...,T<{i}>)`
///                 typle_for!{j in ..=i => if j < i { acc[[j]] } else { curr }}
///             } else {
///                 // If `None` is found at any point, short-circuit with a `None` result
///                 break None;
///             }
///         )
///     }
/// }
/// assert_eq!(
///     ().coalesce_some(),
///     Some(())
/// );
/// assert_eq!(
///     (Some(1), Some("x")).coalesce_some(),
///     Some((1, "x"))
/// );
/// assert_eq!(
///     (None::<i32>, Some("x")).coalesce_some(),
///     None::<(i32, &str)>
/// );
/// ```
///
/// The `typle_fold!` macro can also be used for recursive types. See the
/// [example in the tests].
///
/// [example in the tests]: https://github.com/jongiddy/typle/blob/main/tests/compile/typle_fold.rs
#[proc_macro]
pub fn typle_fold(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_fold macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}

/// Create variants in an enum.
///
/// In an enum, the `typle_variant` macro allows the creation of variants for each component.
///
/// A variant is created for each index in the range provided.
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
/// #     type Output;
/// #     type State;
/// # }
/// #[typle(Tuple for 0..=2)]
/// pub enum ProcessState<T>
/// where
///     T: Tuple,
///     T<_>: Process<Output = u64>,
/// {
///     S = typle_variant!(i in ..T::MAX => Option<T<{i}>::State>, [u64; i]),
///     U = typle_variant!{i in ..Tuple::MAX => u: [u32; i]},
///     V = typle_variant![..Tuple::MAX],
///     Done([u64; Tuple::MAX]),
/// }
/// ```
/// creates
/// ```
/// # trait Process {
/// #     type Output;
/// #     type State;
/// # }
/// pub enum ProcessState<T0, T1>
/// where
///     T0: Process<Output = u64>,
///     T1: Process<Output = u64>,
/// {
///     S0(Option<<T0>::State>, [u64; 0]),
///     S1(Option<<T1>::State>, [u64; 1]),
///     U0 { u: [u32; 0] },
///     U1 { u: [u32; 1] },
///     V0,
///     V1,
///     Done([u64; 2]),
/// }
/// ```
#[proc_macro]
pub fn typle_variant(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_variant macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}
