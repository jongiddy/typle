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
//! The [`typle_fold!`] macro reduces a tuple to a single value.
//!
//! ```rust
//! # use typle::typle;
//! #[typle(Tuple for 0..=12)]
//! pub fn sum<T: Tuple<u32>>(t: T) -> u32 {
//!     typle_fold!(0; i in ..T::LEN => |total| total + t[[i]])
//! }
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
//! arbitrary expressions using the typle index variable on both sides of the
//! colon, as shown below:
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
//!             // When LEN == 1 the code never changes `state`
//!             #[typle_attr_if(T::LEN == 1, allow(unused_mut))]
//!             let mut state = state.unwrap_or(Self::State::S::<typle_ident!(0)>((), None));
//!             for typle_index!(i) in 0..T::LEN {
//!                 // When i == 0, the `output` state variable does not get used
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
//!         loop {
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
//!             break;
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
                evaluate_usize(&expr).ok_or_else(|| Error::new(expr.span(), "range start invalid"))
            })
            .transpose()?
            .ok_or_else(|| Error::new(range.span(), "range start must be bounded"))?;
        let end = range
            .end
            .as_ref()
            .ok_or_else(|| Error::new(range.span(), "range end must be bounded"))?;
        let max = match range.limits {
            syn::RangeLimits::HalfOpen(_) => evaluate_usize(&end)
                .and_then(|max| max.checked_sub(1))
                .ok_or_else(|| Error::new(end.span(), "range end invalid1"))?,
            syn::RangeLimits::Closed(_) => {
                evaluate_usize(&end).ok_or_else(|| Error::new(end.span(), "range end invalid2"))?
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

/// Reduce a tuple to a single value.
///
/// The `typle_fold!` macro applies a closure to collect tuple components into a
/// single value. The arguments to the macro are an initial value terminated by
/// a semi-colon and a closure to apply to the current value.
///
/// Examples:
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// pub fn sum<T: Tuple<u32>>(t: T) -> u32 {
///     typle_fold!(0; i in ..T::LEN => |total| total + t[[i]])
/// }
/// // An empty tuple uses the initial value.
/// assert_eq!(sum(()), 0);
/// // Otherwise the value is passed to the closure to create a new value, which
/// // is then passed to the next iteration.
/// assert_eq!(sum((1, 4, 9, 16)), 30);
/// ```
/// This example can also be implemented using a `for` loop:
/// ```rust
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// pub fn sum<T: Tuple<u32>>(t: T) -> u32 {
///     let mut total = 0;
///     for typle_index!(i) in 0..T::LEN {
///         total += t[[i]];
///     }
///     total
/// }
/// # assert_eq!(sum(()), 0);
/// # assert_eq!(sum((1, 4, 9, 16)), 30);
/// ```
///
/// But the `typle_fold!` macro allows the accumulated value to change type on
/// each iteration. In this example, the type of the accumulator after each
/// iteration is an `Option` containing `i + 1` components.
///
/// ```rust
/// # use typle::typle;
/// trait CoalesceSome<T> {
///     type Output;
///
///     /// Coalesce a tuple of options into an option of tuple that is `Some` only
///     /// if all the components are `Some`.
///     fn coalesce_some(self) -> Self::Output;
/// }
///
/// #[typle(Tuple for 0..=12)]
/// impl<T> CoalesceSome<T> for typle_for!(i in ..T::LEN => Option<T<{i}>>)
/// where
///     T: Tuple,
///     T<_>: Copy,
/// {
///     type Output = Option<T>;
///
///     fn coalesce_some(self) -> Self::Output
///     {
///         typle_fold!(
///             Some(());  // Initially an empty tuple
///             i in ..T::LEN => |opt| opt.and_then(
///                 |_prev| if let Some(curr) = self[[i]] {
///                     // Append the current value to the existing tuple
///                     Some(typle_for!(
///                         j in ..=i => if typle_const!(j < i) { _prev[[j]] } else { curr }
///                     ))
///                 } else {
///                     None
///                 }
///             )
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
///     None
/// );
/// ```
#[proc_macro]
pub fn typle_fold(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_fold macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}

/// Create a tuple or array.
///
/// Loop over the indices of the tuple, returning an expression or type for each index.
///
/// The `typle_for!` macro behavior depends on the delimiters around the macro tokens.
///
/// With parentheses, `typle_for!` creates a new tuple type, pattern, or expression.
///
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// pub fn reverse<T: Tuple>(t: T) -> typle_for!(i in 1..=T::LEN => T<{T::LEN - i}>) {
///     typle_for!(i in 1..=T::LEN => t[[T::LEN - i]])
/// }
///
/// assert_eq!(reverse((Some(3), "four", 5)), (5, "four", Some(3)));
/// ```
///
/// With braces, `typle_for!` expects an expression that evaluates to the components
/// of the tuple. Use the `typle_ty!` or `typle_pat!` macros where the type or pattern
/// is not a valid expression.
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// fn append<T: Tuple, A>(
///     t: T,
///     a: A,
/// ) -> typle_for!{i in 0..=T::LEN => if typle_const!(i < T::LEN) {typle_ty!(T<{i}>)} else {A}} {
///     typle_for!(i in 0..=T::LEN => if typle_const!(i < T::LEN) {t[[i]]} else {a})
/// }
///
/// assert_eq!(append((1, 2, 3), 4), (1, 2, 3, 4));
/// ```
///
/// In a braced `typle_for!` components can be filtered using a const-if expression with no `else` clause.
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// fn split_components<T: Tuple>(
///     t: T
/// ) -> (
///     // (T0, T2,...)
///     typle_for!{i in ..T::LEN => if typle_const!(i % 2 == 0) { typle_ty!(T<{i}>) }},
///     // (T1, T3,...)
///     typle_for!{i in ..T::LEN => if typle_const!(i % 2 == 1) { typle_ty!(T<{i}>) }},
/// ) {
///     (
///         typle_for!{i in ..T::LEN => if typle_const!(i % 2 == 0) { t[[i]] }},
///         typle_for!{i in ..T::LEN => if typle_const!(i % 2 == 1) { t[[i]] }},
///     )
/// }
///
/// assert_eq!(
///     split_components((1, 2, 3, 4, 5, 6)),
///     ((1, 3, 5), (2, 4, 6))
/// );
/// ```
///
/// With brackets, `typle_for!` creates an array expression.
///
/// ```
/// # use typle::typle;
/// #[typle(Tuple for 0..=12)]
/// fn to_strings<T>(t: T) -> [String; T::LEN]
/// where
///     T: Tuple,
///     T<_>: ToString,
/// {
///     typle_for![i in ..T::LEN => t[[i]].to_string()]
/// }
///
/// assert_eq!(
///     to_strings((3, std::io::Error::new(std::io::ErrorKind::Other, "test"))),
///     [String::from("3"), String::from("test")]
/// );
/// ```
#[proc_macro]
pub fn typle_for(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    Error::new_spanned(
        TokenStream::from(item),
        "typle_for macro only available in item with typle attribute",
    )
    .into_compile_error()
    .into()
}

/// Create variants in an enum.
///
/// In an enum, the `typle_variant` macro allows the creation of variants for each component.
///
/// A variant is created for each index in the range provided. The default range is `0..Tuple::MAX`.
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
///     S = typle_variant!(i in .. => Option<T<{i}>::State>, [u64; i]),
///     U = typle_variant! {i in .. => u: [u32; i]},
///     V = typle_variant![..],
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
