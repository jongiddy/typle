# typle

Rust-ic manipulation of tuples.

## An Initial Example

The implementation of the `Hash` trait for tuples simply hashes each component
of the tuple.

In the standard library the implementation (without docs) looks like this:
```rust
macro_rules! impl_hash_tuple {
    () => (
        impl Hash for () {
            #[inline]
            fn hash<H: Hasher>(&self, _state: &mut H) {}
        }
    );

    ( $($name:ident)+) => (
        impl<$($name: Hash),+> Hash for ($($name,)+) where last_type!($($name,)+): ?Sized {
            #[allow(non_snake_case)]
            #[inline]
            fn hash<S: Hasher>(&self, state: &mut S) {
                let ($(ref $name,)+) = *self;
                $($name.hash(state);)+
            }
        }
    );
}

macro_rules! last_type {
    ($a:ident,) => { $a };
    ($a:ident, $($rest_a:ident,)+) => { last_type!($($rest_a,)+) };
}

impl_hash_tuple! {}
impl_hash_tuple! { T }
impl_hash_tuple! { T B }
impl_hash_tuple! { T B C }
impl_hash_tuple! { T B C D }
impl_hash_tuple! { T B C D E }
impl_hash_tuple! { T B C D E F }
impl_hash_tuple! { T B C D E F G }
impl_hash_tuple! { T B C D E F G H }
impl_hash_tuple! { T B C D E F G H I }
impl_hash_tuple! { T B C D E F G H I J }
impl_hash_tuple! { T B C D E F G H I J K }
impl_hash_tuple! { T B C D E F G H I J K L }
```

Using `typle` the same implementation can be made shorter and clearer:
```rust
impl Hash for () {
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

#[typle(Tuple for 1..=12)]
impl<T> Hash for T
where
    T: Tuple,
    T<_>: Hash,
    T<{T::LEN - 1}>: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        for typle_const!(i) in 0..T::LEN {
            self[[i]].hash(state);
        }
    }
}
```

## A Quick Introduction

The `typle!` macro can generate trait implementations for multiple tuple lengths:

```rust
use typle::typle;

struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 0..=3)]
impl<T> From<T> for MyStruct<T>
where
    T: Tuple,
{
    fn from(t: T) -> Self {
        MyStruct { t }
    }
}
```

This generates implementations of the `From` trait for tuples with 0 to 3 components:
```rust
impl From<()> for MyStruct<()> {
    fn from(t: ()) -> Self {
        MyStruct { t }
    }
}

impl<T0> From<(T0,)> for MyStruct<(T0,)> {
    fn from(t: (T0,)) -> Self {
        MyStruct { t }
    }
}

impl<T0, T1> From<(T0, T1)> for MyStruct<(T0, T1)> {
    fn from(t: (T0, T1)) -> Self {
        MyStruct { t }
    }
}

impl<T0, T1, T2> From<(T0, T1, T2)> for MyStruct<(T0, T1, T2)> {
    fn from(t: (T0, T1, T2)) -> Self {
        MyStruct { t }
    }
}
```

Inside `typle` code, select individual components of a tuple using `<{i}>` for
types and `[[i]]` for values.

The `typle_for!` macro creates a new tuple type or value.

In `where` clauses, `<_>` refers to each component type of the tuple. The
`typle_bound!` macro is more general and allows the iteration value to be used
in the trait bounds.

```rust
use std::ops::Mul;

#[typle(Tuple for 1..=3)]
impl<T> MyStruct<T>
where
    T: Tuple,
    T<_>: Copy,
{
    // Return a tuple containing all components except the first
    fn tail(&self) -> typle_for!(i in 1.. => T<{i}>) {
        typle_for!(i in 1.. => self.t[[i]])
    }

    // Return a MyStruct containing the product of the components of two tuples
    fn multiply<M>(
        &self, multipliers: M
    ) -> MyStruct<typle_for!(i in .. => <T<{i}> as Mul<M<{i}>>>::Output)>
    where
        M: Tuple,
        typle_bound!(i in .. => T<{i}>): Mul<M<{i}>>,
    {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]]).into()
    }
}
```

Generated implementation for 3-tuples:

```rust
impl<T0, T1, T2> MyStruct<(T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    fn tail(&self) -> (T1, T2) {
        (self.t.1, self.t.2)
    }

    fn multiply<M0, M1, M2>(
        &self,
        multipliers: (M0, M1, M2),
    ) -> MyStruct<(
        <T0 as Mul<M0>>::Output,
        <T1 as Mul<M1>>::Output,
        <T2 as Mul<M2>>::Output,
    )>
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
        T2: Mul<M2>,
    {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2
        ).into()
    }
}
```

The typle trait can take a type parameter when all components have the same type.

The associated constant `LEN` provides the length of the tuple in each generated
item.

Use the `typle_const!` macro to perform const-for, iterating over a block of
statements.

```rust
#[typle(Tuple for 1..=3)]
impl<T, C> MyStruct<T>
where
    T: Tuple<C>,
    C: for<'a> std::ops::AddAssign<&'a C> + Default,
{
    // Return a reference to the last component of the tuple
    fn last(&self) -> &T<{T::LEN - 1}> {
        &self.t[[T::LEN - 1]]
    }

    // Return the sums of all even positions and all odd positions
    fn interleave(&self) -> (C, C) {
        let mut even_odd = (C::default(), C::default());
        for typle_const!(i) in 0..T::LEN {
            even_odd[[i % 2]] += &self.t[[i]];
        }
        even_odd
    }
}
```

Generated implementation for 3-tuples:

```rust
impl<C> MyStruct<(C, C, C)>
where
    C: for<'a> std::ops::AddAssign<&'a C> + Default,
{
    fn last(&self) -> &C {
        &self.t.2
    }

    fn interleave(&self) -> (C, C) {
        let mut even_odd = (C::default(), C::default());
        {
            {
                even_odd.0 += &self.t.0;
            }
            {
                even_odd.1 += &self.t.1;
            }
            {
                even_odd.0 += &self.t.2;
            }
            ()
        }
        even_odd
    }
}
```

The `typle` macro can be applied to items other than `impl`s. A standalone
`zip` function to pair up the components of two tuples:
```rust
#[typle(Tuple for 1..=3)]
pub fn zip<A, B>(a: A, b: B) -> typle_for!(i in .. => (A<{i}>, B<{i}>))
where
    A: Tuple,
    B: Tuple,
{
    typle_for!(i in .. => (a[[i]], b[[i]]))
}
```

The generated code uses an extra trait to achieve a limited form of overloading:
```rust
#[allow(non_camel_case_types)]
trait _typle_fn_zip {
    type Return;
    fn apply(self) -> Self::Return;
}

pub fn zip<A, B>(a: A, b: B) -> <(A, B) as _typle_fn_zip>::Return
where
    (A, B): _typle_fn_zip,
{
    <(A, B) as _typle_fn_zip>::apply((a, b))
}

impl<A0, A1, A2, B0, B1, B2> _typle_fn_zip for ((A0, A1, A2), (B0, B1, B2)) {
    type Return = ((A0, B0), (A1, B1), (A2, B2));
    fn apply(self) -> Self::Return {
        let (a, b) = self;
        { ((a.0, b.0), (a.1, b.1), (a.2, b.2)) }
    }
}
```

The following example, simplified from code in the `hefty` crate, shows `typle`
applied to an `enum` using the `typle_variant!` macro. Note the use of `T<{..}>`
and `typle_index!` when referring to a `typle` `struct` or `enum`. This is
required because these items require a numeric suffix: `TupleSequenceState0`,
`TupleSequenceState1`,...

The `typle_const!` macro also supports const-if on an expression that evaluates
to a `bool`. const-if allows branches that do not compile, as long as they are
`false` at compile-time. For example, this code compiles when `i + 1 == T::LEN`
even though the state `S::<typle_index!(i + 1)>` (`S3` for 3-tuples) is not
defined.

```rust
#[typle(Tuple for 1..=3)]
mod tuple {
    pub trait Extract {
        type State;
        type Output;

        fn extract(&self, state: Option<Self::State>) -> Self::Output;
    }

    pub enum TupleSequenceState<T>
    where
        T: Tuple,
        T<_>: Extract,
    {
        S = typle_variant!(i in .. =>
            typle_for!(j in ..i => T::<{j}>::Output),
            Option<T<{i}>::State>
        ),
    }

    pub struct TupleSequence<T> {
        tuple: T,
    }

    impl<T> Extract for TupleSequence<T>
    where
        T: Tuple,
        T<_>: Extract,
    {
        type State = TupleSequenceState<T<{..}>>;
        type Output = typle_for!(i in .. => T<{i}>::Output);

        fn extract(&self, state: Option<Self::State>) -> Self::Output {
            #[allow(unused_mut)]  // For LEN = 1 `state` is never mutated
            let mut state = state.unwrap_or(Self::State::S::<typle_index!(0)>((), None));
            for typle_const!(i) in 0..T::LEN {
                // For LEN = 1 there is only one state and the initial `output` variable is unused
                #[allow(irrefutable_let_patterns, unused_variables)]
                if let Self::State::S::<typle_index!(i)>(output, inner_state) = state {
                    let matched = self.tuple[[i]].extract(inner_state);
                    let output = typle_for!(j in ..=i =>
                        if typle_const!(j != i) { output[[j]] } else { matched }
                    );
                    if typle_const!(i + 1 == T::LEN) {
                        return output;
                    } else {
                        state = Self::State::S::<typle_index!(i + 1)>(output, None);
                    }
                }
            }
            unreachable!();
        }
    }
}
```

Generated implementation for 3-tuples:
```rust
pub enum TupleSequenceState3<T0, T1, T2>
where
    T0: Extract,
    T1: Extract,
    T2: Extract,
{
    S0((), Option<<T0>::State>),
    S1((<T0>::Output,), Option<<T1>::State>),
    S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
}

impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
where
    T0: Extract,
    T1: Extract,
    T2: Extract,
{
    type State = TupleSequenceState3<T0, T1, T2>;
    type Output = (<T0>::Output, <T1>::Output, <T2>::Output);
    fn extract(&self, state: Option<Self::State>) -> Self::Output {
        #[allow(unused_mut)]
        let mut state = state.unwrap_or(Self::State::S0((), None));
        {
            {
                #[allow(irrefutable_let_patterns, unused_variables)]
                if let Self::State::S0(output, inner_state) = state {
                    let matched = self.tuple.0.extract(inner_state);
                    let output = ({ matched },);
                    {
                        state = Self::State::S1(output, None);
                    }
                }
            }
            {
                #[allow(irrefutable_let_patterns, unused_variables)]
                if let Self::State::S1(output, inner_state) = state {
                    let matched = self.tuple.1.extract(inner_state);
                    let output = ({ output.0 }, { matched });
                    {
                        state = Self::State::S2(output, None);
                    }
                }
            }
            {
                #[allow(irrefutable_let_patterns, unused_variables)]
                if let Self::State::S2(output, inner_state) = state {
                    let matched = self.tuple.2.extract(inner_state);
                    let output = ({ output.0 }, { output.1 }, { matched });
                    {
                        return output;
                    }
                }
            }
            ()
        }
        unreachable!();
    }
}
```
