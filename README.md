# typle

A Rust macro to create items for different sized tuples.

The `typle!` macro can generate trait implementations for multiple-arity tuples:

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

This generates implementations for tuples with 0 to 3 components:
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

Iterate over tuple components using the `typle_for!` macro.
Select individual components using `<{i}>` for types and `[[i]]` for values.

```rust
use std::ops::{AddAssign, Mul};

#[typle(Tuple for 1..=3)]
impl<T> MyStruct<T>
where
    T: Tuple,
    T::Types: Copy,
{
    fn tail(&self) -> MyStruct<typle_for!(i in 1.. => T<{i}>)> {
        typle_for!(i in 1.. => self.t[[i]]).into()
    }

    fn multiply<M>(
        &self, multipliers: M
    ) -> typle_for!(i in .. => <T<{i}> as Mul<M<{i}>>>::Output)
    where
        M: Tuple,
        T<{i}>: Mul<M<{i}>>,
    {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]])
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
    fn tail(&self) -> MyStruct<(T1, T2)> {
        (self.t.1, self.t.2).into()
    }

    fn multiply<M0, M1, M2>(
        &self,
        multipliers: (M0, M1, M2),
    ) -> (<T0 as Mul<M0>>::Output, <T1 as Mul<M1>>::Output, <T2 as Mul<M2>>::Output)
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
        T2: Mul<M2>,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1, self.t.2 * multipliers.2)
    }
}
```

Use the `typle_const!` macro to perform const-for and const-if. The associated constant `LEN`
provides the length of the tuple in each generated item.

```rust
#[typle(Tuple for 1..=3)]
impl<T, C> MyStruct<T>
where
    T: Tuple<Types=C>,
    C: std::ops::AddAssign + Default + Copy,
{
    fn interleave(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        for typle_const!(i) in 0..T::LEN {
            if typle_const!(i % 2 == 0) {
                even += self.t[[i]];
            } else {
                odd += self.t[[i]];
            }
        }
        (even, odd)
    }
}
```

Generated implementation for 3-tuples:

```rust
impl<C> MyStruct<(C, C, C)>
where
    C: std::ops::AddAssign + Default + Copy,
{
    fn interleave(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        {
            {
                {
                    even += self.t.0;
                }
            }
            {
                {
                    odd += self.t.1;
                }
            }
            {
                {
                    even += self.t.2;
                }
            }
        }
        (even, odd)
    }
}
```

This example, derived from code in the `hefty` crate, shows `typle` applied to an `enum` using the `typle_variant!` macro.
Note the use of `<T::Types>` and `typle_index!` when referring to another typled item.

```rust
pub trait Extract {
    type State;
    type Output;

    fn extract(&self, state: Option<Self::State>);
}

#[typle(Tuple for 1..=3)]
pub enum TupleSequenceState<T>
where
    T: Tuple,
    T::Types: Extract,
{
    S = typle_variant!(i in .. =>
        typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
    ),
}

pub struct TupleSequence<T> {
    tuple: T,
}

#[typle(Tuple for 1..=3)]
impl<T> Extract for TupleSequence<T>
where
    T: Tuple,
    T::Types: Extract,
{
    type State = TupleSequenceState<T::Types>;
    type Output = typle_for!(i in .. => T<{i}>::Output);

    fn extract(&self, state: Option<Self::State>) {
        let state = state.unwrap_or(Self::State::S::<typle_index!(0)>((), None));
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

    fn extract(&self, state: Option<Self::State>) {
        let state = state.unwrap_or(Self::State::S0((), None));
    }
}
```
