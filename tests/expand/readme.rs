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

#[typle(Tuple for 1..=3)]
impl<T, C> MyStruct<T>
where
    T: Tuple<Types=C>,
    C: AddAssign + Default + Copy,
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

pub trait Extract {
    type State;
    type Output;
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
}
