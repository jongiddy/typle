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

use std::ops::Mul;

#[typle(Tuple for 1..=3)]
impl<T> MyStruct<T>
where
    T: Tuple,
    T::Types: Copy,
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
        T::Types<i>: Mul<M<{i}>>,
    {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]]).into()
    }
}

#[typle(Tuple for 1..=3)]
impl<T, C> MyStruct<T>
where
    T: Tuple<Types=C>,
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

pub trait Extract {
    type State;
    type Output;

    fn extract(&self, state: Option<Self::State>) -> Self::Output;
}

#[typle(Tuple for 1..=3)]
pub enum TupleSequenceState<T>
where
    T: Tuple,
    T::Types: Extract,
{
    S = typle_variant!(i in .. =>
        typle_for!(j in ..i => T::<{j}>::Output),
        Option<T<{i}>::State>
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

    fn extract(&self, state: Option<Self::State>) -> Self::Output {
        let mut state = state.unwrap_or(Self::State::S::<typle_index!(0)>((), None));
        for typle_const!(i) in 0..T::LEN {
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
