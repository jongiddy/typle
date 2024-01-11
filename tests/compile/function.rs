use typle::typle;

use std::hash::{Hash, Hasher};
use std::ops::Mul;

// Implementation of standalone functions requires explicit lifetimes.
#[typle(Tuple for 1..=3)]
pub fn hash<'a, T, S: Hasher>(tuple: &'a T, state: &'a mut S)
where
    T: Tuple,
    T<_>: Hash,
    T<{ T::LEN - 1 }>: ?Sized,
{
    for typle_index!(i) in 0..T::LEN {
        tuple[[i]].hash(state);
    }
}

#[typle(Tuple for 0..=3)]
fn multiply<T, M>(t: T, m: M) -> typle_for!(i in .. => <T<{i}> as Mul<M>>::Output)
where
    T: Tuple,
    T<_>: Mul<M>,
    M: Copy,
{
    typle_for!(i in .. => t[[i]] * m)
}

// `heapify` and `zip` as described at
// https://gist.github.com/soqb/9ce3d4502cc16957b80c388c390baafc#syntax-for-type-transformations
#[typle(Tuple for 0..=3)]
fn heapify<T: Tuple>(params: T) -> typle_for!(i in .. => Box<T<{i}>>) {
    typle_for!(i in .. => Box::new(params[[i]]))
}

#[rustfmt::skip]
#[typle(Tuple for 0..=12)]
pub fn zip<A: Tuple, B: Tuple>(
    first: A,
    second: B
) -> typle_for!(i in .. => (A<{i}>, B<{i}>))
{
    typle_for!(i in .. => (first[[i]], second[[i]]))
}
