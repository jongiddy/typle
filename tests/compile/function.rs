use typle::typle;

use std::hash::{Hash, Hasher};
use std::ops::Mul;

// Implementation of standalone functions requires explicit lifetimes.
#[typle(Tuple for 1..=3)]
pub fn hash<'a, T, S: Hasher>(tuple: &'a T, state: &'a mut S)
where
    T: Tuple,
    T<_>: Hash,
    T<{ T::LAST }>: ?Sized,
{
    for typle_index!(i) in 0..T::LEN {
        tuple[[i]].hash(state);
    }
}

#[typle(Tuple for 0..=3)]
#[typle_attr_if(Tuple::LEN == 0, allow(unused_assignments))]
fn multiply<T, M>(t: T, m: M) -> (typle!(i in .. => <T<{i}> as Mul<M>>::Output))
where
    T: Tuple,
    T<_>: Mul<M>,
    M: Copy,
{
    (typle!(i in .. => t[[i]] * m))
}

// `heapify` and `zip` as described at
// https://gist.github.com/soqb/9ce3d4502cc16957b80c388c390baafc#syntax-for-type-transformations
#[typle(Tuple for 0..=3)]
#[typle_attr_if(Tuple::LEN == 0, allow(unused_assignments))]
fn heapify<T: Tuple>(params: T) -> (typle!(i in .. => Box<T<{i}>>)) {
    (typle!(i in .. => Box::new(params[[i]])))
}

#[allow(non_camel_case_types)]
#[typle(Tuple for 0..=5, suffixes=["", "01", "x", "three"])]
#[typle_attr_if(Tuple::LEN == 0, allow(unused_assignments))]
pub fn zip<A: Tuple, B: Tuple>(
    first: A,
    second: B,
) -> (typle!(i in ..Tuple::LEN => (A<{i}>, B<{i}>))) {
    (typle!(i in ..Tuple::LEN => (first[[i]], second[[i]])))
}

#[typle(Tuple for 0..=3)]
#[typle_attr_if(Tuple::LEN == 0, allow(unused_assignments))]
pub fn double<T: Tuple<u32>>(t: T) -> T {
    (typle!(i in ..Tuple::LEN => t[[i]] * 2))
}
