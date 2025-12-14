use std::ops::{Add, Mul};

use typle::typle;

#[typle(Tuple for 0..=3)]
fn duplicate_components<T: Tuple>(
    t: T,
) -> (typle! {
        i in .. => T<{i}>, T<{i}>
    })
where
    T<_>: Clone,
{
    (typle! {
        i in .. => t[[i]].clone(), t[[i]]
    })
}

#[typle(Tuple for 0..=3)]
fn sum_and_product<S: Tuple, T: Tuple>(
    s: S,
    t: T,
) -> (typle! {i in .. => <S<{i}> as Add<T<{i}>>>::Output, <S<{i}> as Mul<T<{i}>>>::Output})
where
    S<_>: Copy,
    T<_>: Copy,
    typle!(i in .. => S<{i}>: Add<T<{i}>> + Mul<T<{i}>>): Tuple::Bounds,
{
    (typle! {i in .. => s[[i]] + t[[i]], s[[i]] * t[[i]]})
}
