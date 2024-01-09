// Implement the Hash trait for tuples, equivalent to:
// https://github.com/rust-lang/rust/blob/be69926a731/library/core/src/hash/mod.rs#L870-L926

use typle::typle;

impl Hash for () {
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

#[typle(Tuple for 1..=12)]
impl<T> Hash for T
where
    T: Tuple,   // `T` must be a tuple with 1-12 components.
    T<_>: Hash, // Each component must implement `Hash`.
    T<{ T::LEN - 1 }>: ?Sized, // The last component may be unsized.
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        for typle_const!(i) in 0..T::LEN {
            self[[i]].hash(state);
        }
    }
}
