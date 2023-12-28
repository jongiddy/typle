# typle

The `typle` crate provides a macro to create items for tuples of
multiple lengths.

For example it can define a function, for tuples up to length 12, to zip a pair
of tuples into a tuple of pairs:

```rust
#[typle(Tuple for 0..=12)]
pub fn zip<A: Tuple, B: Tuple>(
    first: A,
    second: B
) -> typle_for!(i in .. => (A<{i}>, B<{i}>))
{
    typle_for!(i in .. => (first[[i]], second[[i]]))
}

let s = ("LHR", "FCO", "ZRH");
let t = (51.5, 41.8, 47.5);
assert_eq!(
    zip(s, t),
    (("LHR", 51.5), ("FCO", 41.8), ("ZRH", 47.5))
);
assert_eq!(zip((), ()), ());
```

The `Hash` trait for tuples hashes each component of the tuple.

Using `typle` this can be written as:

```rust
impl Hash for () {
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

#[typle(Tuple for 1..=12)]
impl<T> Hash for T
where
    T: Tuple,                 // `T` must be a tuple with 1-12 components.
    T<_>: Hash,               // Each component must implement `Hash`.
    T<{T::LEN - 1}>: ?Sized,  // The last component may be unsized.
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        for typle_const!(i) in 0..T::LEN {
            self[[i]].hash(state);
        }
    }
}
```

Compare the `typle` implementation above to the current implementation
(excluding docs) in the standard library:

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

See the [full documentation](https://docs.rs/typle/) for more examples.
