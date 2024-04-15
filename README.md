# typle

The `typle` crate provides the ability to constrain generic arguments to be
tuples and supports manipulation of the tuple components.

For example, to define a function to zip a pair of tuples into a tuple of pairs:

```rust
#[typle(Tuple for 0..=12)]
pub fn zip<A: Tuple, B: Tuple>(
    a: A,
    b: B,
) -> typle_for!(i in .. => (A<{i}>, B<{i}>))
{
    typle_for!(i in .. => (a[[i]], b[[i]]))
}
```

The types `A` and `B` are generic but are constrained to be tuples. The tuples
can have 0 to 12 components of any (sized) type, but both tuples must have the
same length.

The `typle_for!` macro loops over an index returning a new tuple with the
specified components. For the function return type it creates a type tuple:
`((A<0>, B<0>), (A<1>, B<1>),...)`. In the function body it creates a value tuple:
`((a.0, b.0), (a.1, b.1),...)`.

```rust
assert_eq!(
    zip(("LHR", "FCO", "ZRH"), (51.5, 41.8, 47.5)),
    (("LHR", 51.5), ("FCO", 41.8), ("ZRH", 47.5))
);
assert_eq!(
    zip((2.0, "test"), (Some(9u8), ('a', 'b'))),
    ((2.0, Some(9u8)), ("test", ('a', 'b')))
);
assert_eq!(
    zip((), ()),
    ()
);
```

A common use of `typle` is to implement a trait for tuples of multiple lengths.
Compared to using declarative macros, the `typle` code looks more Rust-like and
provides access to individual components and their position.

For example the `Hash` implementation for tuples simply hashes each component of
the tuple in order.

Using `typle` this can be written as:

```rust
#[typle(Tuple for 1..=12)]
impl<T> Hash for T
where
    T: Tuple,  // `T` must be a tuple with 1-12 components.
    T<_>: Hash,  // Each component must implement `Hash`.
    T<{T::LEN - 1}>: ?Sized,  // The last component may be unsized.
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        for typle_index!(i) in 0..T::LEN {
            self[[i]].hash(state);
        }
    }
}
```

Compare this to the current implementation in the standard library:

```rust
macro_rules! impl_hash_tuple {
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

See the [crate documentation](https://docs.rs/typle/) for more examples.
