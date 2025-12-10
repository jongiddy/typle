# typle

The `typle` crate provides the ability to constrain generic arguments to be
tuples and supports manipulation of the tuple components.

For example, to define a function to zip a pair of tuples into a tuple of pairs:

```rust
#[typle(Tuple for 0..=12)]
pub fn zip<A: Tuple, B: Tuple>(
    a: A,
    b: B,
) -> (typle![i in .. => (A<{i}>, B<{i}>)])
{
    (typle!{i in .. => (a[[i]], b[[i]])})
}
```

The types `A` and `B` are generic but are constrained to be tuples. The tuples
can have 0 to 12 components of any (sized) type, but both tuples must have the
same length.

The `typle!` macro loops over an index returning a new tuple with the
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

```rust
trait HandleStuff {
    type Input;
    type Output;

    fn handle_stuff(&self, input: Self::Input) -> Self::Output;
}

struct MultipleHandlers<T> {
    handlers: T,
}

#[typle(Tuple for 1..=12)]
impl<T: Tuple, I: Clone> HandleStuff for MultipleHandlers<T>
where
    T<_>: HandleStuff<Input = I>,
{
    type Input = I;
    type Output = (typle!(i in .. => T<{i}>::Output));

    fn handle_stuff(&self, input: Self::Input) -> Self::Output {
        (
            typle!(i in ..T::LAST => self.handlers[[i]].handle_stuff(input.clone())),
            // Avoid expensive clone for the last handler.
            self.handlers[[T::LAST]].handle_stuff(input),
        )
    }
}
```

See the [crate documentation](https://docs.rs/typle/) for more examples.
