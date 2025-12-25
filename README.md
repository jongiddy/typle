# typle

The `typle` crate provides the ability to constrain generic arguments to be
tuples and supports manipulation of the tuple components.

It allows you to write a single function or impl that "unrolls" into multiple
concrete versions: one for `(T0,)`, one for `(T0, T1)`, and so on.

With `typle` you can easily define a function to zip a pair of tuples
into a tuple of pairs:

```rust
#[typle(Tuple for 0..=12)]
pub fn zip<A: Tuple, B: Tuple>(a: A, b: B) -> (typle! {
    i in .. => (A<{i}>, B<{i}>)
}) {
    (typle! {
        i in .. => (a[[i]], b[[i]])
    })
}
```

The `#[typle]` attribute macro introduces `Tuple` which acts like a trait within
the annotated item. The types `A` and `B` are generic but are constrained by the
`Tuple` bound to be tuples. The tuples can have 0 to 12 components of any
(sized) type, but both tuples must have the same length.

`A<{i}>` refers to the type of the tuple component at index `i`. `a[[i]]` refers
to the value of the tuple component at index `i` (unrolls to `a.0`, `a.1`,...).

The `typle!` macro loops over a range returning a new sequence with the
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
trait HandleStuff<I> {
    type Output;

    fn handle_stuff(&self, input: I) -> Self::Output;
}

struct MultipleHandlers<T> {
    handlers: T,
}

#[typle(Tuple for 1..=3)]
impl<I, T> HandleStuff<I> for MultipleHandlers<T>
where
    T: Tuple,             // `T` is a tuple with 1 to 3 components.
    T<_>: HandleStuff<I>, // All components implement `HandleStuff`.
    // Conditionally add `Clone` bound to `I`:
    typle!(=> if T::LEN > 1 { I: Clone }): Tuple::Bounds,
{
    // Return a tuple of the output from each handler.
    type Output = (typle! {i in .. => T<{i}>::Output});

    fn handle_stuff(&self, input: I) -> Self::Output {
        (
            typle! {
                i in ..T::LAST =>
                    self.handlers[[i]].handle_stuff(input.clone())
            },
            // Avoid expensive clone for the last handler.
            self.handlers[[T::LAST]].handle_stuff(input),
        )
    }
}
```

This generates the implementations
```rust
impl<I, T0> HandleStuff<I> for MultipleHandlers<(T0,)>
where
    T0: HandleStuff<I>,
{
    type Output = (T0::Output,);
    fn handle_stuff(&self, input: I) -> Self::Output {
        (self.handlers.0.handle_stuff(input),)
    }
}
impl<I, T0, T1> HandleStuff<I> for MultipleHandlers<(T0, T1)>
where
    T0: HandleStuff<I>,
    T1: HandleStuff<I>,
    I: Clone,
{
    type Output = (T0::Output, T1::Output);
    fn handle_stuff(&self, input: I) -> Self::Output {
        (
            self.handlers.0.handle_stuff(input.clone()),
            self.handlers.1.handle_stuff(input),
        )
    }
}
impl<I, T0, T1, T2> HandleStuff<I> for MultipleHandlers<(T0, T1, T2)>
where
    T0: HandleStuff<I>,
    T1: HandleStuff<I>,
    T2: HandleStuff<I>,
    I: Clone,
{
    type Output = (T0::Output, T1::Output, T2::Output);
    fn handle_stuff(&self, input: I) -> Self::Output {
        (
            self.handlers.0.handle_stuff(input.clone()),
            self.handlers.1.handle_stuff(input.clone()),
            self.handlers.2.handle_stuff(input),
        )
    }
}
```
See the [crate documentation](https://docs.rs/typle/) for more examples.
