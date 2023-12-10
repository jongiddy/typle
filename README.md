# typle

A Rust macro to create items for different sized tuples.

Use the `typle` macro to generate items for multiple arities.

```rust
use typle::typle;

use std::ops::Mul;
struct MyStruct<T> {
    pub t: T,
}

#[typle(Tuple for 0..=6)]
impl<T> MyStruct<T>
where
    T: Tuple
{
    fn new(t: T) -> Self {
        MyStruct { t }
    }

    fn multiply<M>(
        &self,
        multipliers: typle_for!(.. => M)
    ) -> typle_for!(i in .. => <T<{i}> as Mul<M>>::Output)
    where
        T::Types: Mul<M> + Copy,
    {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]])
    }
}
```

This code generates implementations for tuples up to 6 components, including the following for 3-tuples:
```rust
impl<T0, T1, T2> MyStruct<(T0, T1, T2)> {
    fn new(t: (T0, T1, T2)) -> Self {
        MyStruct { t }
    }

    fn multiply<M>(
        &self,
        multipliers: (M, M, M),
    ) -> (<T0 as Mul<M>>::Output, <T1 as Mul<M>>::Output, <T2 as Mul<M>>::Output)
    where
        T0: Mul<M> + Copy,
        T1: Mul<M> + Copy,
        T2: Mul<M> + Copy,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1, self.t.2 * multipliers.2)
    }
}
```

Each component of the tuple can be a different type, as long as they meet the constraints in the
`where` clause. To constrain the tuple components to a single type, add another type parameter:

```rust
#[typle(Tuple for 1..=6)]
impl<T, C> MyStruct<T>
where
    T: Tuple<Types=C>,
    C: std::ops::AddAssign + Default + Copy,
{
    fn even_odd(&self) -> (C, C) {
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
```
creates implementations including:
```rust
impl<C> MyStruct<(C, C, C)>
where
    C: std::ops::AddAssign + Default + Copy,
{
    fn even_odd(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        {
            {
                {
                    even += self.t.0;
                }
            }
            {
                {
                    odd += self.t.1;
                }
            }
            {
                {
                    even += self.t.2;
                }
            }
        }
        (even, odd)
    }
}
```

Use `typle` to implement traits for tuples:

```rust
trait HeadTail {
    type Head;
    type Tail;
    fn head(&self) -> Option<Self::Head>;
    fn tail(&self) -> Self::Tail;
}

#[typle(Tuple for 1..=6)]
impl<T> HeadTail for MyStruct<T>
where
    T: Tuple,
    T::Types: Copy,
{
    type Head = T<0>;
    type Tail = typle_for!(i in 1.. => T<{i}>);

    fn head(&self) -> Option<Self::Head> {
        Some(self.t[[0]])
    }

    fn tail(&self) -> Self::Tail {
        typle_for!(i in 1.. => self.t[[i]])
    }
}
```
creates implementations including:
```rust
impl<T0, T1, T2> HeadTail for MyStruct<(T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    type Head = T0;
    type Tail = (T1, T2);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2)
    }
}
```
