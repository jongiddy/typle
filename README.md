# typle

A Rust macro to create items for different sized tuples.

The code below generates implementations for tuples up to 6 components.

```rust
use typle::typle;

use std::ops::{AddAssign, Mul};

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
}

#[typle(Tuple for 1..=6)]
impl<T, C> MyStruct<T>
where
    T: Tuple(C),
    C: AddAssign + Default + Copy,
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

#[typle(Tuple for 1..=6)]
impl<T, C> MyStruct<T>
where
    T: Tuple(C),
    C: Mul<u32> + Copy,
{
    fn multiply(&self, multipliers: typle_for!(.. => u32)) -> typle_for!(.. => <C as Mul<u32>>::Output) {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]])
    }
}

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
    type Tail = MyStruct<typle_for!(i in 1.. => T<{i}>)>;

    fn head(&self) -> Option<Self::Head> {
        Some(self.t[[0]])
    }

    fn tail(&self) -> Self::Tail {
        MyStruct::<typle_for!(i in 1.. => T<{i}>)>::new(typle_for!(i in 1.. => self.t[[i]]))
    }
}
```

The generated implementations for 3-tuples are:

```rust
impl<T0, T1, T2> MyStruct<(T0, T1, T2)> {
    fn new(t: (T0, T1, T2)) -> Self {
        MyStruct { t }
    }
}

impl<C> MyStruct<(C, C, C)>
where
    C: AddAssign + Default + Copy,
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

impl<C> MyStruct<(C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32),
    ) -> (<C as Mul<u32>>::Output, <C as Mul<u32>>::Output, <C as Mul<u32>>::Output) {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1, self.t.2 * multipliers.2)
    }
}

impl<T0, T1, T2> HeadTail for MyStruct<(T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    type Head = T0;
    type Tail = MyStruct<(T1, T2)>;

    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }

    fn tail(&self) -> Self::Tail {
        MyStruct::<(T1, T2)>::new((self.t.1, self.t.2))
    }
}
```
