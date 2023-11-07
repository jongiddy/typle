# typle

A Rust macro to create items for different sized tuples.

```rust
struct MyStruct<T, U> {
    t: T,
    u: Option<U>,
}

#[typle(Tuple for ..=2)]
impl<T, U> MyStruct<T, U>
where
    T: Tuple<impl Sized>,
    U: Tuple<u32>,
{
    fn new(t: T, u: Option<U>) -> MyStruct<T, U> {
        MyStruct { t, u }
    }
}

trait FirstLast {
    type F;
    type L;
    fn first(&self) -> Option<Self::F>;
    fn last(&self) -> Option<Self::L>;
}

#[typle(Tuple for 1..=2)]
impl<T, U> FirstLast for MyStruct<T, U>
where
    T: Tuple<impl Copy>,
{
    type F = T<0>;
    type L = T<{T::LEN - 1}>;

    fn first(&self) -> Option<Self::F> {
        Some(self.t[[0]])
    }

    fn last(&self) -> Option<Self::L> {
        Some(self.t[[T::LEN - 1]])
    }
}
```

expands the implementations to:

```rust
impl MyStruct<(), ()> {
    fn new(t: (), u: Option<()>) -> MyStruct<(), ()> {
        MyStruct { t, u }
    }
}
impl<T0> MyStruct<(T0,), (u32,)>
where
    T0: Sized,
{
    fn new(t: (T0,), u: Option<(u32,)>) -> MyStruct<(T0,), (u32,)> {
        MyStruct { t, u }
    }
}
impl<T0, T1> MyStruct<(T0, T1), (u32, u32)>
where
    T0: Sized,
    T1: Sized,
{
    fn new(t: (T0, T1), u: Option<(u32, u32)>) -> MyStruct<(T0, T1), (u32, u32)> {
        MyStruct { t, u }
    }
}
impl<T0, U> FirstLast for MyStruct<(T0,), U>
where
    T0: Copy,
{
    type F = T0;
    type L = T0;
    fn first(&self) -> Option<Self::F> {
        Some(self.t.0)
    }
    fn last(&self) -> Option<Self::L> {
        Some(self.t.0)
    }
}
impl<T0, T1, U> FirstLast for MyStruct<(T0, T1), U>
where
    T0: Copy,
    T1: Copy,
{
    type F = T0;
    type L = T1;
    fn first(&self) -> Option<Self::F> {
        Some(self.t.0)
    }
    fn last(&self) -> Option<Self::L> {
        Some(self.t.1)
    }
}
```
