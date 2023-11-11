# typle

A Rust macro to create items for different sized tuples.

The code on the left generates implementations for tuples up to 12 elements. The generated implementations for 3-tuples appear on the right.

<table>
<tr>
<th>Source code</th>
<th>Generated code</th>
</tr>
<tr>
<td>

```rust
struct MyStruct<S, T> {
    s: S,
    t: Option<T>,
}

#[typle(Tuple for 0..=12)]
impl<S, T> MyStruct<S, T>
where
    S: Tuple<u32>,
    T: Tuple<impl Sized>,
{
    fn new(s: S, t: Option<T>) -> MyStruct<S, T> {
        MyStruct { s, t }
    }

    fn sum(&self) -> u32 {
        let mut sum = 0;
        for i in 0..S::LEN {
            sum += self.s[[i]];
        }
        sum
    }
}

trait FirstLast {
    type F;
    type L;
    fn first(&self) -> Option<Self::F>;
    fn last(&self) -> Option<Self::L>;
}

#[typle(Tuple for 1..=12)]
impl<S, T> FirstLast for MyStruct<S, T>
where
    T: Tuple<impl Copy>,
{
    type F = T<0>;
    type L = T<{ T::LEN - 1 }>;

    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup[[0]])
    }

    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup[[T::LEN - 1]])
    }
}
```

</td>
<td>

```rust
impl<T0, T1, T2> MyStruct<(u32, u32, u32), (T0, T1, T2)>
where
    T0: Sized,
    T1: Sized,
    T2: Sized,
{
    fn new(
        s: (u32, u32, u32),
        t: Option<(T0, T1, T2)>,
    ) -> MyStruct<(u32, u32, u32), (T0, T1, T2)> {
        MyStruct { s, t }
    }

    fn sum(&self) -> u32 {
        let mut sum = 0;
        {
            {
                sum += self.s.0;
            }
            {
                sum += self.s.1;
            }
            {
                sum += self.s.2;
            }
        }
        sum
    }
}

impl<S, T0, T1, T2> FirstLast for MyStruct<S, (T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    type F = T0;
    type L = T2;

    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }

    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.2)
    }
}
```

</td>
</tr>
</table>
