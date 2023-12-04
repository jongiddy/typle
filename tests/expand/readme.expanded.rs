use typle::typle;
struct MyStruct<S, T> {
    s: S,
    t: Option<T>,
}
impl MyStruct<(), ()> {
    fn new(s: (), t: Option<()>) -> MyStruct<(), ()> {
        MyStruct { s, t }
    }
    fn sum(&self) -> u32 {
        let mut sum = 0;
        {}
        sum
    }
    fn multiply(&self, multipliers: ()) -> () {
        ()
    }
}
impl<T0> MyStruct<(u32,), (T0,)> {
    fn new(s: (u32,), t: Option<(T0,)>) -> MyStruct<(u32,), (T0,)> {
        MyStruct { s, t }
    }
    fn sum(&self) -> u32 {
        let mut sum = 0;
        {
            {
                sum += self.s.0;
            }
        }
        sum
    }
    fn multiply(&self, multipliers: (u32,)) -> (u64,) {
        (self.s.0 as u64 * multipliers.0 as u64,)
    }
}
impl<T0, T1> MyStruct<(u32, u32), (T0, T1)> {
    fn new(s: (u32, u32), t: Option<(T0, T1)>) -> MyStruct<(u32, u32), (T0, T1)> {
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
        }
        sum
    }
    fn multiply(&self, multipliers: (u32, u32)) -> (u64, u64) {
        (self.s.0 as u64 * multipliers.0 as u64, self.s.1 as u64 * multipliers.1 as u64)
    }
}
impl<T0, T1, T2> MyStruct<(u32, u32, u32), (T0, T1, T2)> {
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
    fn multiply(&self, multipliers: (u32, u32, u32)) -> (u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
        )
    }
}
impl<T0, T1, T2, T3> MyStruct<(u32, u32, u32, u32), (T0, T1, T2, T3)> {
    fn new(
        s: (u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3)>,
    ) -> MyStruct<(u32, u32, u32, u32), (T0, T1, T2, T3)> {
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
            {
                sum += self.s.3;
            }
        }
        sum
    }
    fn multiply(&self, multipliers: (u32, u32, u32, u32)) -> (u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
        )
    }
}
impl<T0, T1, T2, T3, T4> MyStruct<(u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4)> {
    fn new(
        s: (u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4)>,
    ) -> MyStruct<(u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4)> {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
> MyStruct<(u32, u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4, T5)> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5)>,
    ) -> MyStruct<(u32, u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4, T5)> {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
> MyStruct<(u32, u32, u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4, T5, T6)> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6)>,
    ) -> MyStruct<(u32, u32, u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4, T5, T6)> {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
> MyStruct<(u32, u32, u32, u32, u32, u32, u32, u32), (T0, T1, T2, T3, T4, T5, T6, T7)> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6, T7)>,
    ) -> MyStruct<
        (u32, u32, u32, u32, u32, u32, u32, u32),
        (T0, T1, T2, T3, T4, T5, T6, T7),
    > {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
            {
                sum += self.s.7;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
            self.s.7 as u64 * multipliers.7 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
> MyStruct<
    (u32, u32, u32, u32, u32, u32, u32, u32, u32),
    (T0, T1, T2, T3, T4, T5, T6, T7, T8),
> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6, T7, T8)>,
    ) -> MyStruct<
        (u32, u32, u32, u32, u32, u32, u32, u32, u32),
        (T0, T1, T2, T3, T4, T5, T6, T7, T8),
    > {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
            {
                sum += self.s.7;
            }
            {
                sum += self.s.8;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
            self.s.7 as u64 * multipliers.7 as u64,
            self.s.8 as u64 * multipliers.8 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
> MyStruct<
    (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9),
> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)>,
    ) -> MyStruct<
        (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9),
    > {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
            {
                sum += self.s.7;
            }
            {
                sum += self.s.8;
            }
            {
                sum += self.s.9;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
            self.s.7 as u64 * multipliers.7 as u64,
            self.s.8 as u64 * multipliers.8 as u64,
            self.s.9 as u64 * multipliers.9 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
> MyStruct<
    (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10),
> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)>,
    ) -> MyStruct<
        (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10),
    > {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
            {
                sum += self.s.7;
            }
            {
                sum += self.s.8;
            }
            {
                sum += self.s.9;
            }
            {
                sum += self.s.10;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
            self.s.7 as u64 * multipliers.7 as u64,
            self.s.8 as u64 * multipliers.8 as u64,
            self.s.9 as u64 * multipliers.9 as u64,
            self.s.10 as u64 * multipliers.10 as u64,
        )
    }
}
impl<
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    T8,
    T9,
    T10,
    T11,
> MyStruct<
    (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11),
> {
    fn new(
        s: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        t: Option<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)>,
    ) -> MyStruct<
        (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11),
    > {
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
            {
                sum += self.s.3;
            }
            {
                sum += self.s.4;
            }
            {
                sum += self.s.5;
            }
            {
                sum += self.s.6;
            }
            {
                sum += self.s.7;
            }
            {
                sum += self.s.8;
            }
            {
                sum += self.s.9;
            }
            {
                sum += self.s.10;
            }
            {
                sum += self.s.11;
            }
        }
        sum
    }
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64, u64) {
        (
            self.s.0 as u64 * multipliers.0 as u64,
            self.s.1 as u64 * multipliers.1 as u64,
            self.s.2 as u64 * multipliers.2 as u64,
            self.s.3 as u64 * multipliers.3 as u64,
            self.s.4 as u64 * multipliers.4 as u64,
            self.s.5 as u64 * multipliers.5 as u64,
            self.s.6 as u64 * multipliers.6 as u64,
            self.s.7 as u64 * multipliers.7 as u64,
            self.s.8 as u64 * multipliers.8 as u64,
            self.s.9 as u64 * multipliers.9 as u64,
            self.s.10 as u64 * multipliers.10 as u64,
            self.s.11 as u64 * multipliers.11 as u64,
        )
    }
}
trait FirstLast {
    type F;
    type L;
    fn first(&self) -> Option<Self::F>;
    fn last(&self) -> Option<Self::L>;
}
impl<S, T0> FirstLast for MyStruct<S, (T0,)>
where
    T0: Copy,
{
    type F = T0;
    type L = T0;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.0)
    }
}
impl<S, T0, T1> FirstLast for MyStruct<S, (T0, T1)>
where
    T0: Copy,
    T1: Copy,
{
    type F = T0;
    type L = T1;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.1)
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
impl<S, T0, T1, T2, T3> FirstLast for MyStruct<S, (T0, T1, T2, T3)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
{
    type F = T0;
    type L = T3;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.3)
    }
}
impl<S, T0, T1, T2, T3, T4> FirstLast for MyStruct<S, (T0, T1, T2, T3, T4)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
{
    type F = T0;
    type L = T4;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.4)
    }
}
impl<S, T0, T1, T2, T3, T4, T5> FirstLast for MyStruct<S, (T0, T1, T2, T3, T4, T5)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
{
    type F = T0;
    type L = T5;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.5)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
{
    type F = T0;
    type L = T6;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.6)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6, T7> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6, T7)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
    T7: Copy,
{
    type F = T0;
    type L = T7;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.7)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6, T7, T8> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6, T7, T8)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
    T7: Copy,
    T8: Copy,
{
    type F = T0;
    type L = T8;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.8)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
    T7: Copy,
    T8: Copy,
    T9: Copy,
{
    type F = T0;
    type L = T9;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.9)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
    T7: Copy,
    T8: Copy,
    T9: Copy,
    T10: Copy,
{
    type F = T0;
    type L = T10;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.10)
    }
}
impl<S, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> FirstLast
for MyStruct<S, (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
    T7: Copy,
    T8: Copy,
    T9: Copy,
    T10: Copy,
    T11: Copy,
{
    type F = T0;
    type L = T11;
    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup.0)
    }
    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup.11)
    }
}
