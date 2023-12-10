use typle::typle;
use std::ops::Mul;
struct MyStruct<T> {
    pub t: T,
}
impl MyStruct<()> {
    fn new(t: ()) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(&self, multipliers: ()) -> () {
        ()
    }
}
impl<T0> MyStruct<(T0,)> {
    fn new(t: (T0,)) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(&self, multipliers: (M,)) -> (<T0 as Mul<M>>::Output,)
    where
        T0: Mul<M> + Copy,
    {
        (self.t.0 * multipliers.0,)
    }
}
impl<T0, T1> MyStruct<(T0, T1)> {
    fn new(t: (T0, T1)) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(
        &self,
        multipliers: (M, M),
    ) -> (<T0 as Mul<M>>::Output, <T1 as Mul<M>>::Output)
    where
        T0: Mul<M> + Copy,
        T1: Mul<M> + Copy,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1)
    }
}
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
impl<T0, T1, T2, T3> MyStruct<(T0, T1, T2, T3)> {
    fn new(t: (T0, T1, T2, T3)) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(
        &self,
        multipliers: (M, M, M, M),
    ) -> (
        <T0 as Mul<M>>::Output,
        <T1 as Mul<M>>::Output,
        <T2 as Mul<M>>::Output,
        <T3 as Mul<M>>::Output,
    )
    where
        T0: Mul<M> + Copy,
        T1: Mul<M> + Copy,
        T2: Mul<M> + Copy,
        T3: Mul<M> + Copy,
    {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2,
            self.t.3 * multipliers.3,
        )
    }
}
impl<T0, T1, T2, T3, T4> MyStruct<(T0, T1, T2, T3, T4)> {
    fn new(t: (T0, T1, T2, T3, T4)) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(
        &self,
        multipliers: (M, M, M, M, M),
    ) -> (
        <T0 as Mul<M>>::Output,
        <T1 as Mul<M>>::Output,
        <T2 as Mul<M>>::Output,
        <T3 as Mul<M>>::Output,
        <T4 as Mul<M>>::Output,
    )
    where
        T0: Mul<M> + Copy,
        T1: Mul<M> + Copy,
        T2: Mul<M> + Copy,
        T3: Mul<M> + Copy,
        T4: Mul<M> + Copy,
    {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2,
            self.t.3 * multipliers.3,
            self.t.4 * multipliers.4,
        )
    }
}
impl<T0, T1, T2, T3, T4, T5> MyStruct<(T0, T1, T2, T3, T4, T5)> {
    fn new(t: (T0, T1, T2, T3, T4, T5)) -> Self {
        MyStruct { t }
    }
    fn multiply<M>(
        &self,
        multipliers: (M, M, M, M, M, M),
    ) -> (
        <T0 as Mul<M>>::Output,
        <T1 as Mul<M>>::Output,
        <T2 as Mul<M>>::Output,
        <T3 as Mul<M>>::Output,
        <T4 as Mul<M>>::Output,
        <T5 as Mul<M>>::Output,
    )
    where
        T0: Mul<M> + Copy,
        T1: Mul<M> + Copy,
        T2: Mul<M> + Copy,
        T3: Mul<M> + Copy,
        T4: Mul<M> + Copy,
        T5: Mul<M> + Copy,
    {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2,
            self.t.3 * multipliers.3,
            self.t.4 * multipliers.4,
            self.t.5 * multipliers.5,
        )
    }
}
impl<C> MyStruct<(C,)>
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
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C)>
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
        }
        (even, odd)
    }
}
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
impl<C> MyStruct<(C, C, C, C)>
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
            {
                {
                    odd += self.t.3;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C)>
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
            {
                {
                    odd += self.t.3;
                }
            }
            {
                {
                    even += self.t.4;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C)>
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
            {
                {
                    odd += self.t.3;
                }
            }
            {
                {
                    even += self.t.4;
                }
            }
            {
                {
                    odd += self.t.5;
                }
            }
        }
        (even, odd)
    }
}
trait HeadTail {
    type Head;
    type Tail;
    fn head(&self) -> Option<Self::Head>;
    fn tail(&self) -> Self::Tail;
}
impl<T0> HeadTail for (T0,)
where
    T0: Copy,
{
    type Head = T0;
    type Tail = ();
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        ()
    }
}
impl<T0, T1> HeadTail for (T0, T1)
where
    T0: Copy,
    T1: Copy,
{
    type Head = T0;
    type Tail = (T1,);
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.1,)
    }
}
impl<T0, T1, T2> HeadTail for (T0, T1, T2)
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    type Head = T0;
    type Tail = (T1, T2);
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.1, self.2)
    }
}
impl<T0, T1, T2, T3> HeadTail for (T0, T1, T2, T3)
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
{
    type Head = T0;
    type Tail = (T1, T2, T3);
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.1, self.2, self.3)
    }
}
impl<T0, T1, T2, T3, T4> HeadTail for (T0, T1, T2, T3, T4)
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
{
    type Head = T0;
    type Tail = (T1, T2, T3, T4);
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.1, self.2, self.3, self.4)
    }
}
impl<T0, T1, T2, T3, T4, T5> HeadTail for (T0, T1, T2, T3, T4, T5)
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
{
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5);
    fn head(&self) -> Option<Self::Head> {
        Some(self.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.1, self.2, self.3, self.4, self.5)
    }
}
