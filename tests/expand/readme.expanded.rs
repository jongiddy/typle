use typle::typle;
use std::ops::{AddAssign, Mul};
struct MyStruct<T> {
    pub t: T,
}
impl<C> MyStruct<(C,)>
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
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C)>
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
        }
        (even, odd)
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
impl<C> MyStruct<(C, C, C, C)>
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
impl<C> MyStruct<(C,)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(&self, multipliers: (u32,)) -> (<C as Mul<u32>>::Output,) {
        (self.t.0 * multipliers.0,)
    }
}
impl<C> MyStruct<(C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32),
    ) -> (<C as Mul<u32>>::Output, <C as Mul<u32>>::Output) {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1)
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
impl<C> MyStruct<(C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
    ) {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2,
            self.t.3 * multipliers.3,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
    ) {
        (
            self.t.0 * multipliers.0,
            self.t.1 * multipliers.1,
            self.t.2 * multipliers.2,
            self.t.3 * multipliers.3,
            self.t.4 * multipliers.4,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
    ) {
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
trait HeadTail {
    type Head;
    type Tail;
    fn head(&self) -> Option<Self::Head>;
    fn tail(&self) -> Self::Tail;
}
impl<T0> HeadTail for MyStruct<(T0,)>
where
    T0: Copy,
{
    type Head = T0;
    type Tail = MyStruct<()>;
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        MyStruct { t: () }
    }
}
impl<T0, T1> HeadTail for MyStruct<(T0, T1)>
where
    T0: Copy,
    T1: Copy,
{
    type Head = T0;
    type Tail = MyStruct<(T1,)>;
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        MyStruct { t: (self.t.1,) }
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
        MyStruct {
            t: (self.t.1, self.t.2),
        }
    }
}
impl<T0, T1, T2, T3> HeadTail for MyStruct<(T0, T1, T2, T3)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
{
    type Head = T0;
    type Tail = MyStruct<(T1, T2, T3)>;
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        MyStruct {
            t: (self.t.1, self.t.2, self.t.3),
        }
    }
}
impl<T0, T1, T2, T3, T4> HeadTail for MyStruct<(T0, T1, T2, T3, T4)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
{
    type Head = T0;
    type Tail = MyStruct<(T1, T2, T3, T4)>;
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        MyStruct {
            t: (self.t.1, self.t.2, self.t.3, self.t.4),
        }
    }
}
impl<T0, T1, T2, T3, T4, T5> HeadTail for MyStruct<(T0, T1, T2, T3, T4, T5)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
{
    type Head = T0;
    type Tail = MyStruct<(T1, T2, T3, T4, T5)>;
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        MyStruct {
            t: (self.t.1, self.t.2, self.t.3, self.t.4, self.t.5),
        }
    }
}
