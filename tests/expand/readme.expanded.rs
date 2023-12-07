use typle::typle;
use std::ops::{AddAssign, Mul};
struct MyStruct<T> {
    t: T,
}
impl MyStruct<()> {
    fn new(t: ()) -> Self {
        MyStruct { t }
    }
}
impl<T0> MyStruct<(T0,)> {
    fn new(t: (T0,)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1> MyStruct<(T0, T1)> {
    fn new(t: (T0, T1)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2> MyStruct<(T0, T1, T2)> {
    fn new(t: (T0, T1, T2)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3> MyStruct<(T0, T1, T2, T3)> {
    fn new(t: (T0, T1, T2, T3)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3, T4> MyStruct<(T0, T1, T2, T3, T4)> {
    fn new(t: (T0, T1, T2, T3, T4)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3, T4, T5> MyStruct<(T0, T1, T2, T3, T4, T5)> {
    fn new(t: (T0, T1, T2, T3, T4, T5)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6> MyStruct<(T0, T1, T2, T3, T4, T5, T6)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7> MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6, T7)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8> MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6, T7, T8)) -> Self {
        MyStruct { t }
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
> MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)) -> Self {
        MyStruct { t }
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
> MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)) -> Self {
        MyStruct { t }
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
> MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)> {
    fn new(t: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)) -> Self {
        MyStruct { t }
    }
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
impl<C> MyStruct<(C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
            {
                {
                    odd += self.t.7;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
            {
                {
                    odd += self.t.7;
                }
            }
            {
                {
                    even += self.t.8;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
            {
                {
                    odd += self.t.7;
                }
            }
            {
                {
                    even += self.t.8;
                }
            }
            {
                {
                    odd += self.t.9;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
            {
                {
                    odd += self.t.7;
                }
            }
            {
                {
                    even += self.t.8;
                }
            }
            {
                {
                    odd += self.t.9;
                }
            }
            {
                {
                    even += self.t.10;
                }
            }
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C, C, C)>
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
            {
                {
                    even += self.t.6;
                }
            }
            {
                {
                    odd += self.t.7;
                }
            }
            {
                {
                    even += self.t.8;
                }
            }
            {
                {
                    odd += self.t.9;
                }
            }
            {
                {
                    even += self.t.10;
                }
            }
            {
                {
                    odd += self.t.11;
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
impl<C> MyStruct<(C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
            self.t.7 * multipliers.7,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
            self.t.7 * multipliers.7,
            self.t.8 * multipliers.8,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
            self.t.7 * multipliers.7,
            self.t.8 * multipliers.8,
            self.t.9 * multipliers.9,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
            self.t.7 * multipliers.7,
            self.t.8 * multipliers.8,
            self.t.9 * multipliers.9,
            self.t.10 * multipliers.10,
        )
    }
}
impl<C> MyStruct<(C, C, C, C, C, C, C, C, C, C, C, C)>
where
    C: Mul<u32> + Copy,
{
    fn multiply(
        &self,
        multipliers: (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32),
    ) -> (
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
        <C as Mul<u32>>::Output,
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
            self.t.6 * multipliers.6,
            self.t.7 * multipliers.7,
            self.t.8 * multipliers.8,
            self.t.9 * multipliers.9,
            self.t.10 * multipliers.10,
            self.t.11 * multipliers.11,
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
    type Tail = ();
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        ()
    }
}
impl<T0, T1> HeadTail for MyStruct<(T0, T1)>
where
    T0: Copy,
    T1: Copy,
{
    type Head = T0;
    type Tail = (T1,);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1,)
    }
}
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
impl<T0, T1, T2, T3> HeadTail for MyStruct<(T0, T1, T2, T3)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
{
    type Head = T0;
    type Tail = (T1, T2, T3);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3)
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
    type Tail = (T1, T2, T3, T4);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3, self.t.4)
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
    type Tail = (T1, T2, T3, T4, T5);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3, self.t.4, self.t.5)
    }
}
impl<T0, T1, T2, T3, T4, T5, T6> HeadTail for MyStruct<(T0, T1, T2, T3, T4, T5, T6)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
    T3: Copy,
    T4: Copy,
    T5: Copy,
    T6: Copy,
{
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3, self.t.4, self.t.5, self.t.6)
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7> HeadTail
for MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7)>
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
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6, T7);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3, self.t.4, self.t.5, self.t.6, self.t.7)
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8> HeadTail
for MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8)>
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
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6, T7, T8);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (self.t.1, self.t.2, self.t.3, self.t.4, self.t.5, self.t.6, self.t.7, self.t.8)
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> HeadTail
for MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)>
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
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6, T7, T8, T9);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (
            self.t.1,
            self.t.2,
            self.t.3,
            self.t.4,
            self.t.5,
            self.t.6,
            self.t.7,
            self.t.8,
            self.t.9,
        )
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> HeadTail
for MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)>
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
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (
            self.t.1,
            self.t.2,
            self.t.3,
            self.t.4,
            self.t.5,
            self.t.6,
            self.t.7,
            self.t.8,
            self.t.9,
            self.t.10,
        )
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> HeadTail
for MyStruct<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)>
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
    type Head = T0;
    type Tail = (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
    fn head(&self) -> Option<Self::Head> {
        Some(self.t.0)
    }
    fn tail(&self) -> Self::Tail {
        (
            self.t.1,
            self.t.2,
            self.t.3,
            self.t.4,
            self.t.5,
            self.t.6,
            self.t.7,
            self.t.8,
            self.t.9,
            self.t.10,
            self.t.11,
        )
    }
}
