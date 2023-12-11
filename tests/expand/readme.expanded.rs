use typle::typle;
struct MyStruct<T> {
    t: T,
}
impl From<()> for MyStruct<()> {
    fn from(t: ()) -> Self {
        MyStruct { t }
    }
}
impl<T0> From<(T0,)> for MyStruct<(T0,)> {
    fn from(t: (T0,)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1> From<(T0, T1)> for MyStruct<(T0, T1)> {
    fn from(t: (T0, T1)) -> Self {
        MyStruct { t }
    }
}
impl<T0, T1, T2> From<(T0, T1, T2)> for MyStruct<(T0, T1, T2)> {
    fn from(t: (T0, T1, T2)) -> Self {
        MyStruct { t }
    }
}
use std::ops::{AddAssign, Mul};
impl<T0> MyStruct<(T0,)>
where
    T0: Copy,
{
    fn tail(&self) -> MyStruct<()> {
        ().into()
    }
    fn multiply<M0>(&self, multipliers: (M0,)) -> (<T0 as Mul<M0>>::Output,)
    where
        T0: Mul<M0>,
    {
        (self.t.0 * multipliers.0,)
    }
}
impl<T0, T1> MyStruct<(T0, T1)>
where
    T0: Copy,
    T1: Copy,
{
    fn tail(&self) -> MyStruct<(T1,)> {
        (self.t.1,).into()
    }
    fn multiply<M0, M1>(
        &self,
        multipliers: (M0, M1),
    ) -> (<T0 as Mul<M0>>::Output, <T1 as Mul<M1>>::Output)
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1)
    }
}
impl<T0, T1, T2> MyStruct<(T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    fn tail(&self) -> MyStruct<(T1, T2)> {
        (self.t.1, self.t.2).into()
    }
    fn multiply<M0, M1, M2>(
        &self,
        multipliers: (M0, M1, M2),
    ) -> (<T0 as Mul<M0>>::Output, <T1 as Mul<M1>>::Output, <T2 as Mul<M2>>::Output)
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
        T2: Mul<M2>,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1, self.t.2 * multipliers.2)
    }
}
impl<C> MyStruct<(C,)>
where
    C: AddAssign + Default + Copy,
{
    fn interleave(&self) -> (C, C) {
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
    fn interleave(&self) -> (C, C) {
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
    fn interleave(&self) -> (C, C) {
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
pub trait Extract {
    type State;
    type Output;
}
pub enum TupleSequenceState1<T0>
where
    T0: Extract,
{
    S0((), Option<<T0>::State>),
}
pub enum TupleSequenceState2<T0, T1>
where
    T0: Extract,
    T1: Extract,
{
    S0((), Option<<T0>::State>),
    S1((<T0>::Output,), Option<<T1>::State>),
}
pub enum TupleSequenceState3<T0, T1, T2>
where
    T0: Extract,
    T1: Extract,
    T2: Extract,
{
    S0((), Option<<T0>::State>),
    S1((<T0>::Output,), Option<<T1>::State>),
    S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
}
pub struct TupleSequence<T> {
    tuple: T,
}
impl<T0> Extract for TupleSequence<(T0,)>
where
    T0: Extract,
{
    type State = TupleSequenceState1<T0>;
    type Output = (<T0>::Output,);
}
impl<T0, T1> Extract for TupleSequence<(T0, T1)>
where
    T0: Extract,
    T1: Extract,
{
    type State = TupleSequenceState2<T0, T1>;
    type Output = (<T0>::Output, <T1>::Output);
}
impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
where
    T0: Extract,
    T1: Extract,
    T2: Extract,
{
    type State = TupleSequenceState3<T0, T1, T2>;
    type Output = (<T0>::Output, <T1>::Output, <T2>::Output);
}
