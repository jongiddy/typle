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
use std::ops::Mul;
impl<T0> MyStruct<(T0,)>
where
    T0: Copy,
{
    fn tail(&self) -> () {
        ()
    }
    fn multiply<M0>(&self, multipliers: (M0,)) -> MyStruct<(<T0 as Mul<M0>>::Output,)>
    where
        T0: Mul<M0>,
    {
        (self.t.0 * multipliers.0,).into()
    }
}
impl<T0, T1> MyStruct<(T0, T1)>
where
    T0: Copy,
    T1: Copy,
{
    fn tail(&self) -> (T1,) {
        (self.t.1,)
    }
    fn multiply<M0, M1>(
        &self,
        multipliers: (M0, M1),
    ) -> MyStruct<(<T0 as Mul<M0>>::Output, <T1 as Mul<M1>>::Output)>
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1).into()
    }
}
impl<T0, T1, T2> MyStruct<(T0, T1, T2)>
where
    T0: Copy,
    T1: Copy,
    T2: Copy,
{
    fn tail(&self) -> (T1, T2) {
        (self.t.1, self.t.2)
    }
    fn multiply<M0, M1, M2>(
        &self,
        multipliers: (M0, M1, M2),
    ) -> MyStruct<
        (<T0 as Mul<M0>>::Output, <T1 as Mul<M1>>::Output, <T2 as Mul<M2>>::Output),
    >
    where
        T0: Mul<M0>,
        T1: Mul<M1>,
        T2: Mul<M2>,
    {
        (self.t.0 * multipliers.0, self.t.1 * multipliers.1, self.t.2 * multipliers.2)
            .into()
    }
}
impl<C> MyStruct<(C,)>
where
    C: for<'a> std::ops::AddAssign<&'a C> + Default,
{
    fn last(&self) -> &C {
        &self.t.0
    }
    fn interleave(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        {
            {
                {
                    even += &self.t.0;
                }
            }
            ()
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C)>
where
    C: for<'a> std::ops::AddAssign<&'a C> + Default,
{
    fn last(&self) -> &C {
        &self.t.1
    }
    fn interleave(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        {
            {
                {
                    even += &self.t.0;
                }
            }
            {
                {
                    odd += &self.t.1;
                }
            }
            ()
        }
        (even, odd)
    }
}
impl<C> MyStruct<(C, C, C)>
where
    C: for<'a> std::ops::AddAssign<&'a C> + Default,
{
    fn last(&self) -> &C {
        &self.t.2
    }
    fn interleave(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        {
            {
                {
                    even += &self.t.0;
                }
            }
            {
                {
                    odd += &self.t.1;
                }
            }
            {
                {
                    even += &self.t.2;
                }
            }
            ()
        }
        (even, odd)
    }
}
pub trait Extract {
    type State;
    type Output;
    fn extract(&self, state: Option<Self::State>) -> Self::Output;
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
    fn extract(&self, state: Option<Self::State>) -> Self::Output {
        let mut state = state.unwrap_or(Self::State::S0((), None));
        {
            {
                if let Self::State::S0(output, inner_state) = state {
                    let matched = self.tuple.0.extract(inner_state);
                    let output = ({ matched },);
                    {
                        return output;
                    }
                }
            }
            ()
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl<T0, T1> Extract for TupleSequence<(T0, T1)>
where
    T0: Extract,
    T1: Extract,
{
    type State = TupleSequenceState2<T0, T1>;
    type Output = (<T0>::Output, <T1>::Output);
    fn extract(&self, state: Option<Self::State>) -> Self::Output {
        let mut state = state.unwrap_or(Self::State::S0((), None));
        {
            {
                if let Self::State::S0(output, inner_state) = state {
                    let matched = self.tuple.0.extract(inner_state);
                    let output = ({ matched },);
                    {
                        state = Self::State::S1(output, None);
                    }
                }
            }
            {
                if let Self::State::S1(output, inner_state) = state {
                    let matched = self.tuple.1.extract(inner_state);
                    let output = ({ output.0 }, { matched });
                    {
                        return output;
                    }
                }
            }
            ()
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
where
    T0: Extract,
    T1: Extract,
    T2: Extract,
{
    type State = TupleSequenceState3<T0, T1, T2>;
    type Output = (<T0>::Output, <T1>::Output, <T2>::Output);
    fn extract(&self, state: Option<Self::State>) -> Self::Output {
        let mut state = state.unwrap_or(Self::State::S0((), None));
        {
            {
                if let Self::State::S0(output, inner_state) = state {
                    let matched = self.tuple.0.extract(inner_state);
                    let output = ({ matched },);
                    {
                        state = Self::State::S1(output, None);
                    }
                }
            }
            {
                if let Self::State::S1(output, inner_state) = state {
                    let matched = self.tuple.1.extract(inner_state);
                    let output = ({ output.0 }, { matched });
                    {
                        state = Self::State::S2(output, None);
                    }
                }
            }
            {
                if let Self::State::S2(output, inner_state) = state {
                    let matched = self.tuple.2.extract(inner_state);
                    let output = ({ output.0 }, { output.1 }, { matched });
                    {
                        return output;
                    }
                }
            }
            ()
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
