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
        let mut even_odd = (C::default(), C::default());
        {
            {
                even_odd.0 += &self.t.0;
            }
            ()
        }
        even_odd
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
        let mut even_odd = (C::default(), C::default());
        {
            {
                even_odd.0 += &self.t.0;
            }
            {
                even_odd.1 += &self.t.1;
            }
            ()
        }
        even_odd
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
        let mut even_odd = (C::default(), C::default());
        {
            {
                even_odd.0 += &self.t.0;
            }
            {
                even_odd.1 += &self.t.1;
            }
            {
                even_odd.0 += &self.t.2;
            }
            ()
        }
        even_odd
    }
}
#[allow(non_camel_case_types)]
trait _typle_fn_zip {
    type Return;
    fn apply(self) -> Self::Return;
}
pub fn zip<A, B>(a: A, b: B) -> <(A, B) as _typle_fn_zip>::Return
where
    (A, B): _typle_fn_zip,
{
    <(A, B) as _typle_fn_zip>::apply((a, b))
}
impl<A0, B0> _typle_fn_zip for ((A0,), (B0,)) {
    type Return = ((A0, B0),);
    fn apply(self) -> Self::Return {
        let (a, b) = self;
        { ((a.0, b.0),) }
    }
}
impl<A0, A1, B0, B1> _typle_fn_zip for ((A0, A1), (B0, B1)) {
    type Return = ((A0, B0), (A1, B1));
    fn apply(self) -> Self::Return {
        let (a, b) = self;
        { ((a.0, b.0), (a.1, b.1)) }
    }
}
impl<A0, A1, A2, B0, B1, B2> _typle_fn_zip for ((A0, A1, A2), (B0, B1, B2)) {
    type Return = ((A0, B0), (A1, B1), (A2, B2));
    fn apply(self) -> Self::Return {
        let (a, b) = self;
        { ((a.0, b.0), (a.1, b.1), (a.2, b.2)) }
    }
}
mod tuple {
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
            #[allow(unused_mut)]
            let mut state = state.unwrap_or(Self::State::S0((), None));
            {
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
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
            #[allow(unused_mut)]
            let mut state = state.unwrap_or(Self::State::S0((), None));
            {
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
                    if let Self::State::S0(output, inner_state) = state {
                        let matched = self.tuple.0.extract(inner_state);
                        let output = ({ matched },);
                        {
                            state = Self::State::S1(output, None);
                        }
                    }
                }
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
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
            #[allow(unused_mut)]
            let mut state = state.unwrap_or(Self::State::S0((), None));
            {
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
                    if let Self::State::S0(output, inner_state) = state {
                        let matched = self.tuple.0.extract(inner_state);
                        let output = ({ matched },);
                        {
                            state = Self::State::S1(output, None);
                        }
                    }
                }
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
                    if let Self::State::S1(output, inner_state) = state {
                        let matched = self.tuple.1.extract(inner_state);
                        let output = ({ output.0 }, { matched });
                        {
                            state = Self::State::S2(output, None);
                        }
                    }
                }
                {
                    #[allow(irrefutable_let_patterns, unused_variables)]
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
}
