use typle::typle;
trait Process {
    type State;
    type Output;
    fn process(state: Self::State) -> Result<Self::Output, Error>;
}
type Alias0 = ();
type Alias1<T0> where T0: Process = (Option<T0::Output>,);
type Alias2<T0, T1>
where
    T0: Process,
    T1: Process,
= (Option<T0::Output>, Option<T1::Output>);
type Alias3<T0, T1, T2>
where
    T0: Process,
    T1: Process,
    T2: Process,
= (Option<T0::Output>, Option<T1::Output>, Option<T2::Output>);
