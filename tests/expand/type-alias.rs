use typle::typle;

trait Process {
    type State;
    type Output;

    fn process(state: Self::State) -> Result<Self::Output, Error>;
}

#[typle(Tuple for 0..=3)]
type Alias<T>
where
    T: Tuple,
    T<_>: Process,
= typle_for!(i in .. => Option<T<{i}>::Output>);
