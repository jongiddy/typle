use typle::typle;

trait Process {
    type State;
    type Output;

    fn process(state: Self::State) -> Result<Self::Output, Error>;
}

#[typle(Tuple for 0..=3)]
type Alias<T> where T: Tuple<impl Process> = typle_expand!(Option<T<INDEX>::Output>);
