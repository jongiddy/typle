use typle::typle;

trait Process {
    type State;
    type Output;

    fn process(state: Self::State) -> Result<Self::Output, Error>;
}

#[typle(Tuple for 2..=3)]
pub enum ProcessState<T>
where
    T: Tuple<impl Process<Output = u64>>,
{
    S(Option<T::State>, [u64; T::INDEX]) = typle_variants!(),
    Done([u64; T::LEN])
}

#[typle(Tuple for 2..=3)]
impl Process for T
where
    T: Tuple<impl Process<Output = u64>>,
{
    type State = ProcessState<(T)>;
    type Output = [u64; T::LEN];

    fn process(state: Self::State) -> Result<Self::Output, Error> {
        for typle_const!(i) in 0..T::LEN {
            if let Self::State::S::<typle_index!(i)>(inner_state, output) = state {
                match self.tuple[[i]].process(inner_state) {
                    Err(e) => {
                        return Err(e);
                    }
                    Ok(value) => {
                        let mut new_output = <[u64; i + 1]>::default();
                        output
                            .into_iter()
                            .chain(std::iter::once(value))
                            .enumerate()
                            .for_each(|(j, bs)| new_output[j] = bs);
                        if typle_const!(i + 1 == T::LEN) {
                            state = Self::State::Done(new_output);
                        } else {
                            state = Self::State::S::<typle_index!(i + 1)>(new_output);
                        }
                    }
                }
            }
        }
        if let Self::State::Done(output) = state {
            return output;
        }
        unreachable!();
    }
}
