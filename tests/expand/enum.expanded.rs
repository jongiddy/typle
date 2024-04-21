use typle::typle;
trait Process {
    type State;
    type Output;
    fn process(state: Self::State) -> Result<Self::Output, Error>;
}
pub enum ProcessState<T0, T1>
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
{
    S0(Option<<T0>::State>, [u64; 0]),
    S1(Option<<T1>::State>, [u64; 1]),
    U0 { u: [u32; 0] },
    U1 { u: [u32; 1] },
    V0,
    V1,
    Done([u64; 2]),
}
impl Default for ProcessState<!, !, !> {
    fn default() -> Self {
        { Self::Done([0; 3]) }
    }
}
impl<T0> Default for ProcessState<T0, !, !>
where
    T0: Process<Output = u64>,
{
    fn default() -> Self {
        { Self::S0(None, []) }
    }
}
impl<T0, T1> Default for ProcessState<T0, T1, !>
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
{
    fn default() -> Self {
        { Self::S0(None, []) }
    }
}
impl<T0, T1, T2> Default for ProcessState<T0, T1, T2>
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
    T2: Process<Output = u64>,
{
    fn default() -> Self {
        { Self::S0(None, []) }
    }
}
impl Process for () {
    type State = ProcessState<!, !, !>;
    type Output = [u64; 0];
    fn process(state: Self::State) -> Result<Self::Output, Error> {
        if let Self::State::Done(output) = state {
            return output;
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl Process for (T0,)
where
    T0: Process<Output = u64>,
{
    type State = ProcessState<T0, !, !>;
    type Output = [u64; 1];
    fn process(state: Self::State) -> Result<Self::Output, Error> {
        loop {
            {
                if let Self::State::S0(inner_state, output) = state {
                    match self.tuple.0.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 0 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::Done(new_output);
                            }
                        }
                    }
                }
            }
            break;
        }
        if let Self::State::Done(output) = state {
            return output;
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl Process for (T0, T1)
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
{
    type State = ProcessState<T0, T1, !>;
    type Output = [u64; 2];
    fn process(state: Self::State) -> Result<Self::Output, Error> {
        loop {
            {
                if let Self::State::S0(inner_state, output) = state {
                    match self.tuple.0.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 0 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::S1(None, new_output);
                            }
                        }
                    }
                }
            }
            {
                if let Self::State::S1(inner_state, output) = state {
                    match self.tuple.1.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 1 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::Done(new_output);
                            }
                        }
                    }
                }
            }
            break;
        }
        if let Self::State::Done(output) = state {
            return output;
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl Process for (T0, T1, T2)
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
    T2: Process<Output = u64>,
{
    type State = ProcessState<T0, T1, T2>;
    type Output = [u64; 3];
    fn process(state: Self::State) -> Result<Self::Output, Error> {
        loop {
            {
                if let Self::State::S0(inner_state, output) = state {
                    match self.tuple.0.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 0 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::S1(None, new_output);
                            }
                        }
                    }
                }
            }
            {
                if let Self::State::S1(inner_state, output) = state {
                    match self.tuple.1.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 1 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::S2(None, new_output);
                            }
                        }
                    }
                }
            }
            {
                if let Self::State::S2(inner_state, output) = state {
                    match self.tuple.2.process(inner_state) {
                        Err(e) => {
                            return Err(e);
                        }
                        Ok(value) => {
                            let mut new_output = <[u64; 2 + 1]>::default();
                            output
                                .into_iter()
                                .chain(std::iter::once(value))
                                .enumerate()
                                .for_each(|(j, bs)| new_output[j] = bs);
                            {
                                state = Self::State::Done(new_output);
                            }
                        }
                    }
                }
            }
            break;
        }
        if let Self::State::Done(output) = state {
            return output;
        }
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
impl Process for (T0, T1, T2)
where
    T0: Process<Output = u64>,
    T1: Process<Output = u64>,
    T2: Process<Output = u64>,
{
    type State = ProcessState;
    type Output = ProcessState<T1, T2>;
    fn process(state: Self::State) -> Result<Self::Output, Error> {
        let x = Self::State::S0((), None);
        ::core::panicking::panic("internal error: entered unreachable code");
    }
}
