use typle::typle;

trait Process {
    type State;
    type Output;

    fn process(state: Self::State) -> Result<Self::Output, Error>;
}

#[typle(Tuple for 0..=3)]
pub enum ProcessState<T>
where
    T: Tuple,
    T<_>: Process<Output = u64>,
{
    // `typle_variant!` creates a variant for each component. The variant will have a number
    // added to the variant name here. `S2(Option<T2::State>, [u64; 2])`
    S = typle_variant!(i in .. => Option<T<{i}>::State>, [u64; i]),
    // U2 {u: [u32; 2]}
    U = typle_variant! {.. => u: [u32; Tuple::LEN]},
    // V2
    V = typle_variant![.. =>],
    Done([u64; Tuple::LEN]),
}

#[typle(Tuple for 0..=3)]
impl<T> Default for ProcessState<T<{ .. }>>
where
    T: Tuple,
    T<_>: Process<Output = u64>,
{
    fn default() -> Self {
        // Const-if allows false branches to contain invalid code. In this case state S0 does not
        // exist for the empty tuple implementation. An alternative to using the `typle_const!` here
        // is to set the typle! macro range to 1..=3 and implement `Default` separately for `()`.
        if typle_const!(T::LEN == 0) {
            Self::Done([])
        } else {
            Self::S0(None, [])
        }
    }
}

#[typle(Tuple for 0..=3)]
impl Process for T
where
    T: Tuple,
    T<_>: Process<Output = u64>,
{
    type State = ProcessState<T<{ .. }>>;
    type Output = [u64; T::LEN];

    fn process(state: Self::State) -> Result<Self::Output, Error> {
        for typle_index!(i) in 0..T::LEN {
            if let Self::State::S::<typle_ident!(i)>(inner_state, output) = state {
                match self.tuple[[i]].process(inner_state) {
                    Err(e) => {
                        return Err(e);
                    }
                    Ok(value) => {
                        // The iteration variable `i` can be used on other const contexts:
                        let mut new_output = <[u64; i + 1]>::default();
                        output
                            .into_iter()
                            .chain(std::iter::once(value))
                            .enumerate()
                            // shadowing of `i` doesn't work correctly so use a different name:
                            .for_each(|(j, bs)| new_output[j] = bs);
                        // Often a standard `if` can be used, but we need a const-if here because
                        // the state S::<typle_ident!(i + 1)> does not exist on the last iteration.
                        // In that case, the second branch is never taken, and will likely get
                        // optimized out, but it still needs to compile.
                        if typle_const!(i + 1 == T::LEN) {
                            state = Self::State::Done(new_output);
                        } else {
                            state = Self::State::S::<typle_ident!(i + 1)>(None, new_output);
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

#[typle(Tuple for 3..=3)]
impl Process for T
where
    T: Tuple,
    T<_>: Process<Output = u64>,
{
    // Test that the number of components determines the suffix
    type State = ProcessState<T<{ 3.. }>>;
    type Output = ProcessState<T<{ 1.. }>>;

    fn process(state: Self::State) -> Result<Self::Output, Error> {
        let x = Self::State::S::<typle_ident!(0)>((), None);
        unreachable!();
    }
}
