#![allow(dead_code)]
use typle::typle;

struct MyStruct<T> {
    pub t: T,
}

#[typle(Tuple for 1..=3)]
impl<T: Tuple<u32>> MyStruct<T> {
    fn max(&self) -> Option<u32> {
        #[typle_attr_if(T::LEN == 1, allow(unused_mut))] // For LEN=1 `max` does not get mutated
        let mut max = self.t[[0]];
        for typle_index!(i) in 1..T::LEN {
            if self.t[[i]] > max {
                max = self.t[[i]];
            }
        }
        Some(max)
    }
}

#[typle(Tuple for 1..=4, never=())]
mod tuple {
    pub trait Extract {
        type State;
        type Output;

        fn extract(&self, state: Option<Self::State>) -> Self::Output;
    }

    impl Extract for () {
        type State = ();
        type Output = ();

        fn extract(&self, _state: Option<Self::State>) -> Self::Output {}
    }

    pub enum TupleSequenceState<T>
    where
        T: Tuple,
        T<_>: Extract,
    {
        S = typle_variant!(i in .. =>
            typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
        ),
    }

    pub struct TupleSequence<T> {
        tuple: T,
    }

    impl<T> Extract for TupleSequence<T>
    where
        T: Tuple,
        T<_>: Extract,
    {
        // The state contains the output from all previous components and the state
        // of the current component.
        type State = TupleSequenceState<T<{ .. }>>;
        type Output = typle_for!(i in ..T::LEN => <T<{i}> as Extract>::Output);

        fn extract(&self, state: Option<Self::State>) -> Self::Output {
            #[typle_attr_if(T::LEN == 1, allow(unused_mut))]
            let mut state = state.unwrap_or(Self::State::S::<typle_ident!(0)>((), None));
            for typle_index!(i) in 0..T::LEN {
                // For LEN = 1 there is only one variant (S0) so `let` is irrefutable
                #[typle_attr_if(T::LEN == 1, allow(irrefutable_let_patterns))]
                // For i == 0, the `output` state variable does not get used
                #[typle_attr_if(i == 0, allow(unused_variables))]
                if let Self::State::S::<typle_ident!(i)>(output, inner_state) = state {
                    let matched = self.tuple[[i]].extract(inner_state);
                    let output = typle_for! {j in ..=i =>
                        if j < i { output[[j]] } else { matched }
                    };
                    if typle_const!(i + 1 == T::LEN) {
                        return output;
                    } else {
                        state = Self::State::S::<typle_ident!(i + 1)>(output, None);
                    }
                }
            }
            unreachable!();
        }
    }
}
