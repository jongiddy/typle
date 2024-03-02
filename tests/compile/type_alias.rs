#![allow(type_alias_bounds, unused)]

use typle::typle;

trait Process {
    type State;
    type Output;

    fn process(state: Self::State) -> Result<Self::Output, Box<dyn std::error::Error>>;
}

#[typle(Tuple for 0..=3)]
type Alias<T>
where
    T: Tuple,
    T<_>: Process,
= typle_for!(i in ..T::MAX => Option<T<{i}>::Output>);
