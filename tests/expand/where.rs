use typle::typle;

#[typle(Tuple for 0..=2)]
impl<T> TupleA<T>
where
    T: Tuple<Types=u32>,
{}

#[typle(Tuple for 0..=2)]
impl<T> TraitB for TupleB<T>
where
    T: Tuple,
    T::Types: Extract,
    T::Types::Output: AsRef<str>,
{}

#[typle(Tuple for 1..=2)]
impl<S, T> Extract for TupleC<T>
where
    S: Tuple,
    S::Types: Extract,
    T: Tuple,
    T::Types: Extract<Output = Option<S>>,
{}
