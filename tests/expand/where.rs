use typle::typle;

#[typle(Tuple for 0..=2)]
impl<T> TupleA<T>
where
    T: Tuple(u32),
{}

#[typle(Tuple for 0..=2)]
impl<T> TraitB for TupleB<T>
where
    T: Tuple,
    T::Types: Extract,
    T::Types::Output: AsRef<str>,
{}
