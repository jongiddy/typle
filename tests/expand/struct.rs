use typle::typle;

#[typle(Tuple for 1..=2)]
type TupleSequenceOutput<T>
where
    T: Tuple,
    T<_>: Extract,
= (typle!(i in ..T::MAX => Option<T<{i}>::Output>));

#[typle(Tuple for 1..=2)]
struct SeqIntoIter<T>
where
    T: Tuple,
    T<_>: Into<ByteStream>,
{
    t: TupleSequenceOutput<T<{ ..T::MAX }>>,
}

#[typle(Tuple for 0..=2)]
pub enum State<T>
where
    T: Tuple,
    T<_>: std::io::Read,
{
    Invalid,
    S = typle_variant! {i in ..T::MAX => field: (typle!(j in ..=i => Option<T<{j}>>))},
}
