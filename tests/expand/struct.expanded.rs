use typle::typle;
type TupleSequenceOutput<T0, T1>
where
    T0: Extract,
    T1: Extract,
= (Option<<T0>::Output>, Option<<T1>::Output>);
struct SeqIntoIter<T0, T1>
where
    T0: Into<ByteStream>,
    T1: Into<ByteStream>,
{
    t: TupleSequenceOutput<T0, T1>,
}
pub enum State<T0, T1>
where
    T0: std::io::Read,
    T1: std::io::Read,
{
    #[default]
    Invalid,
    S0 { field: (Option<T0>,) },
    S1 { field: (Option<T0>, Option<T1>) },
}
#[automatically_derived]
impl<T0, T1> ::core::default::Default for State<T0, T1>
where
    T0: std::io::Read,
    T1: std::io::Read,
{
    #[inline]
    fn default() -> State<T0, T1> {
        Self::Invalid
    }
}
