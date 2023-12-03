use typle::typle;

#[typle(Tuple for 1..=2)]
type TupleSequenceOutput<T>
where
    T: Tuple<impl Extract>,
= typle_for!(i in .. => Option<T<{i}>::Output>);

#[typle(Tuple for 1..=2)]
struct SeqIntoIter<T>
where
    T: Tuple<impl Into<ByteStream>>,
{
    t: TupleSequenceOutput<(T)>,
}

#[typle(Tuple for 0..=2)]
#[derive(Default)]
pub enum State<T>
where
    T: Tuple<impl std::io::Read>,
{
    #[default]
    Invalid,
    S = typle_variant!{i in .. => field: typle_for!(j in ..=i => Option<T<{j}>>)},
}

#[typle(Tuple for 0..=2)]
struct AStruct<T> {
    t: T,
    state: State<(T)>,
}

#[typle(Tuple for 0..=2)]
impl<T> std::io::Read for AStruct<(T)>
where
    T: Tuple<impl std::io::Read>,
{
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        for typle_const!(i) in 0..T::LEN {
            if let State::<typle_index!(T::LEN)>::S::<typle_index!(i)>(output) = self.state.take() {
                let size = self.t[[i]].read(buf);
            }
        }
    }
}
