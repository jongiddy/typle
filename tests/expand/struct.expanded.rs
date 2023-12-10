use typle::typle;
type TupleSequenceOutput1<T0> where T0: Extract = (Option<<T0>::Output>,);
type TupleSequenceOutput2<T0, T1>
where
    T0: Extract,
    T1: Extract,
= (Option<<T0>::Output>, Option<<T1>::Output>);
struct SeqIntoIter1<T0>
where
    T0: Into<ByteStream>,
{
    t: TupleSequenceOutput1<T0>,
}
struct SeqIntoIter2<T0, T1>
where
    T0: Into<ByteStream>,
    T1: Into<ByteStream>,
{
    t: TupleSequenceOutput2<T0, T1>,
}
pub enum State0 {
    #[default]
    Invalid,
}
#[automatically_derived]
impl ::core::default::Default for State0 {
    #[inline]
    fn default() -> State0 {
        Self::Invalid
    }
}
pub enum State1<T0>
where
    T0: std::io::Read,
{
    #[default]
    Invalid,
    S0 { field: (Option<T0>,) },
}
#[automatically_derived]
impl<T0> ::core::default::Default for State1<T0>
where
    T0: std::io::Read,
{
    #[inline]
    fn default() -> State1<T0> {
        Self::Invalid
    }
}
pub enum State2<T0, T1>
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
impl<T0, T1> ::core::default::Default for State2<T0, T1>
where
    T0: std::io::Read,
    T1: std::io::Read,
{
    #[inline]
    fn default() -> State2<T0, T1> {
        Self::Invalid
    }
}
struct AStruct0 {
    t: (),
    state: State0,
}
struct AStruct1<T0> {
    t: (T0,),
    state: State1<T0>,
}
struct AStruct2<T0, T1> {
    t: (T0, T1),
    state: State2<T0, T1>,
}
impl std::io::Read for AStruct0 {
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        {}
    }
}
impl<T0> std::io::Read for AStruct1<T0>
where
    T0: std::io::Read,
{
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        {
            {
                if let State1::S0(output) = self.state.take() {
                    let size = self.t.0.read(buf);
                }
            }
        }
    }
}
impl<T0, T1> std::io::Read for AStruct2<T0, T1>
where
    T0: std::io::Read,
    T1: std::io::Read,
{
    fn read(&mut self, mut buf: &mut [u8]) -> std::io::Result<usize> {
        {
            {
                if let State2::S0(output) = self.state.take() {
                    let size = self.t.0.read(buf);
                }
            }
            {
                if let State2::S1(output) = self.state.take() {
                    let size = self.t.1.read(buf);
                }
            }
        }
    }
}
