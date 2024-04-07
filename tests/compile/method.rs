use typle::typle;

#[allow(unused)]
pub struct X {
    i: u32,
}

#[typle(Tuple for 0..=3)]
impl X {
    #[allow(unused)]
    pub fn new(i: u32) -> Self {
        Self { i }
    }

    pub fn associated<T: Tuple<u32>>(t: T, i: u32) -> T {
        typle_for!(j in ..T::LEN => t[[j]] + i)
    }

    pub fn inherent1<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        typle_for!(j in ..T::LEN => t[[j]] + self.i)
    }

    pub fn inherent2<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        Self::associated(t, self.i)
    }
}
