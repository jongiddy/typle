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
        typle_for!(j in .. => t[[j]] + i)
    }

    pub fn inherent1<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        typle_for!(j in .. => t[[j]] + self.i)
    }

    pub fn inherent2<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        Self::associated(t, self.i)
    }

    #[allow(clippy::assign_op_pattern)]
    pub fn inherent3<'a, T: Tuple<u32>>(&'a mut self, t: T) -> T {
        self.i += 1;
        self.i = self.i + 1;
        t
    }

    pub fn inherent4<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        let _v: Vec<Self> = Vec::<Self>::new();
        t
    }

    pub fn inherent5<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        #[typle_attr_if(T::LEN == 0, allow(unused_variables))]
        let X { i } = self;
        typle_for!(j in .. => t[[j]] + i)
    }

    pub fn inherent6<'a, T: Tuple<u32>>(&'a self, t: T) -> T {
        #[typle_attr_if(T::LEN == 0, allow(unused_variables))]
        let Self { i } = self;
        typle_for!(j in .. => t[[j]] + i)
    }
}
