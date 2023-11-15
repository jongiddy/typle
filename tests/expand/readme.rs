use typle::typle;

struct MyStruct<S, T> {
    s: S,
    t: Option<T>,
}

#[typle(Tuple for 0..=12)]
impl<S, T> MyStruct<S, T>
where
    S: Tuple<u32>,
    T: Tuple<impl Sized>,
{
    fn new(s: S, t: Option<T>) -> MyStruct<S, T> {
        MyStruct { s, t }
    }

    fn sum(&self) -> u32 {
        let mut sum = 0;
        for typle_const!(i) in 0..S::LEN {
            sum += self.s[[i]];
        }
        sum
    }

    fn multiply(&self, multipliers: S) -> typle_expand!(u64) {
        typle_expand!(self.s[[S::INDEX]] as u64 * multipliers[[S::INDEX]] as u64)
    }
}

trait FirstLast {
    type F;
    type L;
    fn first(&self) -> Option<Self::F>;
    fn last(&self) -> Option<Self::L>;
}

#[typle(Tuple for 1..=12)]
impl<S, T> FirstLast for MyStruct<S, T>
where
    T: Tuple<impl Copy>,
{
    type F = T<0>;
    type L = T<{ T::LEN - 1 }>;

    fn first(&self) -> Option<Self::F> {
        self.t.map(|tup| tup[[0]])
    }

    fn last(&self) -> Option<Self::L> {
        self.t.map(|tup| tup[[T::LEN - 1]])
    }
}
