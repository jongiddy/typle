use typle::typle;

struct MyStruct<T> {
    pub t: T,
}

#[typle(Tuple for 1..=3)]
impl<T> MyStruct<T>
where
    T: Tuple<Types=u32>,
{
    fn max(&self) -> Option<u32> {
        let mut max = self.t[[0]];
        for typle_const!(i) in 1..T::LEN {
            if self.t[[i]] > max {
                max = self.t[[i]];
            }
        }
        Some(max)
    }
}
