#![allow(dead_code)]
use typle::typle;

struct MyStruct<T> {
    pub t: T,
}

#[typle(Tuple for 1..=3)]
impl<T: Tuple<u32>> MyStruct<T> {
    fn max(&self) -> Option<u32> {
        #[typle_attr_if(T::LEN == 1, allow(unused_mut))] // For LEN=1 `max` does not get mutated
        let mut max = self.t[[0]];
        for typle_const!(i) in 1..T::LEN {
            if self.t[[i]] > max {
                max = self.t[[i]];
            }
        }
        Some(max)
    }
}
