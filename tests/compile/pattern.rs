#![allow(dead_code)]
use typle::typle;

pub struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 3..=3)]
impl<T: Tuple<u32>> MyStruct<T> {
    pub fn test_macro(&self) -> u32 {
        let typle_for!(i in 0..2 => x::<typle_ident!(i)>): typle_for!(i in 0..2 => T<{i}>) =
            typle_for!(i in 0..2 => self.t[[i]] * 3);
        x0 + x1
    }

    pub fn component(&self) -> u32 {
        let x::<typle_ident!(1)>: T<1> = self.t.1;
        x1
    }

    pub fn test_slice(&self) -> u32 {
        let [x0, x1] = typle_for![i in 0..2 => self.t[[i]] * 3];
        x0 + x1
    }

    pub fn test_tuple(&self) -> u32 {
        let (x0, x1) = typle_for!(i in 0..2 => self.t[[i]] * 3);
        x0 + x1
    }
}
