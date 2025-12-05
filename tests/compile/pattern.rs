#![allow(dead_code)]
use typle::typle;

pub struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 3..=3)]
impl<T: Tuple<u32>> MyStruct<T> {
    pub fn test_macro(&self) -> u32 {
        let (typle!(i in 0..2 => x::<typle_ident!(i)>)): (typle!(i in 0..2 => T<{i}>)) =
            (typle!(i in 0..2 => self.t[[i]] * 3));
        x0 + x1
    }

    pub fn component(&self) -> u32 {
        let x::<typle_ident!(1)>: T<1> = self.t.1;
        x1
    }

    pub fn test_slice(&self) -> u32 {
        let [x0, x1] = [typle!(i in 0..2 => self.t[[i]] * 3)];
        x0 + x1
    }

    pub fn test_tuple(&self) -> u32 {
        let (x0, x1) = (typle!(i in 0..2 => self.t[[i]] * 3));
        x0 + x1
    }
}

#[typle(Tuple for 1..=12)]
fn multiply_by<T: Tuple<u32>>(t: T, m: u32) -> T {
    // let (x0, x1, x2) = (t.0 * m, t.1 * m, t.2 * m);
    let (typle!(i in .. => x::<typle_ident!(i)>)) = (typle!(i in .. => t[[i]] * m));
    assert_eq!(x0, t.0 * m);
    (typle!(i in .. => x::<typle_ident!(i)>))
}
