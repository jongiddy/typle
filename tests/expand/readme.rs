use typle::typle;

use std::ops::{AddAssign, Mul};

struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 0..=12)]
impl<T> MyStruct<T>
where
    T: Tuple
{
    fn new(t: T) -> Self {
        MyStruct { t }
    }
}

#[typle(Tuple for 1..=12)]
impl<T, C> MyStruct<T>
where
    T: Tuple(C),
    C: AddAssign + Default + Copy,
{
    fn even_odd(&self) -> (C, C) {
        let mut even = C::default();
        let mut odd = C::default();
        for typle_const!(i) in 0..T::LEN {
            if typle_const!(i % 2 == 0) {
                even += self.t[[i]];
            } else {
                odd += self.t[[i]];
            }
        }
        (even, odd)
    }
}

#[typle(Tuple for 1..=12)]
impl<T, C> MyStruct<T>
where
    T: Tuple(C),
    C: Mul<u32> + Copy,
{
    fn multiply(&self, multipliers: typle_for!(.. => u32)) -> typle_for!(.. => <C as Mul<u32>>::Output) {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]])
    }
}

trait HeadTail {
    type Head;
    type Tail;
    fn head(&self) -> Option<Self::Head>;
    fn tail(&self) -> Self::Tail;
}

#[typle(Tuple for 1..=12)]
impl<T> HeadTail for MyStruct<T>
where
    T: Tuple,
    T::Types: Copy,
{
    type Head = T<0>;
    type Tail = typle_for!(i in 1.. => T<{i}>);

    fn head(&self) -> Option<Self::Head> {
        Some(self.t[[0]])
    }

    fn tail(&self) -> Self::Tail {
        typle_for!(i in 1.. => self.t[[i]])
    }
}
