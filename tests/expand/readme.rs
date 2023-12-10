use typle::typle;

use std::ops::Mul;

struct MyStruct<T> {
    pub t: T,
}

#[typle(Tuple for 0..=6)]
impl<T> MyStruct<T>
where
    T: Tuple
{
    fn new(t: T) -> Self {
        MyStruct { t }
    }

    fn multiply<M>(
        &self,
        multipliers: typle_for!(.. => M)
    ) -> typle_for!(i in .. => <T<{i}> as Mul<M>>::Output)
    where
        T::Types: Mul<M> + Copy,
    {
        typle_for!(i in .. => self.t[[i]] * multipliers[[i]])
    }
}

#[typle(Tuple for 1..=6)]
impl<T, C> MyStruct<T>
where
    T: Tuple<Types=C>,
    C: std::ops::AddAssign + Default + Copy,
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

trait HeadTail {
    type Head;
    type Tail;
    fn head(&self) -> Option<Self::Head>;
    fn tail(&self) -> Self::Tail;
}

#[typle(Tuple for 1..=6)]
impl<T> HeadTail for T
where
    T: Tuple,
    T::Types: Copy,
{
    type Head = T<0>;
    type Tail = typle_for!(i in 1.. => T<{i}>);

    fn head(&self) -> Option<Self::Head> {
        Some(self[[0]])
    }

    fn tail(&self) -> Self::Tail {
        typle_for!(i in 1.. => self[[i]])
    }
}
