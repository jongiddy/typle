use typle::typle;

struct S<T>
{
    t: T
}

#[typle(Tuple for 0..=2)]
impl<T> S<T>
where
    T: Tuple<u32>,
{
    fn new(t: typle_for!(i in .. => &T<{i}>)) {
        // Square brackets create an array
        let a = typle_for![i in 0..T::LEN => *t[[i]] * 2];
        // Parentheses create a tuple
        // The default bounds of the range are 0..Tuple::LEN
        let b = typle_for!(i in .. => *t[[i]] * 2);
        // Arbitrary expressions can be used for the indices and
        // the iterator variable can be left out if not needed
        let init: [Option<u32>; T::LEN] = typle_for![T::LEN * 2..T::LEN * 3 => None];
    }
}
