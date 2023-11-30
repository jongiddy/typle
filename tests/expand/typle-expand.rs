use typle::typle;

#[typle(Tuple for 0..=2)]
struct S<'a, T>
where
    T: Tuple<u32>
{
    t: typle_expand!(&'a T<INDEX>),
}

#[typle(Tuple for 0..=2)]
impl<'a, T> S<'a, (T)>
where
    T: Tuple<u32>
{
    fn new(t: typle_expand!(&T<INDEX>)) {
        let a = typle_expand![*t[[INDEX]] * 2];
        let b = typle_expand!(*t[[INDEX]] * 2);
    }
}
