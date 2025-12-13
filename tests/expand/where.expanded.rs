use typle::typle;
impl TupleA<()> {}
impl TupleA<(u32,)> {}
impl TupleA<(u32, u32)> {}
impl TraitB for TupleB<()>
where
    (): Debug,
{}
impl<T0> TraitB for TupleB<(T0,)>
where
    (T0,): Debug,
    T0: Extract,
    T0::Output: AsRef<str>,
{}
impl<T0, T1> TraitB for TupleB<(T0, T1)>
where
    (T0, T1): Debug,
    T0: Extract,
    T1: Extract,
    T0::Output: AsRef<str>,
    T1::Output: AsRef<str>,
{}
impl<S0, T0> Extract for TupleC<(T0,)>
where
    (S0,): Debug,
    T0: Extract<Output = Option<(S0,)>>,
    (T0,): Debug,
{}
impl<S0, S1, T0, T1> Extract for TupleC<(T0, T1)>
where
    (S0, S1): Debug,
    S1: Extract,
    T0: Extract<Output = Option<(S0, S1)>>,
    (T0, T1): Debug,
{}
impl<T0, F> TraitD for TupleD<T0, !>
where
    T0: Mul<T0>,
    T0: AsRef<str>,
    F: Fn((T0,)) -> (T0,),
{
    fn g() {
        let f: TupleD<T0, !> = TupleD::<T0, !>::new();
        T0::output_to_bytestream();
    }
}
impl<T0, T1, F> TraitD for TupleD<T0, T1>
where
    T0: Mul<T1>,
    T1: Mul<T0>,
    T1: AsRef<str>,
    F: Fn((T0, T1)) -> (T0, T1),
{
    fn g() {
        let f: TupleD<T0, T1> = TupleD::<T0, T1>::new();
        T0::output_to_bytestream();
    }
}
