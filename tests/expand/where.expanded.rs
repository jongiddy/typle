use typle::typle;
impl TupleA<()> {}
impl TupleA<(u32,)> {}
impl TupleA<(u32, u32)> {}
impl TraitB for TupleB<()> {}
impl<T0> TraitB for TupleB<(T0,)>
where
    T0: Extract,
    T0::Output: AsRef<str>,
{}
impl<T0, T1> TraitB for TupleB<(T0, T1)>
where
    T0: Extract,
    T1: Extract,
    T0::Output: AsRef<str>,
    T1::Output: AsRef<str>,
{}
impl<S0, T0> Extract for TupleC<(T0,)>
where
    S0: Extract,
    T0: Extract<Output = Option<(S0,)>>,
{}
impl<S0, S1, T0, T1> Extract for TupleC<(T0, T1)>
where
    S0: Extract,
    S1: Extract,
    T0: Extract<Output = Option<(S0, S1)>>,
    T1: Extract<Output = Option<(S0, S1)>>,
{}
