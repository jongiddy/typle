use typle::typle;

// Tuple with all components of the same specific type
#[typle(Tuple for 0..=2)]
impl<T: Tuple<u32>> TupleA<T> {}

// Tuple with components that can be different types, but all bound by the
// same traits.
#[typle(Tuple for 0..=2)]
impl<T> TraitB for TupleB<T>
where
    T: Tuple + Debug,
    T<_>: Extract,
    T<_>::Output: AsRef<str>,
{
}

// Tuples can have some components bound by using a range. A single component
// can be named using a constant.
#[typle(Tuple for 1..=2)]
impl<S, T> Extract for TupleC<T>
where
    S: Debug + Tuple,
    S<{ 1.. }>: Extract,
    T: Tuple,
    T<0>: Extract<Output = Option<S>>,
    T: Debug,
{
}

// typle_bound! allows the component index to be used in the trait bound
#[typle(Tuple for 1..=2)]
impl<T, F> TraitD for TupleD<T<{ ..T::MAX }>>
where
    T: Tuple,
    typle_bound!(i in .. => T<{i}>): Mul<T<{ T::LEN - i - 1 }>>,
    T<{ T::LEN - 1 }>: AsRef<str>,
    F: Fn(T) -> T,
{
    fn g() {
        // T{ ..T::MAX } expands to all types in a referenced enum
        let f: TupleD<T<{ ..T::MAX }>> = TupleD::<T<{ ..T::MAX }>>::new();
        T::<0>::output_to_bytestream();
    }
}
