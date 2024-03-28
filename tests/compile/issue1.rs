#![allow(unused)]
use typle::typle;

pub trait Verifier {
    type Error;

    fn verify(&self, report: &()) -> Result<(), Self::Error>;
}

#[typle(Tuple for 1..=3)]
impl<T> Verifier for T
where
    T: Tuple,
    T<_>: Verifier,
    // Cannot expand nested bounds directly. Instead of:
    // <T<_> as Verifier>::Error: Into<Box<dyn std::error::Error>>,
    // this requires:
    typle_bound!(i in ..T::LEN => <T<{i}> as Verifier>::Error): Into<Box<dyn std::error::Error>>,
{
    type Error = Box<dyn std::error::Error>;

    fn verify(&self, _report: &()) -> Result<(), Self::Error> {
        Ok(())
    }
}
