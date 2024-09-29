#![allow(dead_code)]
use typle::typle;

pub struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 1..=2, never=std::convert::Infallible)]
impl<T> MyStruct<T>
where
    T: Tuple,
    T<_>: ToString,
    T<0>: Default,
{
    fn select(&mut self) {
        typle_get!(&self.t[0]);
        typle_get!(&self.t[1]);
        typle_get!(&self.t[2]);
        let _: Option<String> = typle_get!(&self.t[1], ToString::to_string);
        let _: Option<String> = typle_get!(&self.t[1], |s| s.to_string());
        *typle_get!(&mut self.t[0]).unwrap() = <T<0> as Default>::default();
        let i = 1;
        let _: Option<String> = typle_get!(&self.t[i], ToString::to_string);
    }
}
