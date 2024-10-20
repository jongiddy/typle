#![allow(dead_code)]
use typle::typle;

pub struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 1..=2, never=std::convert::Infallible)]
impl<T> MyStruct<T>
where
    T: Tuple,
    T<_>: Default + ToString,
{
    fn select(&mut self) {
        let i = 1;
        typle_get!(if i in .. {self.t[[i]] = <T<{i}> as Default>::default();});
        let _: String = typle_get!(if i in .. {self.t[[i]].to_string()} else { String::new() });
        match typle_const!(i * 2) {
            j@.. => {self.t[[j]].to_string()},
            _ => { String::new() }
        }
    }
}
