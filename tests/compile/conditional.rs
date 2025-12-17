#![allow(unused)]

use typle::typle;

trait Trait {
    type Input;
    type Output;

    fn method(&self, input: Self::Input) -> Self::Output;
}

struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 0..=3)]
#[typle_attr_if(T::LEN == 1, rustfmt::skip)]
impl<T: Tuple> Trait for MyStruct<T> {
    type Input = typle!(=> if T::LEN == 0 { () } else { T<{0}> });
    type Output = typle!(=> if T::LEN == 0 { () } else { T<{0}> });

    fn method(
        &self,
        input: typle!(=> if T::LEN == 0 { () } else { T<{0}>}),
    ) -> typle!(=> if T::LEN == 0 { () } else { T<{0}> }) {
        typle!(=> if T::LEN == 0 { () } else { input })
    }
}
