#![allow(dead_code)]
use typle::typle;

struct MyStruct<T> {
    t: T,
}

#[typle(Tuple for 3..=3)]
impl<T: Tuple> MyStruct<T> {
    fn call_macro(_t: T) {
        stringify!(T);
        stringify!(T::LEN);
        stringify!(typle_ty!(T));
        stringify!(typle_expr!(T::LEN));
    }
}
