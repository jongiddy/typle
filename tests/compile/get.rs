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
        #[typle_attr_if(T::LEN == 1, allow(clippy::single_match))]
        match i {
            j @ typle_index!(0..T::LEN) => {
                self.t[[j]] = <T<{ j }> as Default>::default();
            }
            _ => {}
        }
        #[typle_attr_if(T::LEN == 1, allow(clippy::match_single_binding))]
        match i {
            j @ typle_index!(0..T::LEN - 1) => self.t[[j]].to_string(),
            _ => String::new(),
        };
        match i * 2 {
            j @ typle_index!(0..T::LEN) => self.t[[j]].to_string(),
            _ => String::new(),
        };
    }
}

#[typle(Tuple for 1..=6)]
fn get_component<'t, C, T>(t: &'t T, i: usize) -> Option<&'t C>
where
    T: Tuple<C>,
{
    match i {
        j @ typle_index!(0..T::LEN) => Some(&t[[j]]),
        _ => None,
    }
}

#[typle(Tuple for 1..=6)]
fn get_component_or_default<'t, C, T>(t: &'t T, i: usize, default: &'t C) -> &'t C
where
    T: Tuple<C>,
{
    match i {
        j @ typle_index!(0..T::LEN) => &t[[j]],
        _ => default,
    }
}
