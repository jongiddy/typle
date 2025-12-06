#![allow(dead_code)]
use typle::typle;

struct Test<const N: usize> {
    t: [usize; N],
}

// generic const that turns into literal is not wrapped in block
#[typle(Tuple for 0..=2)]
impl<'a> From<T> for Test<{ T::LEN }>
where
    T: Tuple<&'a str>,
{
    #[typle_attr_if(T::LEN == 0, allow(unused_variables))]
    fn from(t: T) -> Self {
        Self {
            t: [typle!(i in .. => t[[i]].len())],
        }
    }
}

const X: usize = 1;

// generic const variable stays wrapped in block
#[typle(Tuple for 0..=2)]
impl<'a> From<(usize, T)> for Test<{ X }>
where
    T: Tuple<&'a str>,
{
    fn from(value: (usize, T)) -> Self {
        Self { t: [value.0] }
    }
}

// generic const literal value does not change
#[typle(Tuple for 0..=2)]
impl<'a> From<(u8, T)> for Test<1>
where
    T: Tuple<&'a str>,
{
    fn from(value: (u8, T)) -> Self {
        Self {
            t: [value.0 as usize],
        }
    }
}
