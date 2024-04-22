use typle::typle;

// How to convert `Iterator<Item = Result<T, E>` to `Result<(Vec<T<0>>, Vec<T<1>>), E>`
// for any tuple size, generalizing the 2-tuple case described in
// https://users.rust-lang.org/t/unzip-with-error-handling/110250
// That is, implement the following `TryUnzip` trait for `Iterator<Item = Result<Tuple, Error>>`.
pub trait TryUnzip {
    type Output;
    type Error;

    fn try_unzip(self) -> Result<Self::Output, Self::Error>;
}

// typle` does not work when the tuple types are only associated types because
// associated types cannot distinguish implementations. The implementations for
// each tuple length conflict because they are all on the same type (in this
// case `Iterator`): https://github.com/rust-lang/rust/issues/20400

// #[typle(Tuple for 2..=3)]
// impl<I, T, E> TryUnzip for I
// where
//     I: Iterator<Item = Result<T, E>>,
//     T: Tuple,
// {
//     type Output = typle_for!(i in .. => Vec<T<{i}>>);
//     type Error = E;
//
//     fn try_unzip(self) -> Result<Self::Output, Self::Error> {
//         let mut vecs = typle_for!(i in .. => Vec::new());
//         for result in self {
//             let t = result?;
//             for typle_index!(i) in 0..T::LEN {
//                 vecs[[i]].push(t[[i]]);
//             }
//         }
//         Ok(vecs)
//     }
// }

// If the trait can be modified, then we can use a generic type for the tuple
// type instead of an associated type:

pub trait TryUnzipModified<Output> {
    type Error;

    fn try_unzip(self) -> Result<Output, Self::Error>;
}

#[typle(Tuple for 2..=3)]
impl<I, T, E> TryUnzipModified<typle_for!(i in .. => Vec<T<{i}>>)> for I
where
    I: Iterator<Item = Result<T, E>>,
    T: Tuple,
{
    type Error = E;

    fn try_unzip(self) -> Result<typle_for!(i in .. => Vec<T<{i}>>), Self::Error> {
        #[typle_attr_if(T::LEN == 0, allow(unused_mut))]
        let mut vecs = typle_for!(i in .. => Vec::new());
        for result in self {
            #[typle_attr_if(T::LEN == 0, allow(clippy::let_unit_value, unused_variables))]
            let t = result?;
            for typle_index!(i) in 0..T::LEN {
                vecs[[i]].push(t[[i]]);
            }
        }
        Ok(vecs)
    }
}

// A solution that does not modify the original trait is to add a new trait with
// an associated method, implement it on the tuple type, and call it from an
// impl for the original trait:

pub trait TryUnzipTuple<T, E> {
    type Output;

    fn try_unzip<I>(iter: I) -> Result<Self::Output, E>
    where
        I: Iterator<Item = Result<T, E>>;
}

#[typle(Tuple for 2..=3)]
impl<T, E> TryUnzipTuple<T, E> for T
where
    T: Tuple,
{
    type Output = typle_for!(i in .. => Vec<T<{i}>>);

    fn try_unzip<I>(iter: I) -> Result<Self::Output, E>
    where
        I: Iterator<Item = Result<T, E>>,
    {
        #[typle_attr_if(T::LEN == 0, allow(unused_mut))]
        let mut vecs = typle_for!(i in .. => Vec::new());
        for result in iter {
            #[typle_attr_if(T::LEN == 0, allow(clippy::let_unit_value, unused_variables))]
            let t = result?;
            for typle_index!(i) in 0..T::LEN {
                vecs[[i]].push(t[[i]]);
            }
        }
        Ok(vecs)
    }
}

impl<I, T, E> TryUnzip for I
where
    I: Iterator<Item = Result<T, E>>,
    T: TryUnzipTuple<T, E>,
{
    type Output = <T as TryUnzipTuple<T, E>>::Output;
    type Error = E;

    fn try_unzip(self) -> Result<Self::Output, Self::Error> {
        <T as TryUnzipTuple<T, E>>::try_unzip(self)
    }
}
