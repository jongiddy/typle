use typle::typle;
use std::hash::{Hash, Hasher};
use std::ops::Mul;
#[allow(non_camel_case_types)]
trait _typle_fn_hash {
    type Return;
    fn apply(self) -> Self::Return;
}
fn hash<'a, T, S>(
    tuple: T,
    state: &'a mut S,
) -> <(T, &'a mut S) as _typle_fn_hash>::Return
where
    (T, &'a mut S): _typle_fn_hash,
{
    <(T, &'a mut S) as _typle_fn_hash>::apply((tuple, state))
}
impl<'a, T0, S> _typle_fn_hash for ((T0,), &'a mut S)
where
    T0: Hash,
    S: Hasher,
{
    type Return = ();
    fn apply(self) -> Self::Return {
        let (tuple, state) = self;
        {
            {
                {
                    tuple.0.hash(state);
                }
                ()
            }
        }
    }
}
impl<'a, T0, T1, S> _typle_fn_hash for ((T0, T1), &'a mut S)
where
    T0: Hash,
    T1: Hash,
    S: Hasher,
{
    type Return = ();
    fn apply(self) -> Self::Return {
        let (tuple, state) = self;
        {
            {
                {
                    tuple.0.hash(state);
                }
                {
                    tuple.1.hash(state);
                }
                ()
            }
        }
    }
}
impl<'a, T0, T1, T2, S> _typle_fn_hash for ((T0, T1, T2), &'a mut S)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    S: Hasher,
{
    type Return = ();
    fn apply(self) -> Self::Return {
        let (tuple, state) = self;
        {
            {
                {
                    tuple.0.hash(state);
                }
                {
                    tuple.1.hash(state);
                }
                {
                    tuple.2.hash(state);
                }
                ()
            }
        }
    }
}
#[allow(non_camel_case_types)]
trait _typle_fn_multiply {
    type Return;
    fn apply(self) -> Self::Return;
}
fn multiply<T, M>(t: T, m: M) -> <(T, M) as _typle_fn_multiply>::Return
where
    (T, M): _typle_fn_multiply,
{
    <(T, M) as _typle_fn_multiply>::apply((t, m))
}
impl<T0, M> _typle_fn_multiply for ((T0,), M)
where
    T0: Mul<M>,
    M: Copy,
{
    type Return = (<T0 as Mul<M>>::Output,);
    fn apply(self) -> Self::Return {
        let (t, m) = self;
        { (t.0 * m,) }
    }
}
impl<T0, T1, M> _typle_fn_multiply for ((T0, T1), M)
where
    T0: Mul<M>,
    T1: Mul<M>,
    M: Copy,
{
    type Return = (<T0 as Mul<M>>::Output, <T1 as Mul<M>>::Output);
    fn apply(self) -> Self::Return {
        let (t, m) = self;
        { (t.0 * m, t.1 * m) }
    }
}
impl<T0, T1, T2, M> _typle_fn_multiply for ((T0, T1, T2), M)
where
    T0: Mul<M>,
    T1: Mul<M>,
    T2: Mul<M>,
    M: Copy,
{
    type Return = (
        <T0 as Mul<M>>::Output,
        <T1 as Mul<M>>::Output,
        <T2 as Mul<M>>::Output,
    );
    fn apply(self) -> Self::Return {
        let (t, m) = self;
        { (t.0 * m, t.1 * m, t.2 * m) }
    }
}
#[allow(non_camel_case_types)]
trait _typle_fn_heapify {
    type Return;
    fn apply(self) -> Self::Return;
}
fn heapify<T>(params: T) -> <(T,) as _typle_fn_heapify>::Return
where
    (T,): _typle_fn_heapify,
{
    <(T,) as _typle_fn_heapify>::apply((params,))
}
impl<T0> _typle_fn_heapify for ((T0,),) {
    type Return = (Box<T0>,);
    fn apply(self) -> Self::Return {
        let (params,) = self;
        { (Box::new(params.0),) }
    }
}
impl<T0, T1> _typle_fn_heapify for ((T0, T1),) {
    type Return = (Box<T0>, Box<T1>);
    fn apply(self) -> Self::Return {
        let (params,) = self;
        { (Box::new(params.0), Box::new(params.1)) }
    }
}
impl<T0, T1, T2> _typle_fn_heapify for ((T0, T1, T2),) {
    type Return = (Box<T0>, Box<T1>, Box<T2>);
    fn apply(self) -> Self::Return {
        let (params,) = self;
        { (Box::new(params.0), Box::new(params.1), Box::new(params.2)) }
    }
}
#[allow(non_camel_case_types)]
trait _typle_fn_zip {
    type Return;
    fn apply(self) -> Self::Return;
}
fn zip<A, B>(first: A, second: B) -> <(A, B) as _typle_fn_zip>::Return
where
    (A, B): _typle_fn_zip,
{
    <(A, B) as _typle_fn_zip>::apply((first, second))
}
impl<A0, B0> _typle_fn_zip for ((A0,), (B0,)) {
    type Return = ((A0, B0),);
    fn apply(self) -> Self::Return {
        let (first, second) = self;
        { ((first.0, second.0),) }
    }
}
impl<A0, A1, B0, B1> _typle_fn_zip for ((A0, A1), (B0, B1)) {
    type Return = ((A0, B0), (A1, B1));
    fn apply(self) -> Self::Return {
        let (first, second) = self;
        { ((first.0, second.0), (first.1, second.1)) }
    }
}
impl<A0, A1, A2, B0, B1, B2> _typle_fn_zip for ((A0, A1, A2), (B0, B1, B2)) {
    type Return = ((A0, B0), (A1, B1), (A2, B2));
    fn apply(self) -> Self::Return {
        let (first, second) = self;
        { ((first.0, second.0), (first.1, second.1), (first.2, second.2)) }
    }
}
