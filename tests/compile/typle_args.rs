#![allow(unused)]
use typle::typle;

#[typle(Tuple for 0..=3)]
fn append<T: Tuple, A>(t: T, a: A) -> (T<{ .. }>, A) {
    (t[[..]], a)
}

#[typle(Tuple for 1..=3)]
fn append_array<T: Tuple<bool>>(t: T, a: bool) -> [bool; T::LEN + 1] {
    [t[[..]], a]
}

#[typle(Tuple for 1..=3)]
fn append_double<T: Tuple<u32>>(t: T, a: u32) -> [u32; T::LEN + 1] {
    [typle!(i in .. => 2 * t[[i]]), 2 * a]
}

#[typle(Tuple for 0..=3)]
fn append_even<T: Tuple>(t: T, a: u32) -> (typle!(i in .. => if i % 2 == 0 {T<{i}>}), u32) {
    (typle!(i in .. => if i % 2 == 0 {t[[i]]}), a)
}

#[typle(Tuple for 0..=3)]
fn even_string_odd<T: Tuple>(t: T) -> (typle!(i in .. => if i % 2 == 0 {String} else {T<{i}>}))
where
    typle!(i in .. => if i % 2 == 0 {T<{i}>: ToString}): Tuple::Bounds,
{
    #[typle_attr_if(T::LEN == 0, allow(clippy::unused_unit))]
    (typle!(i in .. => if i % 2 == 0 {t[[i]].to_string()} else {t[[i]]}))
}

struct World {}

trait ExclusiveSystemParam {}

struct ExclusiveSystemParamItem<F> {
    f: F,
}

trait ExclusiveSystemParamFunction<F> {
    type In;
    type Out;
    type Param;

    fn run(
        &mut self,
        world: &mut World,
        _in: Self::In,
        param_value: ExclusiveSystemParamItem<Self::Param>,
    ) -> Self::Out;
}

// https://github.com/jongiddy/bevy/blob/ac91b191/crates/bevy_ecs/src/system/exclusive_function_system.rs#L183
#[typle(Tuple for 0..=3)]
impl<Out, Func: Send + Sync + 'static, F: Tuple> ExclusiveSystemParamFunction<fn(F<{ .. }>) -> Out>
    for Func
where
    for<'a> &'a mut Func: FnMut(&mut World, F<{ .. }>) -> Out
        + FnMut(&mut World, typle!(i in .. => ExclusiveSystemParamItem<F<{i}>>)) -> Out,
    Out: 'static,
    F<_>: ExclusiveSystemParam,
{
    type In = ();
    type Out = Out;
    type Param = F;
    #[inline]
    fn run(
        &mut self,
        world: &mut World,
        _in: (),
        param_value: ExclusiveSystemParamItem<F>,
    ) -> Self::Out {
        todo!()
    }
}

#[derive(Clone)]
struct Input {}

trait HandleStuff {
    type Output;

    fn handle_stuff(&self, input: Input) -> Self::Output;
}

struct MultipleHandlers<T> {
    handlers: T,
}

#[typle(Tuple for 1..=3)]
impl<T> HandleStuff for MultipleHandlers<T>
where
    T: Tuple,          // `T` is a tuple with 1 to 3 components.
    T<_>: HandleStuff, // All components implement `HandleStuff`.
{
    // Return a tuple of output from each handler applied to the same input.
    type Output = (typle! {i in .. => T<{i}>::Output});

    fn handle_stuff(&self, input: Input) -> Self::Output {
        (
            typle! {
                i in ..T::LAST => self.handlers[[i]].handle_stuff(input.clone())
            },
            // Avoid expensive clone for the last handler.
            self.handlers[[T::LAST]].handle_stuff(input),
        )
    }
}
