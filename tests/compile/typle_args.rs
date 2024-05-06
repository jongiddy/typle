#![allow(unused)]
use typle::typle;

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

struct Func {}

// https://github.com/jongiddy/bevy/blob/ac91b191/crates/bevy_ecs/src/system/exclusive_function_system.rs#L183
#[typle(Tuple for 0..=3)]
impl<Out, Func: Send + Sync + 'static, F: Tuple> ExclusiveSystemParamFunction<fn(F<{ .. }>) -> Out>
    for Func
where
    for<'a> &'a mut Func: FnMut(&mut World, F<{ .. }>) -> Out
        + FnMut(&mut World, typle_args!(i in .. => ExclusiveSystemParamItem<F<{i}>>)) -> Out,
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
