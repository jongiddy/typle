use typle::typle;

pub struct Something {}

pub trait IsUseful<Other> {
    type State: IsUseful<Something>;
}

pub trait UsefulTrait {
    type UsefulType: IsUseful<Something>;

    const SIZE: usize;
}

#[typle(Tuple for 1..=3)]
impl<T: Tuple> UsefulTrait for T
where
    T<_>: UsefulTrait,
    typle_bound!(i in 1.. => T<{i}>::UsefulType):IsUseful<
        typle_fold!(
            T<0>::UsefulType;
            j in 1..i => |Inner| <T<{j}>::UsefulType as IsUseful<Inner>>::State
        ),
    >,
{
    type UsefulType = typle_fold!(
        T<0>::UsefulType;
        j in 1.. => |Inner| <T<{j}>::UsefulType as IsUseful<Inner>>::State
    );

    const SIZE: usize = typle_fold!(0; i in .. => |total| total + T::<{i}>::SIZE);
}
