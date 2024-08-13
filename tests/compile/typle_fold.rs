use typle::typle;

pub struct Something {}

pub trait IsUseful<Other> {
    type State: IsUseful<Something>;
}

pub trait UsefulTrait {
    type UsefulType: IsUseful<Something>;

    const SIZE: usize;

    #[allow(dead_code)]
    fn display(&self) -> String;
}

#[typle(Tuple for 1..=3)]
impl<T: Tuple> UsefulTrait for T
where
    T<_>: UsefulTrait + std::fmt::Display,
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

    fn display(&self) -> String {
        typle_fold!(
            "[".to_string() + &self[[0]].to_string();
            i in 1.. => |s| s + "," + &self[[i]].to_string()
        ) + "]"
    }
}
