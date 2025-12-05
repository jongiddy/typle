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

trait CoalesceSome<T> {
    type Output;
    /// Coalesce a tuple of Options into an Option of tuple that is `Some`
    /// only if all the components are `Some`.
    fn coalesce_some(self) -> Option<Self::Output>;
}

#[typle(Tuple for 0..=2)]
impl<T: Tuple> CoalesceSome<T> for (typle!(i in .. => Option<T<{i}>>)) {
    type Output = T;
    fn coalesce_some(self) -> Option<Self::Output>
    {
        #[allow(unused_variables)]
        typle_fold!(
            ();  // Initially an empty tuple
            i in ..=T::LEN => |acc| if typle_const!(i == T::LEN) {
                // Final iteration: wrap accumulated tuple in `Some`
                Some(acc)
            } else if let Some(curr) = self[[i]] {
                // Append the current value to the prior tuple to create a
                // new accumulator with the type `(T<0>,...,T<{i}>)`
                (acc[[..i]], curr)
            } else {
                // If `None` is found at any point, short-circuit with a `None` result
                break None;
            }
        )
    }
}
