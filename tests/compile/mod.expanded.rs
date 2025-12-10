pub mod const_generic {
    #![allow(dead_code)]
    use typle::typle;
    struct Test<const N: usize> {
        t: [usize; N],
    }
    impl<'a> From<()> for Test<0> {
        #[allow(unused_variables)]
        fn from(t: ()) -> Self {
            Self { t: [] }
        }
    }
    impl<'a> From<(&'a str,)> for Test<1> {
        fn from(t: (&'a str,)) -> Self {
            Self { t: [t.0.len()] }
        }
    }
    impl<'a> From<(&'a str, &'a str)> for Test<2> {
        fn from(t: (&'a str, &'a str)) -> Self {
            Self { t: [t.0.len(), t.1.len()] }
        }
    }
    const X: usize = 1;
    impl<'a> From<(usize, ())> for Test<{ X }> {
        fn from(value: (usize, ())) -> Self {
            Self { t: [value.0] }
        }
    }
    impl<'a> From<(usize, (&'a str,))> for Test<{ X }> {
        fn from(value: (usize, (&'a str,))) -> Self {
            Self { t: [value.0] }
        }
    }
    impl<'a> From<(usize, (&'a str, &'a str))> for Test<{ X }> {
        fn from(value: (usize, (&'a str, &'a str))) -> Self {
            Self { t: [value.0] }
        }
    }
    impl<'a> From<(u8, ())> for Test<1> {
        fn from(value: (u8, ())) -> Self {
            Self { t: [value.0 as usize] }
        }
    }
    impl<'a> From<(u8, (&'a str,))> for Test<1> {
        fn from(value: (u8, (&'a str,))) -> Self {
            Self { t: [value.0 as usize] }
        }
    }
    impl<'a> From<(u8, (&'a str, &'a str))> for Test<1> {
        fn from(value: (u8, (&'a str, &'a str))) -> Self {
            Self { t: [value.0 as usize] }
        }
    }
}
pub mod doc_typle {
    #![allow(dead_code)]
    use typle::typle;
    struct MyStruct<T> {
        pub t: T,
    }
    impl MyStruct<(u32,)> {
        fn max(&self) -> Option<u32> {
            #[allow(unused_mut)]
            let mut max = self.t.0;
            Some(max)
        }
    }
    impl MyStruct<(u32, u32)> {
        fn max(&self) -> Option<u32> {
            let mut max = self.t.0;
            loop {
                {
                    if self.t.1 > max {
                        max = self.t.1;
                    }
                }
                break;
            }
            Some(max)
        }
    }
    impl MyStruct<(u32, u32, u32)> {
        fn max(&self) -> Option<u32> {
            let mut max = self.t.0;
            loop {
                {
                    if self.t.1 > max {
                        max = self.t.1;
                    }
                }
                {
                    if self.t.2 > max {
                        max = self.t.2;
                    }
                }
                break;
            }
            Some(max)
        }
    }
    /// Trait for types that can treated as an infinitely wrapping sequence of chars.
    trait WrappingString {
        /// Return a 2 character substring starting at position `start`.
        fn wrapping_substring_at(&self, start: usize) -> String;
    }
    impl WrappingString for (char,) {
        #[allow(unused)]
        fn wrapping_substring_at(&self, start: usize) -> String {
            { [self.0, self.0].into_iter().collect() }
        }
    }
    impl WrappingString for (char, char) {
        fn wrapping_substring_at(&self, start: usize) -> String {
            {
                match start % 2 {
                    0 => [self.0, self.1].into_iter().collect(),
                    1 => [self.1, self.0].into_iter().collect(),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    impl WrappingString for (char, char, char) {
        fn wrapping_substring_at(&self, start: usize) -> String {
            {
                match start % 3 {
                    0 => [self.0, self.1].into_iter().collect(),
                    1 => [self.1, self.2].into_iter().collect(),
                    2 => [self.2, self.0].into_iter().collect(),
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                }
            }
        }
    }
    mod tuple {
        pub trait Extract {
            type State;
            type Output;
            fn extract(&self, state: Option<Self::State>) -> Self::Output;
        }
        impl Extract for () {
            type State = ();
            type Output = ();
            fn extract(&self, _state: Option<Self::State>) -> Self::Output {}
        }
        pub enum TupleSequenceState<T0, T1, T2, T3>
        where
            T0: Extract,
            T1: Extract,
            T2: Extract,
            T3: Extract,
        {
            S0((), Option<<T0>::State>),
            S1((<T0>::Output,), Option<<T1>::State>),
            S2((<T0>::Output, <T1>::Output), Option<<T2>::State>),
            S3((<T0>::Output, <T1>::Output, <T2>::Output), Option<<T3>::State>),
        }
        pub struct TupleSequence<T> {
            tuple: T,
        }
        impl<T0> Extract for TupleSequence<(T0,)>
        where
            T0: Extract,
        {
            type State = TupleSequenceState<T0, (), (), ()>;
            type Output = (<T0 as Extract>::Output,);
            fn extract(&self, state: Option<Self::State>) -> Self::Output {
                #[allow(unused_mut)]
                let mut state = state.unwrap_or(Self::State::S0((), None));
                loop {
                    {
                        #[allow(irrefutable_let_patterns)] #[allow(unused_variables)]
                        if let Self::State::S0(output, inner_state) = state {
                            let matched = self.tuple.0.extract(inner_state);
                            let output = (matched,);
                            {
                                return output;
                            }
                        }
                    }
                    break;
                }
                ::core::panicking::panic("internal error: entered unreachable code");
            }
        }
        impl<T0, T1> Extract for TupleSequence<(T0, T1)>
        where
            T0: Extract,
            T1: Extract,
        {
            type State = TupleSequenceState<T0, T1, (), ()>;
            type Output = (<T0 as Extract>::Output, <T1 as Extract>::Output);
            fn extract(&self, state: Option<Self::State>) -> Self::Output {
                let mut state = state.unwrap_or(Self::State::S0((), None));
                loop {
                    {
                        #[allow(unused_variables)]
                        if let Self::State::S0(output, inner_state) = state {
                            let matched = self.tuple.0.extract(inner_state);
                            let output = (matched,);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = (output.0, matched);
                            {
                                return output;
                            }
                        }
                    }
                    break;
                }
                ::core::panicking::panic("internal error: entered unreachable code");
            }
        }
        impl<T0, T1, T2> Extract for TupleSequence<(T0, T1, T2)>
        where
            T0: Extract,
            T1: Extract,
            T2: Extract,
        {
            type State = TupleSequenceState<T0, T1, T2, ()>;
            type Output = (
                <T0 as Extract>::Output,
                <T1 as Extract>::Output,
                <T2 as Extract>::Output,
            );
            fn extract(&self, state: Option<Self::State>) -> Self::Output {
                let mut state = state.unwrap_or(Self::State::S0((), None));
                loop {
                    {
                        #[allow(unused_variables)]
                        if let Self::State::S0(output, inner_state) = state {
                            let matched = self.tuple.0.extract(inner_state);
                            let output = (matched,);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = (output.0, matched);
                            {
                                state = Self::State::S2(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S2(output, inner_state) = state {
                            let matched = self.tuple.2.extract(inner_state);
                            let output = (output.0, output.1, matched);
                            {
                                return output;
                            }
                        }
                    }
                    break;
                }
                ::core::panicking::panic("internal error: entered unreachable code");
            }
        }
        impl<T0, T1, T2, T3> Extract for TupleSequence<(T0, T1, T2, T3)>
        where
            T0: Extract,
            T1: Extract,
            T2: Extract,
            T3: Extract,
        {
            type State = TupleSequenceState<T0, T1, T2, T3>;
            type Output = (
                <T0 as Extract>::Output,
                <T1 as Extract>::Output,
                <T2 as Extract>::Output,
                <T3 as Extract>::Output,
            );
            fn extract(&self, state: Option<Self::State>) -> Self::Output {
                let mut state = state.unwrap_or(Self::State::S0((), None));
                loop {
                    {
                        #[allow(unused_variables)]
                        if let Self::State::S0(output, inner_state) = state {
                            let matched = self.tuple.0.extract(inner_state);
                            let output = (matched,);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = (output.0, matched);
                            {
                                state = Self::State::S2(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S2(output, inner_state) = state {
                            let matched = self.tuple.2.extract(inner_state);
                            let output = (output.0, output.1, matched);
                            {
                                state = Self::State::S3(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S3(output, inner_state) = state {
                            let matched = self.tuple.3.extract(inner_state);
                            let output = (output.0, output.1, output.2, matched);
                            {
                                return output;
                            }
                        }
                    }
                    break;
                }
                ::core::panicking::panic("internal error: entered unreachable code");
            }
        }
    }
}
pub mod for_loop {
    #![allow(unreachable_code, unused)]
    use typle::typle;
    struct Looper<T> {
        t: T,
    }
    #[allow(clippy::never_loop)]
    impl<T0, T1, T2, T3, T4, T5, T6, T7> Looper<(T0, T1, T2, T3, T4, T5, T6, T7)> {
        fn for_loop(&self) {
            loop {
                {
                    {
                        let _x = 0;
                    }
                    {}
                }
                {
                    {
                        let _x = 2;
                        break;
                    }
                    {}
                }
                let mut _typle_break = false;
                loop {
                    if _typle_break {
                        _typle_break = false;
                        break;
                    }
                    _typle_break = true;
                    {
                        {
                            let _x = 3;
                            continue;
                        }
                        {}
                    }
                }
                'label: loop {
                    if _typle_break {
                        _typle_break = false;
                        break;
                    }
                    _typle_break = true;
                    {
                        {
                            let _x = 4;
                            break 'label;
                        }
                        {}
                    }
                }
                if _typle_break {
                    break;
                }
                {
                    {
                        for _j in 0..2 {
                            continue;
                        }
                        while 5 == 3 {
                            continue;
                        }
                        loop {
                            break;
                        }
                    }
                }
                break;
            }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_continue {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<T0, T1, T2, T3> _typle_fn_do_continue for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
        fn apply(self) -> Self::Return {
            let (t,) = self;
            {
                let mut output = Vec::new();
                loop {
                    {
                        output.push(0);
                    }
                    {
                        output.push(1);
                    }
                    let mut _typle_break = false;
                    loop {
                        if _typle_break {
                            _typle_break = false;
                            break;
                        }
                        _typle_break = true;
                        {
                            {
                                continue;
                            }
                            output.push(2);
                        }
                    }
                    {
                        output.push(3);
                    }
                    break;
                }
                output
            }
        }
    }
    pub fn do_continue<T>(t: T) -> <(T,) as _typle_fn_do_continue>::Return
    where
        (T,): _typle_fn_do_continue,
    {
        <(T,) as _typle_fn_do_continue>::apply((t,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_continue_labelled {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<T0, T1, T2, T3> _typle_fn_do_continue_labelled for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
        #[allow(clippy::never_loop)]
        fn apply(self) -> Self::Return {
            let (t,) = self;
            {
                let mut output = Vec::new();
                loop {
                    {
                        loop {
                            output.push(0);
                            break;
                        }
                    }
                    {
                        loop {
                            output.push(1);
                            break;
                        }
                    }
                    let mut _typle_break = false;
                    'label: loop {
                        if _typle_break {
                            _typle_break = false;
                            break;
                        }
                        _typle_break = true;
                        {
                            loop {
                                {
                                    continue 'label;
                                }
                                output.push(2);
                                break;
                            }
                        }
                    }
                    if _typle_break {
                        break;
                    }
                    {
                        loop {
                            output.push(3);
                            break;
                        }
                    }
                    break;
                }
                output
            }
        }
    }
    pub fn do_continue_labelled<T>(
        t: T,
    ) -> <(T,) as _typle_fn_do_continue_labelled>::Return
    where
        (T,): _typle_fn_do_continue_labelled,
    {
        <(T,) as _typle_fn_do_continue_labelled>::apply((t,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_break {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<T0, T1, T2, T3> _typle_fn_do_break for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
        fn apply(self) -> Self::Return {
            let (t,) = self;
            {
                let mut output = Vec::new();
                loop {
                    {
                        output.push(0);
                    }
                    {
                        output.push(1);
                    }
                    {
                        {
                            break;
                        }
                        output.push(2);
                    }
                    {
                        output.push(3);
                    }
                    break;
                }
                output
            }
        }
    }
    pub fn do_break<T>(t: T) -> <(T,) as _typle_fn_do_break>::Return
    where
        (T,): _typle_fn_do_break,
    {
        <(T,) as _typle_fn_do_break>::apply((t,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_break_labelled {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<T0, T1, T2, T3> _typle_fn_do_break_labelled for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
        #[allow(clippy::never_loop)]
        fn apply(self) -> Self::Return {
            let (t,) = self;
            {
                let mut output = Vec::new();
                loop {
                    {
                        loop {
                            output.push(0);
                            break;
                        }
                    }
                    {
                        loop {
                            output.push(1);
                            break;
                        }
                    }
                    let mut _typle_break = false;
                    'label: loop {
                        if _typle_break {
                            _typle_break = false;
                            break;
                        }
                        _typle_break = true;
                        {
                            loop {
                                {
                                    break 'label;
                                }
                                output.push(2);
                                break;
                            }
                        }
                    }
                    if _typle_break {
                        break;
                    }
                    {
                        loop {
                            output.push(3);
                            break;
                        }
                    }
                    break;
                }
                output
            }
        }
    }
    pub fn do_break_labelled<T>(t: T) -> <(T,) as _typle_fn_do_break_labelled>::Return
    where
        (T,): _typle_fn_do_break_labelled,
    {
        <(T,) as _typle_fn_do_break_labelled>::apply((t,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_check_out_of_bounds {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_check_out_of_bounds for ((),) {
        type Return = usize;
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t,) = self;
            {
                let mut count = 0;
                loop {
                    {
                        count += 1;
                    }
                    {
                        count += 1;
                    }
                    {
                        count += 1;
                    }
                    break;
                }
                count
            }
        }
    }
    pub fn check_out_of_bounds<T>(
        t: T,
    ) -> <(T,) as _typle_fn_check_out_of_bounds>::Return
    where
        (T,): _typle_fn_check_out_of_bounds,
    {
        <(T,) as _typle_fn_check_out_of_bounds>::apply((t,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_check_negative_range {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_check_negative_range for ((),) {
        type Return = usize;
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t,) = self;
            {
                #[allow(clippy::let_and_return)]
                let mut count = 0;
                count
            }
        }
    }
    pub fn check_negative_range<T>(
        t: T,
    ) -> <(T,) as _typle_fn_check_negative_range>::Return
    where
        (T,): _typle_fn_check_negative_range,
    {
        <(T,) as _typle_fn_check_negative_range>::apply((t,))
    }
}
pub mod function {
    use typle::typle;
    use std::hash::{Hash, Hasher};
    use std::ops::Mul;
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_hash {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a, T0, S: Hasher> _typle_fn_hash for (&'a (T0,), &'a mut S)
    where
        T0: Hash,
        T0: ?Sized,
    {
        type Return = ();
        fn apply(self) -> Self::Return {
            let (tuple, state) = self;
            {
                loop {
                    {
                        tuple.0.hash(state);
                    }
                    break;
                }
            }
        }
    }
    impl<'a, T0, T1, S: Hasher> _typle_fn_hash for (&'a (T0, T1), &'a mut S)
    where
        T0: Hash,
        T1: Hash,
        T1: ?Sized,
    {
        type Return = ();
        fn apply(self) -> Self::Return {
            let (tuple, state) = self;
            {
                loop {
                    {
                        tuple.0.hash(state);
                    }
                    {
                        tuple.1.hash(state);
                    }
                    break;
                }
            }
        }
    }
    impl<'a, T0, T1, T2, S: Hasher> _typle_fn_hash for (&'a (T0, T1, T2), &'a mut S)
    where
        T0: Hash,
        T1: Hash,
        T2: Hash,
        T2: ?Sized,
    {
        type Return = ();
        fn apply(self) -> Self::Return {
            let (tuple, state) = self;
            {
                loop {
                    {
                        tuple.0.hash(state);
                    }
                    {
                        tuple.1.hash(state);
                    }
                    {
                        tuple.2.hash(state);
                    }
                    break;
                }
            }
        }
    }
    pub fn hash<'a, T, S>(
        tuple: &'a T,
        state: &'a mut S,
    ) -> <(&'a T, &'a mut S) as _typle_fn_hash>::Return
    where
        (&'a T, &'a mut S): _typle_fn_hash,
    {
        <(&'a T, &'a mut S) as _typle_fn_hash>::apply((tuple, state))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_multiply {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<M> _typle_fn_multiply for ((), M)
    where
        M: Copy,
    {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t, m) = self;
            { () }
        }
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
    fn multiply<T, M>(t: T, m: M) -> <(T, M) as _typle_fn_multiply>::Return
    where
        (T, M): _typle_fn_multiply,
    {
        <(T, M) as _typle_fn_multiply>::apply((t, m))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_heapify {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_heapify for ((),) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (params,) = self;
            { () }
        }
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
    fn heapify<T>(params: T) -> <(T,) as _typle_fn_heapify>::Return
    where
        (T,): _typle_fn_heapify,
    {
        <(T,) as _typle_fn_heapify>::apply((params,))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_zip {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_zip for ((), ()) {
        type Return = ();
        #[rustfmt::skip]
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (first, second) = self;
            { () }
        }
    }
    impl<A0, B0> _typle_fn_zip for ((A0,), (B0,)) {
        type Return = ((A0, B0),);
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            { ((first.0, second.0),) }
        }
    }
    impl<A0, A1, B0, B1> _typle_fn_zip for ((A0, A1), (B0, B1)) {
        type Return = ((A0, B0), (A1, B1));
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            { ((first.0, second.0), (first.1, second.1)) }
        }
    }
    impl<A0, A1, A2, B0, B1, B2> _typle_fn_zip for ((A0, A1, A2), (B0, B1, B2)) {
        type Return = ((A0, B0), (A1, B1), (A2, B2));
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            { ((first.0, second.0), (first.1, second.1), (first.2, second.2)) }
        }
    }
    impl<A0, A1, A2, A3, B0, B1, B2, B3> _typle_fn_zip
    for ((A0, A1, A2, A3), (B0, B1, B2, B3)) {
        type Return = ((A0, B0), (A1, B1), (A2, B2), (A3, B3));
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                )
            }
        }
    }
    impl<A0, A1, A2, A3, A4, B0, B1, B2, B3, B4> _typle_fn_zip
    for ((A0, A1, A2, A3, A4), (B0, B1, B2, B3, B4)) {
        type Return = ((A0, B0), (A1, B1), (A2, B2), (A3, B3), (A4, B4));
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                )
            }
        }
    }
    impl<A0, A1, A2, A3, A4, A5, B0, B1, B2, B3, B4, B5> _typle_fn_zip
    for ((A0, A1, A2, A3, A4, A5), (B0, B1, B2, B3, B4, B5)) {
        type Return = ((A0, B0), (A1, B1), (A2, B2), (A3, B3), (A4, B4), (A5, B5));
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                )
            }
        }
    }
    impl<A0, A1, A2, A3, A4, A5, A6, B0, B1, B2, B3, B4, B5, B6> _typle_fn_zip
    for ((A0, A1, A2, A3, A4, A5, A6), (B0, B1, B2, B3, B4, B5, B6)) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                )
            }
        }
    }
    impl<A0, A1, A2, A3, A4, A5, A6, A7, B0, B1, B2, B3, B4, B5, B6, B7> _typle_fn_zip
    for ((A0, A1, A2, A3, A4, A5, A6, A7), (B0, B1, B2, B3, B4, B5, B6, B7)) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
            (A7, B7),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                    (first.7, second.7),
                )
            }
        }
    }
    impl<
        A0,
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        B0,
        B1,
        B2,
        B3,
        B4,
        B5,
        B6,
        B7,
        B8,
    > _typle_fn_zip
    for ((A0, A1, A2, A3, A4, A5, A6, A7, A8), (B0, B1, B2, B3, B4, B5, B6, B7, B8)) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
            (A7, B7),
            (A8, B8),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                    (first.7, second.7),
                    (first.8, second.8),
                )
            }
        }
    }
    impl<
        A0,
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        B0,
        B1,
        B2,
        B3,
        B4,
        B5,
        B6,
        B7,
        B8,
        B9,
    > _typle_fn_zip
    for (
        (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9),
        (B0, B1, B2, B3, B4, B5, B6, B7, B8, B9),
    ) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
            (A7, B7),
            (A8, B8),
            (A9, B9),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                    (first.7, second.7),
                    (first.8, second.8),
                    (first.9, second.9),
                )
            }
        }
    }
    impl<
        A0,
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        B0,
        B1,
        B2,
        B3,
        B4,
        B5,
        B6,
        B7,
        B8,
        B9,
        B10,
    > _typle_fn_zip
    for (
        (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
        (B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10),
    ) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
            (A7, B7),
            (A8, B8),
            (A9, B9),
            (A10, B10),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                    (first.7, second.7),
                    (first.8, second.8),
                    (first.9, second.9),
                    (first.10, second.10),
                )
            }
        }
    }
    impl<
        A0,
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        B0,
        B1,
        B2,
        B3,
        B4,
        B5,
        B6,
        B7,
        B8,
        B9,
        B10,
        B11,
    > _typle_fn_zip
    for (
        (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
        (B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11),
    ) {
        type Return = (
            (A0, B0),
            (A1, B1),
            (A2, B2),
            (A3, B3),
            (A4, B4),
            (A5, B5),
            (A6, B6),
            (A7, B7),
            (A8, B8),
            (A9, B9),
            (A10, B10),
            (A11, B11),
        );
        #[rustfmt::skip]
        fn apply(self) -> Self::Return {
            let (first, second) = self;
            {
                (
                    (first.0, second.0),
                    (first.1, second.1),
                    (first.2, second.2),
                    (first.3, second.3),
                    (first.4, second.4),
                    (first.5, second.5),
                    (first.6, second.6),
                    (first.7, second.7),
                    (first.8, second.8),
                    (first.9, second.9),
                    (first.10, second.10),
                    (first.11, second.11),
                )
            }
        }
    }
    pub fn zip<A, B>(first: A, second: B) -> <(A, B) as _typle_fn_zip>::Return
    where
        (A, B): _typle_fn_zip,
    {
        <(A, B) as _typle_fn_zip>::apply((first, second))
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_double {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_double for ((),) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t,) = self;
            { () }
        }
    }
    impl _typle_fn_double for ((u32,),) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0 * 2,) }
        }
    }
    impl _typle_fn_double for ((u32, u32),) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0 * 2, t.1 * 2) }
        }
    }
    impl _typle_fn_double for ((u32, u32, u32),) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0 * 2, t.1 * 2, t.2 * 2) }
        }
    }
    pub fn double<T>(t: T) -> <(T,) as _typle_fn_double>::Return
    where
        (T,): _typle_fn_double,
    {
        <(T,) as _typle_fn_double>::apply((t,))
    }
}
pub mod get {
    #![allow(dead_code)]
    use typle::typle;
    pub struct MyStruct<T> {
        t: T,
    }
    impl<T0> MyStruct<(T0,)>
    where
        T0: Default + ToString,
    {
        fn select(&mut self) {
            let i = 1;
            #[allow(clippy::single_match)]
            match i {
                0 => {
                    self.t.0 = <T0 as Default>::default();
                }
                _ => {}
            }
            #[allow(clippy::match_single_binding)]
            match i {
                _ => String::new(),
            };
            match i * 2 {
                0 => self.t.0.to_string(),
                _ => String::new(),
            };
        }
    }
    impl<T0, T1> MyStruct<(T0, T1)>
    where
        T0: Default + ToString,
        T1: Default + ToString,
    {
        fn select(&mut self) {
            let i = 1;
            match i {
                0 => {
                    self.t.0 = <T0 as Default>::default();
                }
                1 => {
                    self.t.1 = <T1 as Default>::default();
                }
                _ => {}
            }
            match i {
                0 => self.t.0.to_string(),
                _ => String::new(),
            };
            match i * 2 {
                0 => self.t.0.to_string(),
                1 => self.t.1.to_string(),
                _ => String::new(),
            };
        }
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_get_component {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C,), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    _ => None,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C, C), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    1 => Some(&t.1),
                    _ => None,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C, C, C), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    1 => Some(&t.1),
                    2 => Some(&t.2),
                    _ => None,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C, C, C, C), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    1 => Some(&t.1),
                    2 => Some(&t.2),
                    3 => Some(&t.3),
                    _ => None,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C, C, C, C, C), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    1 => Some(&t.1),
                    2 => Some(&t.2),
                    3 => Some(&t.3),
                    4 => Some(&t.4),
                    _ => None,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component for (&'t (C, C, C, C, C, C), usize) {
        type Return = Option<&'t C>;
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            {
                match i {
                    0 => Some(&t.0),
                    1 => Some(&t.1),
                    2 => Some(&t.2),
                    3 => Some(&t.3),
                    4 => Some(&t.4),
                    5 => Some(&t.5),
                    _ => None,
                }
            }
        }
    }
    fn get_component<'t, T>(
        t: &'t T,
        i: usize,
    ) -> <(&'t T, usize) as _typle_fn_get_component>::Return
    where
        (&'t T, usize): _typle_fn_get_component,
    {
        <(&'t T, usize) as _typle_fn_get_component>::apply((t, i))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_get_component_or_default {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'t, C> _typle_fn_get_component_or_default for (&'t (C,), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    _ => default,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component_or_default for (&'t (C, C), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    1 => &t.1,
                    _ => default,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component_or_default for (&'t (C, C, C), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    1 => &t.1,
                    2 => &t.2,
                    _ => default,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component_or_default for (&'t (C, C, C, C), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    1 => &t.1,
                    2 => &t.2,
                    3 => &t.3,
                    _ => default,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component_or_default
    for (&'t (C, C, C, C, C), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    1 => &t.1,
                    2 => &t.2,
                    3 => &t.3,
                    4 => &t.4,
                    _ => default,
                }
            }
        }
    }
    impl<'t, C> _typle_fn_get_component_or_default
    for (&'t (C, C, C, C, C, C), usize, &'t C) {
        type Return = &'t C;
        fn apply(self) -> Self::Return {
            let (t, i, default) = self;
            {
                match i {
                    0 => &t.0,
                    1 => &t.1,
                    2 => &t.2,
                    3 => &t.3,
                    4 => &t.4,
                    5 => &t.5,
                    _ => default,
                }
            }
        }
    }
    fn get_component_or_default<'t, T, C>(
        t: &'t T,
        i: usize,
        default: &'t C,
    ) -> <(&'t T, usize, &'t C) as _typle_fn_get_component_or_default>::Return
    where
        (&'t T, usize, &'t C): _typle_fn_get_component_or_default,
    {
        <(
            &'t T,
            usize,
            &'t C,
        ) as _typle_fn_get_component_or_default>::apply((t, i, default))
    }
}
pub mod issue1 {
    #![allow(unused)]
    use typle::typle;
    pub trait Verifier {
        type Error;
        fn verify(&self, report: &()) -> Result<(), Self::Error>;
    }
    impl<T0> Verifier for (T0,)
    where
        T0: Verifier,
        <T0 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
    {
        type Error = Box<dyn std::error::Error>;
        fn verify(&self, _report: &()) -> Result<(), Self::Error> {
            Ok(())
        }
    }
    impl<T0, T1> Verifier for (T0, T1)
    where
        T0: Verifier,
        T1: Verifier,
        <T0 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
        <T1 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
    {
        type Error = Box<dyn std::error::Error>;
        fn verify(&self, _report: &()) -> Result<(), Self::Error> {
            Ok(())
        }
    }
    impl<T0, T1, T2> Verifier for (T0, T1, T2)
    where
        T0: Verifier,
        T1: Verifier,
        T2: Verifier,
        <T0 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
        <T1 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
        <T2 as Verifier>::Error: Into<Box<dyn std::error::Error>>,
    {
        type Error = Box<dyn std::error::Error>;
        fn verify(&self, _report: &()) -> Result<(), Self::Error> {
            Ok(())
        }
    }
}
pub mod macros {
    #![allow(dead_code)]
    use typle::typle;
    struct MyStruct<T> {
        t: T,
    }
    impl<T0, T1, T2> MyStruct<(T0, T1, T2)> {
        fn call_macro(_t: (T0, T1, T2)) {
            "T";
            "T :: LEN";
            "(T0, T1, T2)";
            "3";
        }
    }
}
pub mod method {
    use typle::typle;
    #[allow(unused)]
    pub struct X {
        i: u32,
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_associated {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_associated for ((), u32) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t, i) = self;
            { () }
        }
    }
    impl _typle_fn_associated for ((u32,), u32) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            { (t.0 + i,) }
        }
    }
    impl _typle_fn_associated for ((u32, u32), u32) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            { (t.0 + i, t.1 + i) }
        }
    }
    impl _typle_fn_associated for ((u32, u32, u32), u32) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, i) = self;
            { (t.0 + i, t.1 + i, t.2 + i) }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent1 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent1 for (&'a X, ()) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            { () }
        }
    }
    impl<'a> _typle_fn_inherent1 for (&'a X, (u32,)) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { (t.0 + _typle_self.i,) }
        }
    }
    impl<'a> _typle_fn_inherent1 for (&'a X, (u32, u32)) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { (t.0 + _typle_self.i, t.1 + _typle_self.i) }
        }
    }
    impl<'a> _typle_fn_inherent1 for (&'a X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { (t.0 + _typle_self.i, t.1 + _typle_self.i, t.2 + _typle_self.i) }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent2 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent2 for (&'a X, ()) {
        type Return = ();
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            { X::associated(t, _typle_self.i) }
        }
    }
    impl<'a> _typle_fn_inherent2 for (&'a X, (u32,)) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { X::associated(t, _typle_self.i) }
        }
    }
    impl<'a> _typle_fn_inherent2 for (&'a X, (u32, u32)) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { X::associated(t, _typle_self.i) }
        }
    }
    impl<'a> _typle_fn_inherent2 for (&'a X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            { X::associated(t, _typle_self.i) }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent3 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent3 for (&'a mut X, ()) {
        type Return = ();
        #[allow(clippy::assign_op_pattern)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            {
                _typle_self.i += 1;
                _typle_self.i = _typle_self.i + 1;
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent3 for (&'a mut X, (u32,)) {
        type Return = (u32,);
        #[allow(clippy::assign_op_pattern)]
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                _typle_self.i += 1;
                _typle_self.i = _typle_self.i + 1;
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent3 for (&'a mut X, (u32, u32)) {
        type Return = (u32, u32);
        #[allow(clippy::assign_op_pattern)]
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                _typle_self.i += 1;
                _typle_self.i = _typle_self.i + 1;
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent3 for (&'a mut X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        #[allow(clippy::assign_op_pattern)]
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                _typle_self.i += 1;
                _typle_self.i = _typle_self.i + 1;
                t
            }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent4 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent4 for (&'a X, ()) {
        type Return = ();
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            {
                let _v: Vec<X> = Vec::<X>::new();
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent4 for (&'a X, (u32,)) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let _v: Vec<X> = Vec::<X>::new();
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent4 for (&'a X, (u32, u32)) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let _v: Vec<X> = Vec::<X>::new();
                t
            }
        }
    }
    impl<'a> _typle_fn_inherent4 for (&'a X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let _v: Vec<X> = Vec::<X>::new();
                t
            }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent5 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent5 for (&'a X, ()) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            {
                #[allow(unused_variables)]
                let X { i } = _typle_self;
                ()
            }
        }
    }
    impl<'a> _typle_fn_inherent5 for (&'a X, (u32,)) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i,)
            }
        }
    }
    impl<'a> _typle_fn_inherent5 for (&'a X, (u32, u32)) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i, t.1 + i)
            }
        }
    }
    impl<'a> _typle_fn_inherent5 for (&'a X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i, t.1 + i, t.2 + i)
            }
        }
    }
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_inherent6 {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<'a> _typle_fn_inherent6 for (&'a X, ()) {
        type Return = ();
        #[allow(unused_assignments)]
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (_typle_self, t) = self;
            {
                #[allow(unused_variables)]
                let X { i } = _typle_self;
                ()
            }
        }
    }
    impl<'a> _typle_fn_inherent6 for (&'a X, (u32,)) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i,)
            }
        }
    }
    impl<'a> _typle_fn_inherent6 for (&'a X, (u32, u32)) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i, t.1 + i)
            }
        }
    }
    impl<'a> _typle_fn_inherent6 for (&'a X, (u32, u32, u32)) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (_typle_self, t) = self;
            {
                let X { i } = _typle_self;
                (t.0 + i, t.1 + i, t.2 + i)
            }
        }
    }
    impl X {
        #[allow(unused)]
        pub fn new(i: u32) -> Self {
            Self { i }
        }
        pub fn associated<T>(t: T, i: u32) -> <(T, u32) as _typle_fn_associated>::Return
        where
            (T, u32): _typle_fn_associated,
        {
            <(T, u32) as _typle_fn_associated>::apply((t, i))
        }
        pub fn inherent1<'a, T>(
            &'a self,
            t: T,
        ) -> <(&'a X, T) as _typle_fn_inherent1>::Return
        where
            (&'a X, T): _typle_fn_inherent1,
        {
            <(&'a X, T) as _typle_fn_inherent1>::apply((self, t))
        }
        pub fn inherent2<'a, T>(
            &'a self,
            t: T,
        ) -> <(&'a X, T) as _typle_fn_inherent2>::Return
        where
            (&'a X, T): _typle_fn_inherent2,
        {
            <(&'a X, T) as _typle_fn_inherent2>::apply((self, t))
        }
        pub fn inherent3<'a, T>(
            &'a mut self,
            t: T,
        ) -> <(&'a mut X, T) as _typle_fn_inherent3>::Return
        where
            (&'a mut X, T): _typle_fn_inherent3,
        {
            <(&'a mut X, T) as _typle_fn_inherent3>::apply((self, t))
        }
        pub fn inherent4<'a, T>(
            &'a self,
            t: T,
        ) -> <(&'a X, T) as _typle_fn_inherent4>::Return
        where
            (&'a X, T): _typle_fn_inherent4,
        {
            <(&'a X, T) as _typle_fn_inherent4>::apply((self, t))
        }
        pub fn inherent5<'a, T>(
            &'a self,
            t: T,
        ) -> <(&'a X, T) as _typle_fn_inherent5>::Return
        where
            (&'a X, T): _typle_fn_inherent5,
        {
            <(&'a X, T) as _typle_fn_inherent5>::apply((self, t))
        }
        pub fn inherent6<'a, T>(
            &'a self,
            t: T,
        ) -> <(&'a X, T) as _typle_fn_inherent6>::Return
        where
            (&'a X, T): _typle_fn_inherent6,
        {
            <(&'a X, T) as _typle_fn_inherent6>::apply((self, t))
        }
    }
}
pub mod pattern {
    #![allow(dead_code)]
    use typle::typle;
    pub struct MyStruct<T> {
        t: T,
    }
    impl MyStruct<(u32, u32, u32)> {
        pub fn test_macro(&self) -> u32 {
            let (x0, x1): (u32, u32) = (self.t.0 * 3, self.t.1 * 3);
            x0 + x1
        }
        pub fn component(&self) -> u32 {
            let x1: u32 = self.t.1;
            x1
        }
        pub fn test_slice(&self) -> u32 {
            let [x0, x1] = [self.t.0 * 3, self.t.1 * 3];
            x0 + x1
        }
        pub fn test_tuple(&self) -> u32 {
            let (x0, x1) = (self.t.0 * 3, self.t.1 * 3);
            x0 + x1
        }
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_multiply_by {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_multiply_by for ((u32,), u32) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0,) = (t.0 * m,);
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0,)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32), u32) {
        type Return = (u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1) = (t.0 * m, t.1 * m);
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32), u32) {
        type Return = (u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2) = (t.0 * m, t.1 * m, t.2 * m);
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3) = (t.0 * m, t.1 * m, t.2 * m, t.3 * m);
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4) = (t.0 * m, t.1 * m, t.2 * m, t.3 * m, t.4 * m);
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6, x7) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                    t.7 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6, x7)
            }
        }
    }
    impl _typle_fn_multiply_by for ((u32, u32, u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6, x7, x8) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                    t.7 * m,
                    t.8 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6, x7, x8)
            }
        }
    }
    impl _typle_fn_multiply_by
    for ((u32, u32, u32, u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                    t.7 * m,
                    t.8 * m,
                    t.9 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
            }
        }
    }
    impl _typle_fn_multiply_by
    for ((u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                    t.7 * m,
                    t.8 * m,
                    t.9 * m,
                    t.10 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
            }
        }
    }
    impl _typle_fn_multiply_by
    for ((u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32), u32) {
        type Return = (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32);
        fn apply(self) -> Self::Return {
            let (t, m) = self;
            {
                let (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = (
                    t.0 * m,
                    t.1 * m,
                    t.2 * m,
                    t.3 * m,
                    t.4 * m,
                    t.5 * m,
                    t.6 * m,
                    t.7 * m,
                    t.8 * m,
                    t.9 * m,
                    t.10 * m,
                    t.11 * m,
                );
                match (&x0, &(t.0 * m)) {
                    (left_val, right_val) => {
                        if !(*left_val == *right_val) {
                            let kind = ::core::panicking::AssertKind::Eq;
                            ::core::panicking::assert_failed(
                                kind,
                                &*left_val,
                                &*right_val,
                                ::core::option::Option::None,
                            );
                        }
                    }
                };
                (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
            }
        }
    }
    fn multiply_by<T>(t: T, m: u32) -> <(T, u32) as _typle_fn_multiply_by>::Return
    where
        (T, u32): _typle_fn_multiply_by,
    {
        <(T, u32) as _typle_fn_multiply_by>::apply((t, m))
    }
}
pub mod type_alias {
    #![allow(type_alias_bounds, unused)]
    use typle::typle;
    trait Process {
        type State;
        type Output;
        fn process(
            state: Self::State,
        ) -> Result<Self::Output, Box<dyn std::error::Error>>;
    }
    type Alias<T0, T1, T2>
    where
        T0: Process,
        T1: Process,
        T2: Process,
    = (Option<<T0>::Output>, Option<<T1>::Output>, Option<<T2>::Output>);
}
pub mod typle_args {
    #![allow(unused)]
    use typle::typle;
    #[allow(non_camel_case_types)]
    trait _typle_fn_append {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl<A> _typle_fn_append for ((), A) {
        type Return = (A,);
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t, a) = self;
            { (a,) }
        }
    }
    impl<T0, A> _typle_fn_append for ((T0,), A) {
        type Return = (T0, A);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, a) }
        }
    }
    impl<T0, T1, A> _typle_fn_append for ((T0, T1), A) {
        type Return = (T0, T1, A);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, t.1, a) }
        }
    }
    impl<T0, T1, T2, A> _typle_fn_append for ((T0, T1, T2), A) {
        type Return = (T0, T1, T2, A);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, t.1, t.2, a) }
        }
    }
    fn append<T, A>(t: T, a: A) -> <(T, A) as _typle_fn_append>::Return
    where
        (T, A): _typle_fn_append,
    {
        <(T, A) as _typle_fn_append>::apply((t, a))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_append_array {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_append_array for ((bool,), bool) {
        type Return = [bool; 1 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [t.0, a] }
        }
    }
    impl _typle_fn_append_array for ((bool, bool), bool) {
        type Return = [bool; 2 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [t.0, t.1, a] }
        }
    }
    impl _typle_fn_append_array for ((bool, bool, bool), bool) {
        type Return = [bool; 3 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [t.0, t.1, t.2, a] }
        }
    }
    fn append_array<T>(t: T, a: bool) -> <(T, bool) as _typle_fn_append_array>::Return
    where
        (T, bool): _typle_fn_append_array,
    {
        <(T, bool) as _typle_fn_append_array>::apply((t, a))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_append_double {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_append_double for ((u32,), u32) {
        type Return = [u32; 1 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [2 * t.0, 2 * a] }
        }
    }
    impl _typle_fn_append_double for ((u32, u32), u32) {
        type Return = [u32; 2 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [2 * t.0, 2 * t.1, 2 * a] }
        }
    }
    impl _typle_fn_append_double for ((u32, u32, u32), u32) {
        type Return = [u32; 3 + 1];
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { [2 * t.0, 2 * t.1, 2 * t.2, 2 * a] }
        }
    }
    fn append_double<T>(t: T, a: u32) -> <(T, u32) as _typle_fn_append_double>::Return
    where
        (T, u32): _typle_fn_append_double,
    {
        <(T, u32) as _typle_fn_append_double>::apply((t, a))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_append_even {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_append_even for ((), u32) {
        type Return = (u32,);
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t, a) = self;
            { (a,) }
        }
    }
    impl<T0> _typle_fn_append_even for ((T0,), u32) {
        type Return = (T0, u32);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, a) }
        }
    }
    impl<T0, T1> _typle_fn_append_even for ((T0, T1), u32) {
        type Return = (T0, u32);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, a) }
        }
    }
    impl<T0, T1, T2> _typle_fn_append_even for ((T0, T1, T2), u32) {
        type Return = (T0, T2, u32);
        fn apply(self) -> Self::Return {
            let (t, a) = self;
            { (t.0, t.2, a) }
        }
    }
    fn append_even<T>(t: T, a: u32) -> <(T, u32) as _typle_fn_append_even>::Return
    where
        (T, u32): _typle_fn_append_even,
    {
        <(T, u32) as _typle_fn_append_even>::apply((t, a))
    }
    #[allow(non_camel_case_types)]
    trait _typle_fn_even_string_odd {
        type Return;
        fn apply(self) -> Self::Return;
    }
    impl _typle_fn_even_string_odd for ((),) {
        type Return = ();
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t,) = self;
            { #[allow(clippy::unused_unit)] () }
        }
    }
    impl<T0> _typle_fn_even_string_odd for ((T0,),)
    where
        T0: ToString,
    {
        type Return = (String,);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0.to_string(),) }
        }
    }
    impl<T0, T1> _typle_fn_even_string_odd for ((T0, T1),)
    where
        T0: ToString,
    {
        type Return = (String, T1);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0.to_string(), t.1) }
        }
    }
    impl<T0, T1, T2> _typle_fn_even_string_odd for ((T0, T1, T2),)
    where
        T0: ToString,
        T2: ToString,
    {
        type Return = (String, T1, String);
        fn apply(self) -> Self::Return {
            let (t,) = self;
            { (t.0.to_string(), t.1, t.2.to_string()) }
        }
    }
    fn even_string_odd<T>(t: T) -> <(T,) as _typle_fn_even_string_odd>::Return
    where
        (T,): _typle_fn_even_string_odd,
    {
        <(T,) as _typle_fn_even_string_odd>::apply((t,))
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
    impl<Out, Func: Send + Sync + 'static> ExclusiveSystemParamFunction<fn() -> Out>
    for Func
    where
        for<'a> &'a mut Func: FnMut(&mut World) -> Out + FnMut(&mut World) -> Out,
        Out: 'static,
    {
        type In = ();
        type Out = Out;
        type Param = ();
        #[inline]
        fn run(
            &mut self,
            world: &mut World,
            _in: (),
            param_value: ExclusiveSystemParamItem<()>,
        ) -> Self::Out {
            ::core::panicking::panic("not yet implemented")
        }
    }
    impl<
        Out,
        Func: Send + Sync + 'static,
        F0,
    > ExclusiveSystemParamFunction<fn(F0) -> Out> for Func
    where
        for<'a> &'a mut Func: FnMut(&mut World, F0) -> Out
            + FnMut(&mut World, ExclusiveSystemParamItem<F0>) -> Out,
        Out: 'static,
        F0: ExclusiveSystemParam,
    {
        type In = ();
        type Out = Out;
        type Param = (F0,);
        #[inline]
        fn run(
            &mut self,
            world: &mut World,
            _in: (),
            param_value: ExclusiveSystemParamItem<(F0,)>,
        ) -> Self::Out {
            ::core::panicking::panic("not yet implemented")
        }
    }
    impl<
        Out,
        Func: Send + Sync + 'static,
        F0,
        F1,
    > ExclusiveSystemParamFunction<fn(F0, F1) -> Out> for Func
    where
        for<'a> &'a mut Func: FnMut(&mut World, F0, F1) -> Out
            + FnMut(
                &mut World,
                ExclusiveSystemParamItem<F0>,
                ExclusiveSystemParamItem<F1>,
            ) -> Out,
        Out: 'static,
        F0: ExclusiveSystemParam,
        F1: ExclusiveSystemParam,
    {
        type In = ();
        type Out = Out;
        type Param = (F0, F1);
        #[inline]
        fn run(
            &mut self,
            world: &mut World,
            _in: (),
            param_value: ExclusiveSystemParamItem<(F0, F1)>,
        ) -> Self::Out {
            ::core::panicking::panic("not yet implemented")
        }
    }
    impl<
        Out,
        Func: Send + Sync + 'static,
        F0,
        F1,
        F2,
    > ExclusiveSystemParamFunction<fn(F0, F1, F2) -> Out> for Func
    where
        for<'a> &'a mut Func: FnMut(&mut World, F0, F1, F2) -> Out
            + FnMut(
                &mut World,
                ExclusiveSystemParamItem<F0>,
                ExclusiveSystemParamItem<F1>,
                ExclusiveSystemParamItem<F2>,
            ) -> Out,
        Out: 'static,
        F0: ExclusiveSystemParam,
        F1: ExclusiveSystemParam,
        F2: ExclusiveSystemParam,
    {
        type In = ();
        type Out = Out;
        type Param = (F0, F1, F2);
        #[inline]
        fn run(
            &mut self,
            world: &mut World,
            _in: (),
            param_value: ExclusiveSystemParamItem<(F0, F1, F2)>,
        ) -> Self::Out {
            ::core::panicking::panic("not yet implemented")
        }
    }
    trait HandleStuff {
        type Input;
        type Output;
        fn handle_stuff(&self, input: Self::Input) -> Self::Output;
    }
    struct MultipleHandlers<T> {
        handlers: T,
    }
    impl<T0, I> HandleStuff for MultipleHandlers<(T0,)>
    where
        T0: HandleStuff<Input = I>,
    {
        type Input = I;
        type Output = (<T0>::Output,);
        fn handle_stuff(&self, input: Self::Input) -> Self::Output {
            (self.handlers.0.handle_stuff(input),)
        }
    }
    impl<T0, T1, I> HandleStuff for MultipleHandlers<(T0, T1)>
    where
        T0: HandleStuff<Input = I>,
        T1: HandleStuff<Input = I>,
        I: Clone,
    {
        type Input = I;
        type Output = (<T0>::Output, <T1>::Output);
        fn handle_stuff(&self, input: Self::Input) -> Self::Output {
            (
                self.handlers.0.handle_stuff(input.clone()),
                self.handlers.1.handle_stuff(input),
            )
        }
    }
    impl<T0, T1, T2, I> HandleStuff for MultipleHandlers<(T0, T1, T2)>
    where
        T0: HandleStuff<Input = I>,
        T1: HandleStuff<Input = I>,
        T2: HandleStuff<Input = I>,
        I: Clone,
    {
        type Input = I;
        type Output = (<T0>::Output, <T1>::Output, <T2>::Output);
        fn handle_stuff(&self, input: Self::Input) -> Self::Output {
            (
                self.handlers.0.handle_stuff(input.clone()),
                self.handlers.1.handle_stuff(input.clone()),
                self.handlers.2.handle_stuff(input),
            )
        }
    }
}
pub mod typle_fold {
    #![allow(dead_code)]
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
    impl<T0> UsefulTrait for (T0,)
    where
        T0: UsefulTrait + std::fmt::Display,
    {
        type UsefulType = <T0>::UsefulType;
        const SIZE: usize = (loop {
            let total = 0;
            let total = total + <T0>::SIZE;
            break total;
        });
        fn display(&self) -> String {
            ("[".to_string() + &self.0.to_string()) + "]"
        }
    }
    impl<T0, T1> UsefulTrait for (T0, T1)
    where
        T0: UsefulTrait + std::fmt::Display,
        T1: UsefulTrait + std::fmt::Display,
        <T1>::UsefulType: IsUseful<<T0>::UsefulType>,
    {
        type UsefulType = <<T1>::UsefulType as IsUseful<<T0>::UsefulType>>::State;
        const SIZE: usize = (loop {
            let total = 0;
            let total = total + <T0>::SIZE;
            let total = total + <T1>::SIZE;
            break total;
        });
        fn display(&self) -> String {
            (loop {
                let s = "[".to_string() + &self.0.to_string();
                let s = s + "," + &self.1.to_string();
                break s;
            }) + "]"
        }
    }
    impl<T0, T1, T2> UsefulTrait for (T0, T1, T2)
    where
        T0: UsefulTrait + std::fmt::Display,
        T1: UsefulTrait + std::fmt::Display,
        T2: UsefulTrait + std::fmt::Display,
        <T1>::UsefulType: IsUseful<<T0>::UsefulType>,
        <T2>::UsefulType: IsUseful<
            <<T1>::UsefulType as IsUseful<<T0>::UsefulType>>::State,
        >,
    {
        type UsefulType = <<T2>::UsefulType as IsUseful<
            <<T1>::UsefulType as IsUseful<<T0>::UsefulType>>::State,
        >>::State;
        const SIZE: usize = (loop {
            let total = 0;
            let total = total + <T0>::SIZE;
            let total = total + <T1>::SIZE;
            let total = total + <T2>::SIZE;
            break total;
        });
        fn display(&self) -> String {
            (loop {
                let s = "[".to_string() + &self.0.to_string();
                let s = s + "," + &self.1.to_string();
                let s = s + "," + &self.2.to_string();
                break s;
            }) + "]"
        }
    }
    trait CoalesceSome<T> {
        type Output;
        /// Coalesce a tuple of Options into an Option of tuple that is `Some`
        /// only if all the components are `Some`.
        fn coalesce_some(self) -> Option<Self::Output>;
    }
    impl CoalesceSome<()> for () {
        type Output = ();
        fn coalesce_some(self) -> Option<Self::Output> {
            #[allow(unused_variables)]
            (loop {
                let acc = ();
                let acc = { Some(acc) };
                break acc;
            })
        }
    }
    impl<T0> CoalesceSome<(T0,)> for (Option<T0>,) {
        type Output = (T0,);
        fn coalesce_some(self) -> Option<Self::Output> {
            #[allow(unused_variables)]
            (loop {
                let acc = ();
                let acc = if let Some(curr) = self.0 {
                    (curr,)
                } else {
                    break None;
                };
                let acc = { Some(acc) };
                break acc;
            })
        }
    }
    impl<T0, T1> CoalesceSome<(T0, T1)> for (Option<T0>, Option<T1>) {
        type Output = (T0, T1);
        fn coalesce_some(self) -> Option<Self::Output> {
            #[allow(unused_variables)]
            (loop {
                let acc = ();
                let acc = if let Some(curr) = self.0 {
                    (curr,)
                } else {
                    break None;
                };
                let acc = if let Some(curr) = self.1 {
                    (acc.0, curr)
                } else {
                    break None;
                };
                let acc = { Some(acc) };
                break acc;
            })
        }
    }
}
pub mod unzip {
    use typle::typle;
    pub trait TryUnzip {
        type Output;
        type Error;
        fn try_unzip(self) -> Result<Self::Output, Self::Error>;
    }
    pub trait TryUnzipModified<Output> {
        type Error;
        fn try_unzip(self) -> Result<Output, Self::Error>;
    }
    impl<I, T0, T1, E> TryUnzipModified<(Vec<T0>, Vec<T1>)> for I
    where
        I: Iterator<Item = Result<(T0, T1), E>>,
    {
        type Error = E;
        fn try_unzip(self) -> Result<(Vec<T0>, Vec<T1>), Self::Error> {
            let mut vecs = (Vec::new(), Vec::new());
            for result in self {
                let t = result?;
                loop {
                    {
                        vecs.0.push(t.0);
                    }
                    {
                        vecs.1.push(t.1);
                    }
                    break;
                }
            }
            Ok(vecs)
        }
    }
    impl<I, T0, T1, T2, E> TryUnzipModified<(Vec<T0>, Vec<T1>, Vec<T2>)> for I
    where
        I: Iterator<Item = Result<(T0, T1, T2), E>>,
    {
        type Error = E;
        fn try_unzip(self) -> Result<(Vec<T0>, Vec<T1>, Vec<T2>), Self::Error> {
            let mut vecs = (Vec::new(), Vec::new(), Vec::new());
            for result in self {
                let t = result?;
                loop {
                    {
                        vecs.0.push(t.0);
                    }
                    {
                        vecs.1.push(t.1);
                    }
                    {
                        vecs.2.push(t.2);
                    }
                    break;
                }
            }
            Ok(vecs)
        }
    }
    pub trait TryUnzipTuple<T, E> {
        type Output;
        fn try_unzip<I>(iter: I) -> Result<Self::Output, E>
        where
            I: Iterator<Item = Result<T, E>>;
    }
    impl<T0, T1, E> TryUnzipTuple<(T0, T1), E> for (T0, T1) {
        type Output = (Vec<T0>, Vec<T1>);
        fn try_unzip<I>(iter: I) -> Result<Self::Output, E>
        where
            I: Iterator<Item = Result<(T0, T1), E>>,
        {
            let mut vecs = (Vec::new(), Vec::new());
            for result in iter {
                let t = result?;
                loop {
                    {
                        vecs.0.push(t.0);
                    }
                    {
                        vecs.1.push(t.1);
                    }
                    break;
                }
            }
            Ok(vecs)
        }
    }
    impl<T0, T1, T2, E> TryUnzipTuple<(T0, T1, T2), E> for (T0, T1, T2) {
        type Output = (Vec<T0>, Vec<T1>, Vec<T2>);
        fn try_unzip<I>(iter: I) -> Result<Self::Output, E>
        where
            I: Iterator<Item = Result<(T0, T1, T2), E>>,
        {
            let mut vecs = (Vec::new(), Vec::new(), Vec::new());
            for result in iter {
                let t = result?;
                loop {
                    {
                        vecs.0.push(t.0);
                    }
                    {
                        vecs.1.push(t.1);
                    }
                    {
                        vecs.2.push(t.2);
                    }
                    break;
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
}
