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
            loop {
                break;
            }
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
    mod tuple {
        pub trait Extract {
            type State;
            type Output;
            fn extract(&self, state: Option<Self::State>) -> Self::Output;
        }
        impl Extract for () {
            type State = ();
            type Output = ();
            fn extract(&self, _state: Option<Self::State>) -> Self::Output {
                ()
            }
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
                            let output = ({ matched },);
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
                            let output = ({ matched },);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = ({ output.0 }, { matched });
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
                            let output = ({ matched },);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = ({ output.0 }, { matched });
                            {
                                state = Self::State::S2(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S2(output, inner_state) = state {
                            let matched = self.tuple.2.extract(inner_state);
                            let output = ({ output.0 }, { output.1 }, { matched });
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
                            let output = ({ matched },);
                            {
                                state = Self::State::S1(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S1(output, inner_state) = state {
                            let matched = self.tuple.1.extract(inner_state);
                            let output = ({ output.0 }, { matched });
                            {
                                state = Self::State::S2(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S2(output, inner_state) = state {
                            let matched = self.tuple.2.extract(inner_state);
                            let output = ({ output.0 }, { output.1 }, { matched });
                            {
                                state = Self::State::S3(output, None);
                            }
                        }
                    }
                    {
                        if let Self::State::S3(output, inner_state) = state {
                            let matched = self.tuple.3.extract(inner_state);
                            let output = (
                                { output.0 },
                                { output.1 },
                                { output.2 },
                                { matched },
                            );
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
    pub fn do_continue<T>(t: T) -> <(T,) as _typle_fn_do_continue>::Return
    where
        (T,): _typle_fn_do_continue,
    {
        <(T,) as _typle_fn_do_continue>::apply((t,))
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_continue_labelled {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn do_continue_labelled<T>(
        t: T,
    ) -> <(T,) as _typle_fn_do_continue_labelled>::Return
    where
        (T,): _typle_fn_do_continue_labelled,
    {
        <(T,) as _typle_fn_do_continue_labelled>::apply((t,))
    }
    impl<T0, T1, T2, T3> _typle_fn_do_continue_labelled for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_break {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn do_break<T>(t: T) -> <(T,) as _typle_fn_do_break>::Return
    where
        (T,): _typle_fn_do_break,
    {
        <(T,) as _typle_fn_do_break>::apply((t,))
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_do_break_labelled {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn do_break_labelled<T>(t: T) -> <(T,) as _typle_fn_do_break_labelled>::Return
    where
        (T,): _typle_fn_do_break_labelled,
    {
        <(T,) as _typle_fn_do_break_labelled>::apply((t,))
    }
    impl<T0, T1, T2, T3> _typle_fn_do_break_labelled for ((T0, T1, T2, T3),) {
        type Return = Vec<usize>;
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_check_out_of_bounds {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn check_out_of_bounds<T>(
        t: T,
    ) -> <(T,) as _typle_fn_check_out_of_bounds>::Return
    where
        (T,): _typle_fn_check_out_of_bounds,
    {
        <(T,) as _typle_fn_check_out_of_bounds>::apply((t,))
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_check_negative_range {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn check_negative_range<T>(
        t: T,
    ) -> <(T,) as _typle_fn_check_negative_range>::Return
    where
        (T,): _typle_fn_check_negative_range,
    {
        <(T,) as _typle_fn_check_negative_range>::apply((t,))
    }
    impl _typle_fn_check_negative_range for ((),) {
        type Return = usize;
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (t,) = self;
            {
                let mut count = 0;
                loop {
                    break;
                }
                count
            }
        }
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
    pub fn hash<'a, T, S>(
        tuple: &'a T,
        state: &'a mut S,
    ) -> <(&'a T, &'a mut S) as _typle_fn_hash>::Return
    where
        (&'a T, &'a mut S): _typle_fn_hash,
    {
        <(&'a T, &'a mut S) as _typle_fn_hash>::apply((tuple, state))
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
    impl<M> _typle_fn_multiply for ((), M)
    where
        M: Copy,
    {
        type Return = ();
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
    impl _typle_fn_heapify for ((),) {
        type Return = ();
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_zip {
        type Return;
        fn apply(self) -> Self::Return;
    }
    #[rustfmt::skip]
    pub fn zip<A, B>(first: A, second: B) -> <(A, B) as _typle_fn_zip>::Return
    where
        (A, B): _typle_fn_zip,
    {
        <(A, B) as _typle_fn_zip>::apply((first, second))
    }
    impl _typle_fn_zip for ((), ()) {
        type Return = ();
        fn apply(self) -> Self::Return {
            #[allow(unused_variables)]
            let (first, second) = self;
            { () }
        }
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
    impl<A0, A1, A2, A3, B0, B1, B2, B3> _typle_fn_zip
    for ((A0, A1, A2, A3), (B0, B1, B2, B3)) {
        type Return = ((A0, B0), (A1, B1), (A2, B2), (A3, B3));
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
    #[allow(non_camel_case_types)]
    pub trait _typle_fn_double {
        type Return;
        fn apply(self) -> Self::Return;
    }
    pub fn double<T>(t: T) -> <(T,) as _typle_fn_double>::Return
    where
        (T,): _typle_fn_double,
    {
        <(T,) as _typle_fn_double>::apply((t,))
    }
    impl _typle_fn_double for ((),) {
        type Return = ();
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
    fn multiply_by<T>(t: T, m: u32) -> <(T, u32) as _typle_fn_multiply_by>::Return
    where
        (T, u32): _typle_fn_multiply_by,
    {
        <(T, u32) as _typle_fn_multiply_by>::apply((t, m))
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
