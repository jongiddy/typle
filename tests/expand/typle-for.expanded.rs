use typle::typle;
struct S0 {
    t: (),
}
struct S1 {
    t: (Option<u32>,),
}
struct S2 {
    t: (Option<u32>, Option<u32>),
}
impl<'a> S0 {
    fn new(t: ()) {
        let a = [];
        let b = ();
        let init: [Option<u32>; 0] = [];
    }
}
impl<'a> S1<u32> {
    fn new(t: (&u32,)) {
        let a = [*t.0 * 2];
        let b = (*t.0 * 2,);
        let init: [Option<u32>; 1] = [None];
    }
}
impl<'a> S2<u32, u32> {
    fn new(t: (&u32, &u32)) {
        let a = [*t.0 * 2, *t.1 * 2];
        let b = (*t.0 * 2, *t.1 * 2);
        let init: [Option<u32>; 2] = [None, None];
    }
}
