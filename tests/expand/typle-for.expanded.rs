use typle::typle;
struct S<T> {
    t: T,
}
impl S<()> {
    fn new(t: ()) {
        let a = [];
        let b = ();
        let init: [Option<u32>; 0] = [];
    }
}
impl S<(u32,)> {
    fn new(t: (&u32,)) {
        let a = [*t.0 * 2];
        let b = (*t.0 * 2,);
        let init: [Option<u32>; 1] = [None];
    }
}
impl S<(u32, u32)> {
    fn new(t: (&u32, &u32)) {
        let a = [*t.0 * 2, *t.1 * 2];
        let b = (*t.0 * 2, *t.1 * 2);
        let init: [Option<u32>; 2] = [None, None];
    }
}
