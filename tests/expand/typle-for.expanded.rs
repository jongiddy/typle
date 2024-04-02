use typle::typle;
struct S<T> {
    t: T,
}
impl S<()> {
    fn new(t: ()) {
        let a: [u32; 0] = [];
        let b = ();
        let init: [Option<u32>; 0] = [];
        let c = ();
        let d = ();
        let e = [];
    }
}
impl S<(u32,)> {
    fn new(t: (&u32,)) {
        let a: [u32; 1] = [*t.0 * 2];
        let b = (*t.0 * 2,);
        let init: [Option<u32>; 1] = [None];
        let c = ({ b.0 },);
        let d = ({ b.0 },);
        let e = [{ b.0 }];
    }
}
impl S<(u32, u32)> {
    fn new(t: (&u32, &u32)) {
        let a: [u32; 2] = [*t.0 * 2, *t.1 * 2];
        let b = (*t.0 * 2, *t.1 * 2);
        let init: [Option<u32>; 2] = [None, None];
        let c = ({ b.0 },);
        let d = ({ b.0 },);
        let e = [{ b.0 }];
    }
}
