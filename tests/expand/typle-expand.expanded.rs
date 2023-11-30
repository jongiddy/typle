use typle::typle;
struct S0<'a> {
    t: (),
}
struct S1<'a> {
    t: (&'a u32,),
}
struct S2<'a> {
    t: (&'a u32, &'a u32),
}
impl<'a> S0<'a> {
    fn new(t: ()) {
        let a = [];
        let b = ();
    }
}
impl<'a> S1<'a, u32> {
    fn new(t: (&u32,)) {
        let a = [*t.0 * 2];
        let b = (*t.0 * 2,);
    }
}
impl<'a> S2<'a, u32, u32> {
    fn new(t: (&u32, &u32)) {
        let a = [*t.0 * 2, *t.1 * 2];
        let b = (*t.0 * 2, *t.1 * 2);
    }
}
