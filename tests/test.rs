use crate::compile::function::zip;

mod compile;

#[test]
fn test_expand() {
    macrotest::expand("tests/expand/*.rs");
    macrotest::expand("tests/compile/mod.rs");
}

#[test]
fn test_zip() {
    let s = ("LHR", "FCO", "ZRH");
    let t = (51.5, 41.8, 47.5);
    assert_eq!(zip(s, t), (("LHR", 51.5), ("FCO", 41.8), ("ZRH", 47.5)));
    let s = ('a', 'b', 'c');
    let t = (1, 2, 3);
    assert_eq!(zip(s, t), (('a', 1), ('b', 2), ('c', 3)));
    let s = (2.0, "test".to_string());
    let t = (9u8, ());
    assert_eq!(zip(s, t), ((2.0, 9u8), ("test".to_string(), ())));
}
