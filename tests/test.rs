use crate::compile::readme::{zip, MyStruct};

mod compile;

#[test]
fn test_expand() {
    macrotest::expand("tests/expand/*.rs");
    macrotest::expand("tests/compile/mod.rs");
}

#[test]
fn test_tail() {
    let m = MyStruct::from(('a', 2, "test"));
    let tail = m.tail();
    assert_eq!(tail, (2, "test"));
    let m = MyStruct::from(tail);
    let tail = m.tail();
    assert_eq!(tail, ("test",));
    let m = MyStruct::from(tail);
    let tail = m.tail();
    assert_eq!(tail, ());
}

#[test]
fn test_zip() {
    let s = ('a', 'b', 'c');
    let t = (1, 2, 3);
    assert_eq!(zip(s, t), (('a', 1), ('b', 2), ('c', 3)));
    let s = (2.0, "test".to_string());
    let t = (9u8, ());
    assert_eq!(zip(s, t), ((2.0, 9u8), ("test".to_string(), ())));
}
