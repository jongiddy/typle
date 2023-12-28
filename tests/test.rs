use std::hash::Hasher as _;

use crate::compile::function::{hash, zip};

mod compile;

#[test]
fn test_expand() {
    macrotest::expand("tests/expand/*.rs");
    macrotest::expand("tests/compile/mod.rs");
}

#[test]
fn test_hash() {
    let t = (1, 2, 3);
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hash(&t, &mut hasher);
    assert_eq!(hasher.finish(), 646939227381880718);
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hash(&("one", "two"), &mut hasher);
    assert_eq!(hasher.finish(), 15995114744266341102);
}

#[test]
fn test_zip() {
    let s = ("LHR", "FCO", "ZRH");
    let t = (51.5, 41.8, 47.5);
    assert_eq!(zip(s, t), (("LHR", 51.5), ("FCO", 41.8), ("ZRH", 47.5)));
    assert_eq!(zip((), ()), ());
    let s = ('a', 'b', 'c');
    let t = (1, 2, 3);
    assert_eq!(zip(s, t), (('a', 1), ('b', 2), ('c', 3)));
    let s = (2.0, "test".to_string());
    let t = (9u8, ());
    assert_eq!(zip(s, t), ((2.0, 9u8), ("test".to_string(), ())));
}
