use std::hash::Hasher as _;

use crate::compile::for_loop::{
    check_negative_range, check_out_of_bounds, do_break, do_break_labelled, do_continue,
    do_continue_labelled,
};
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
    assert_eq!(
        zip(("LHR", "FCO", "ZRH"), (51.5, 41.8, 47.5)),
        (("LHR", 51.5), ("FCO", 41.8), ("ZRH", 47.5))
    );
    assert_eq!(
        zip((2.0, "test"), (Some(9u8), ('a', 'b'))),
        ((2.0, Some(9u8)), ("test", ('a', 'b')))
    );
    assert_eq!(zip((), ()), ());

    let s = ('a', 'b', 'c');
    let t = (1, 2, 3);
    assert_eq!(zip(s, t), (('a', 1), ('b', 2), ('c', 3)));

    let s = (2.0, "test".to_string());
    let t = (9u8, ());
    assert_eq!(zip(s, t), ((2.0, 9u8), ("test".to_string(), ())));
}

#[test]
fn test_continue() {
    let output = do_continue((1, 2, 3, 4));
    assert_eq!(output, vec![0, 1, 3]);
}

#[test]
fn test_continue_labelled() {
    let output = do_continue_labelled((1, 2, 3, 4));
    assert_eq!(output, vec![0, 1, 3]);
}

#[test]
fn test_break() {
    let output = do_break((1, 2, 3, 4));
    assert_eq!(output, vec![0, 1]);
}

#[test]
fn test_break_labelled() {
    let output = do_break_labelled((1, 2, 3, 4));
    assert_eq!(output, vec![0, 1]);
}

#[test]
fn test_out_of_typle_range() {
    assert_eq!(check_out_of_bounds(()), 3);
}

#[test]
fn test_negative_range() {
    assert_eq!(check_negative_range(()), 0);
}
