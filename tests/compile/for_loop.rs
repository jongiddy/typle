#![allow(unreachable_code, unused)]

use typle::typle;

struct Looper<T> {
    t: T,
}

#[typle(Tuple for 8..=8)]
#[allow(clippy::never_loop)]
impl<T: Tuple> Looper<T> {
    fn for_loop(&self) {
        'label: for typle_index!(i) in 0..T::LEN {
            // This block does not have a break or continue so does not need a loop.
            if typle_const!(i == 0) {
                let _x = i;
            }
            // For i = 1 no blocks are true. All blocks get removed.
            if typle_const!(i == 2) {
                let _x = i;
                break;
            }
            if typle_const!(i == 3) {
                let _x = i;
                continue;
            }
            // With no local break or continue there is no loop, but we still need to
            // check that the initial flag does not indicate a break.
            if typle_const!(i == 4) {
                let _x = i;
                break 'label;
            }
            // Unlabelled breaks or continues in inner loops do not create a loop.
            if typle_const!(i == 5) {
                for _j in 0..2 {
                    continue;
                }
                while i == 3 {
                    continue;
                }
                loop {
                    break;
                }
            }
            // For i = 6+ no blocks are true. All blocks get removed, also allowing
            // the loop handling code to be removed.
        }
    }
}

#[typle(Tuple for 4..=4)]
pub fn do_continue<T: Tuple>(t: T) -> Vec<usize> {
    let mut output = Vec::new();
    for typle_index!(i) in 0..T::LEN {
        if typle_const!(i == 2) {
            continue;
        }
        output.push(i);
    }
    output
}

#[typle(Tuple for 4..=4)]
#[allow(clippy::never_loop)]
pub fn do_continue_labelled<T: Tuple>(t: T) -> Vec<usize> {
    let mut output = Vec::new();
    'label: for typle_index!(i) in 0..T::LEN {
        loop {
            if typle_const!(i == 2) {
                continue 'label;
            }
            output.push(i);
            break;
        }
    }
    output
}

#[typle(Tuple for 4..=4)]
pub fn do_break<T: Tuple>(t: T) -> Vec<usize> {
    let mut output = Vec::new();
    for typle_index!(i) in 0..T::LEN {
        if typle_const!(i == 2) {
            break;
        }
        output.push(i);
    }
    output
}

#[typle(Tuple for 4..=4)]
#[allow(clippy::never_loop)]
pub fn do_break_labelled<T: Tuple>(t: T) -> Vec<usize> {
    let mut output = Vec::new();
    'label: for typle_index!(i) in 0..T::LEN {
        loop {
            if typle_const!(i == 2) {
                break 'label;
            }
            output.push(i);
            break;
        }
    }
    output
}

// Check indexes out of tuple range
#[typle(Tuple for 0..=0)]
pub fn check_out_of_bounds<T: Tuple>(t: T) -> usize {
    let mut count = 0;
    for typle_index!(i) in 15..18 {
        count += 1;
    }
    count
}

// Check negative ranges
#[typle(Tuple for 0..=0)]
pub fn check_negative_range<T: Tuple>(t: T) -> usize {
    #[allow(clippy::let_and_return)]
    let mut count = 0;
    for typle_index!(i) in 4..2 {
        count += 1;
    }
    count
}
