#![allow(unreachable_code)]

use typle::typle;

struct Looper<T> {
    t: T,
}

#[typle(Tuple for 8..=8)]
impl<T> Looper<T>
where
    T: Tuple,
{
    fn for_loop(&self) {
        'label: for typle_const!(i) in 0..T::LEN {
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
