#![allow(unreachable_code)]
use typle::typle;
struct Looper<T> {
    t: T,
}
impl<T0, T1, T2, T3, T4, T5, T6, T7> Looper<(T0, T1, T2, T3, T4, T5, T6, T7)> {
    fn for_loop(&self) {
        'label: {
            {
                {
                    let _x = 0;
                }
                {}
            }
            let mut _typle_break = false;
            if !_typle_break {
                loop {
                    if _typle_break {
                        _typle_break = false;
                        break;
                    }
                    _typle_break = true;
                    {
                        {
                            let _x = 2;
                            break;
                        }
                        {}
                    }
                }
            }
            if !_typle_break {
                loop {
                    if _typle_break {
                        _typle_break = false;
                        break;
                    }
                    _typle_break = true;
                    {
                        {
                            let _x = 3;
                            continue;
                        }
                        {}
                    }
                }
            }
            if !_typle_break {
                {
                    let _x = 4;
                    break 'label;
                }
                {}
            }
            if !_typle_break {
                {
                    for _j in 0..2 {
                        continue;
                    }
                    while 5 == 3 {
                        continue;
                    }
                    loop {
                        break;
                    }
                }
            }
            ()
        }
    }
}