# 0.11

## Backwards-incompatible changes
- `typle_for!` no longer supports different behaviour based on delimiters.
    - To create an array use `typle!` inside brackets.
    `typle_for![i in .. => t[[i]]]` becomes `[typle!(i in .. => t[[i]])]`.
    - `typle_for!` macro no longer needs `typle_const!` on an `if` expression.
    - `typle_for!` macro no longer requires `typle_ty!` and `typle_pat!` for
non-expression types and patterns inside an `if` body.

- `typle_get!` is replaced by `match` with a `typle_index!` range.

## Other changes
- add `LAST` associated variable equal to `LEN - 1`.
