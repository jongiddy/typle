# 0.13

## Backwards-incompatible changes
- Remove `typle_for!`. Use `typle!`.

## Other changes
- Allow `typle!` macro with no range in singleton position.

# 0.12

## Backwards-incompatible changes
- `typle_bounds!` has been replaced by support for `typle!` macro in where clauses.
- `typle_args` as a synonym for `typle!` has been removed.

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
- `typle_for!` is deprecated. Use `typle!` inside a tuple or array instead.
- add `LAST` associated variable equal to `LEN - 1`.
