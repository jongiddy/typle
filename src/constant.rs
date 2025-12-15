use std::ops::Bound;

use proc_macro2::Span;
use syn::spanned::Spanned as _;
use syn::{Error, Expr, Stmt};

#[allow(clippy::single_match)]
pub fn evaluate_bool(expr: &Expr) -> syn::Result<bool> {
    match expr {
        Expr::Binary(binary) => match binary.op {
            syn::BinOp::Eq(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left == right);
                    }
                }
            }
            syn::BinOp::Ne(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left != right);
                    }
                }
            }
            syn::BinOp::Lt(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left < right);
                    }
                }
            }
            syn::BinOp::Le(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left <= right);
                    }
                }
            }
            syn::BinOp::Gt(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left > right);
                    }
                }
            }
            syn::BinOp::Ge(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return Ok(left >= right);
                    }
                }
            }
            syn::BinOp::And(_) => {
                return Ok(evaluate_bool(&binary.left)? && evaluate_bool(&binary.right)?);
            }
            syn::BinOp::Or(_) => {
                return Ok(evaluate_bool(&binary.left)? || evaluate_bool(&binary.right)?);
            }
            _ => {}
        },
        _ => {}
    }
    Err(Error::new(expr.span(), "unsupported boolean expression"))
}

// None -> Not a range
// Bound<Err(Span)> -> Span for a bound that is not an integer
#[allow(clippy::type_complexity)]
pub fn evaluate_range(
    expr: &Expr,
) -> Option<(Bound<Result<usize, Span>>, Bound<Result<usize, Span>>)> {
    match expr {
        Expr::Block(block) => {
            let mut stmts = block.block.stmts.iter().fuse();
            if let (Some(Stmt::Expr(expr, None)), None) = (stmts.next(), stmts.next()) {
                return evaluate_range(expr);
            }
        }
        Expr::Range(range) => {
            let start = range.start.as_ref().map_or(Bound::Unbounded, |start_expr| {
                Bound::Included(
                    evaluate_usize(start_expr)
                        .map(Ok)
                        .unwrap_or_else(|| Err(start_expr.span())),
                )
            });
            let end = range.end.as_ref().map_or(Bound::Unbounded, |end_expr| {
                let end = evaluate_usize(end_expr)
                    .map(Ok)
                    .unwrap_or_else(|| Err(end_expr.span()));
                match range.limits {
                    syn::RangeLimits::HalfOpen(_) => Bound::Excluded(end),
                    syn::RangeLimits::Closed(_) => Bound::Included(end),
                }
            });
            return Some((start, end));
        }
        _ => {}
    }
    None
}

pub fn evaluate_usize(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Block(block) => {
            if block.block.stmts.len() == 1 {
                if let Stmt::Expr(expr, _) = &block.block.stmts[0] {
                    return evaluate_usize(expr);
                }
            }
        }
        Expr::Lit(literal) => {
            if let syn::Lit::Int(int) = &literal.lit {
                return int.base10_parse().ok();
            }
        }
        Expr::Binary(binary) => match binary.op {
            syn::BinOp::Add(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_add(right);
                    }
                }
            }
            syn::BinOp::Div(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_div(right);
                    }
                }
            }
            syn::BinOp::Sub(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_sub(right);
                    }
                }
            }
            syn::BinOp::Mul(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_mul(right);
                    }
                }
            }
            syn::BinOp::Rem(_) => {
                if let Some(left) = evaluate_usize(&binary.left) {
                    if let Some(right) = evaluate_usize(&binary.right) {
                        return left.checked_rem(right);
                    }
                }
            }
            _ => {}
        },
        Expr::Paren(expr) => {
            return evaluate_usize(&expr.expr);
        }
        _ => {}
    }
    None
}
