use syn::spanned::Spanned as _;
use syn::{Error, Expr, Result, Stmt};

#[allow(clippy::single_match)]
pub fn evaluate_bool(expr: &Expr) -> Result<bool> {
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
            _ => {}
        },
        _ => {}
    }
    Err(Error::new(expr.span(), "unsupported boolean expression"))
}

pub fn evaluate_range(expr: &Expr) -> Option<&syn::ExprRange> {
    match expr {
        Expr::Block(block) => {
            if block.block.stmts.len() == 1 {
                if let Stmt::Expr(expr, _) = &block.block.stmts[0] {
                    return evaluate_range(expr);
                }
            }
        }
        Expr::Range(range) => {
            return Some(range);
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
