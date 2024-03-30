// Typed HIR (THIR) transformation pass
// 1. We take the AST
// 2. We lower it, bit by bit into a typed HIR (THIR)
// 3. The THIR is a more abstract representation of the AST, and has types attached to it

use crate::spanned;
use crate::syntax::{
    ast::{Expr, Program, Stmt, ToplevelStmt, Ty},
    lexer::{BinOp, SourceLoc, UnOp},
    span::Spanned,
};

use super::thir::*;

/// Report a dummy location for placeholders
fn dummy_location() -> SourceLoc {
    Default::default()
}

fn lower_binop(lhs: &Spanned<Expr>, op: BinOp, rhs: &Spanned<Expr>) -> HIRExpr {
    HIRExpr::BinOp(Box::new(lower_expr(lhs)), op, Box::new(lower_expr(rhs)))
}

fn lower_unop(op: UnOp, expr: &Spanned<Expr>) -> HIRExpr {
    HIRExpr::UnOp(op, Box::new(lower_expr(expr)))
}

fn lower_expr(expr: &Spanned<Expr>) -> Spanned<HIRExpr> {
    match &expr.target {
        Expr::Literal(lit) => {
            spanned!(
                HIRExpr::Literal(lit.clone(), Ty::Unknown),
                expr.location.clone()
            )
        }
        Expr::Variable(ref v) => {
            spanned!(HIRExpr::Variable(v.clone()), expr.location.clone())
        }
        Expr::BinOp(ref lhs, op, ref rhs) => {
            spanned!(lower_binop(lhs, op.clone(), rhs), expr.location.clone())
        }
        Expr::UnOp(op, e) => spanned!(lower_unop(op.clone(), e), expr.location.clone()),
        Expr::Array(elems) => spanned!(
            HIRExpr::Array {
                elems: elems.iter().map(lower_expr).collect()
            },
            expr.location.clone()
        ),
        Expr::Tuple(elems) => spanned!(
            HIRExpr::Tuple {
                elems: elems.iter().map(lower_expr).collect()
            },
            expr.location.clone()
        ),

        _ => unimplemented!(),
    }
}

fn lower_stmt(stmt: &Spanned<Stmt>) -> Spanned<HIRStmt> {
    let lowered_stmt = match &stmt.target {
        Stmt::Expr(expr) => HIRStmt::Expr(lower_expr(expr).target),
        Stmt::Return(expr) => HIRStmt::Return(expr.as_ref().map(lower_expr)),

        Stmt::Local { name, ty, value } => HIRStmt::Local {
            name: name.clone(),
            ty: ty.clone(),
            value: value.as_ref().map(|e| lower_expr(e)),
        },
        Stmt::StructDecl { name, fields } => HIRStmt::StructDecl {
            name: name.clone(),
            fields: fields.clone(),
        },
        Stmt::Assign { target, value } => HIRStmt::Assign {
            // We don't care about the location of the assignment target
            target: lower_expr(&spanned!(target.clone(), dummy_location())).target,
            value: lower_expr(value),
        },
        Stmt::If {
            cond,
            then_block,
            else_block,
        } => HIRStmt::If {
            cond: lower_expr(cond),
            then_block: lower_block(then_block.to_vec()),
            else_block: else_block.as_ref().map(|b| lower_block(b.to_vec())),
        },
        Stmt::For {
            init,
            from,
            to,
            body,
        } => HIRStmt::For {
            init: init.clone(),
            from: lower_expr(from),
            to: lower_expr(to),
            body: lower_block(body.to_vec()),
        },

        _ => unimplemented!("yes"),
    };

    spanned!(lowered_stmt, stmt.location.clone())
}

fn lower_block(block: Vec<Spanned<Stmt>>) -> HIRBlock {
    block.iter().map(lower_stmt).collect()
}

fn lower_toplevel_stmt(stmt: &Spanned<ToplevelStmt>) -> Spanned<HIRToplevelStmt> {
    let lowered_stmt = match &stmt.target {
        ToplevelStmt::Stmt(stmt) => HIRToplevelStmt::Stmt(lower_stmt(stmt).target),
        ToplevelStmt::Import { path, alias } => HIRToplevelStmt::ToplevelStmt(stmt.target.clone()),
        ToplevelStmt::FunctionDecl {
            name,
            return_ty,
            params,
            body,
        } => HIRToplevelStmt::ToplevelStmt(stmt.target.clone()),
        _ => unimplemented!(),
    };

    spanned!(lowered_stmt, stmt.location.clone())
}

fn lower_to_thir(program: &Program) -> Vec<Spanned<HIRToplevelStmt>> {
    program.iter().map(lower_toplevel_stmt).collect()
}

#[cfg(test)]
mod thir_tests {
    use super::*;
    use crate::syntax::ast::Literal;
    use crate::syntax::parse::Parser;

    #[test]
    fn test_lower_assign() {
        let mut parser = Parser::new("local x:int = 1");
        let stmt = parser.parse_stmt().unwrap();

        let lowered = lower_stmt(&stmt);
        println!("{:#?}", lowered);
    }

    #[test]
    fn test_lower_binop() {
        let mut parser = Parser::new("local x:int = 1 + 2");
        let stmt = parser.parse_stmt().unwrap();

        let lowered = lower_stmt(&stmt);
        println!("{:#?}", lowered);
    }

    #[test]
    fn test_lower_block() {
        let mut parser = Parser::new(
            r#"
                local x:int = 1
                local y:int = 2
            end"#,
        );
        let block = parser.parse_block().unwrap();

        let lowered = lower_block(block);
        println!("{:#?}", lowered);
    }

    #[test]
    fn test_lower_program() {
        let str = r#"
                local x:int = 1
                local y:int = 2
            "#;
        let program = Parser::parse(str).unwrap();

        let lowered = lower_to_thir(&program);
        println!("{:#?}", lowered);
    }
}
