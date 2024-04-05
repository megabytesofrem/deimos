use crate::{
    passes::typed::TExpr,
    syntax::{
        ast::Literal,
        lexer::{BinOp, UnOp},
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct Compiler {
    builder: String,
}

impl Compiler {
    fn emit_literal(&self, lit: &Literal) -> String {
        match lit {
            Literal::Int(i) => i.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            _ => todo!(),
        }
    }

    fn emit_expr(&mut self, expr: &TExpr) -> String {
        match expr {
            TExpr::Literal(lit, _) => self.emit_literal(lit),
            TExpr::Variable(name) => name.clone(),
            TExpr::BinOp(lhs, binop, rhs) => {
                self.emit_binop_expr(binop.clone(), &lhs.target, &rhs.target)
            }
            _ => todo!(),
        }
    }

    fn emit_binop_expr(&mut self, binop: BinOp, lhs: &TExpr, rhs: &TExpr) -> String {
        let lhs = self.emit_expr(lhs);
        let rhs = self.emit_expr(rhs);

        format!("{} {} {}", lhs, binop.to_str(), rhs)
    }

    fn emit_unop_expr(&mut self, unop: UnOp, expr: &TExpr) -> String {
        let value = self.emit_expr(expr);
        format!("{}{}", unop.to_str(), value)
    }
}
