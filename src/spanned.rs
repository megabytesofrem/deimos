use serde::{Deserialize, Serialize};

use crate::syntax::ast::{Expr, Member, Stmt};
use crate::syntax::lexer::SourceLoc;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub target: T,
    pub location: SourceLoc,
}

pub fn spanned<T>(target: T, location: SourceLoc) -> Spanned<T> {
    Spanned { target, location }
}

/// Helpful functions to work with `Spanned` values

impl Expr {
    pub fn spanned_default(self) -> Spanned<Expr> {
        Spanned {
            target: self,
            location: SourceLoc::default(),
        }
    }
}

impl Stmt {
    pub fn spanned_default(self) -> Spanned<Stmt> {
        Spanned {
            target: self,
            location: SourceLoc::default(),
        }
    }
}

impl Expr {
    /// Strip the span from an expression, yielding the underlying expression
    pub fn strip_span(&self) -> Expr {
        match self {
            Expr::Literal(lit) => Expr::Literal(lit.clone()),
            Expr::Ident(ident) => Expr::Ident(ident.clone()),
            Expr::Member(member) => {
                // Strip the span from the `target` expression within the `Member`
                let stripped_target = member.target.target.strip_span();

                // Reconstruct the `Member` with the stripped `target` and the original `name`
                Expr::Member(*Box::new(Member {
                    target: Box::new(stripped_target.spanned_default()),
                    name: member.name.clone(),
                }))
            }

            Expr::Reference(expr) => {
                Expr::Reference(Box::new(expr.target.strip_span().spanned_default()))
            }

            Expr::BinOp(lhs, op, rhs) => Expr::BinOp(
                Box::new(lhs.target.strip_span().spanned_default()),
                op.clone(),
                Box::new(rhs.target.strip_span().spanned_default()),
            ),

            Expr::UnOp(op, expr) => Expr::UnOp(
                op.clone(),
                Box::new(expr.target.strip_span().spanned_default()),
            ),

            Expr::Array(exprs) => Expr::Array(
                exprs
                    .iter()
                    .map(|expr| expr.target.strip_span().spanned_default())
                    .collect(),
            ),

            Expr::Tuple(exprs) => Expr::Tuple(
                exprs
                    .iter()
                    .map(|expr| expr.target.strip_span().spanned_default())
                    .collect(),
            ),

            Expr::Cast(expr, ty) => Expr::Cast(
                Box::new(expr.target.strip_span().spanned_default()),
                ty.clone(),
            ),

            Expr::StructCons { fields } => Expr::StructCons {
                fields: fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), expr.target.strip_span().spanned_default()))
                    .collect(),
            },

            Expr::ArrayIndex { array, index } => Expr::ArrayIndex {
                array: Box::new(array.target.strip_span().spanned_default()),
                index: Box::new(index.target.strip_span().spanned_default()),
            },

            Expr::Call { callee, args } => Expr::Call {
                callee: Box::new(callee.target.strip_span().spanned_default()),
                args: args
                    .iter()
                    .map(|arg| arg.target.strip_span().spanned_default())
                    .collect(),
            },
        }
    }
}

impl Stmt {
    pub fn strip_span(&self) -> Stmt {
        match self {
            Stmt::Expr(expr) => Stmt::Expr(expr.target.strip_span().spanned_default()),
            Stmt::Return(expr) => Stmt::Return(
                expr.as_ref()
                    .map(|expr| expr.target.strip_span().spanned_default()),
            ),

            // This is not an actual statement -- it's sole existence is to mark terminated
            // blocks in the AST for if/elif/else
            Stmt::BlockTerminator => Stmt::BlockTerminator,

            Stmt::Let { name, ty, value } => Stmt::Let {
                name: name.clone(),
                ty: ty.clone(),
                value: value
                    .as_ref()
                    .map(|expr| expr.target.strip_span().spanned_default()),
            },

            Stmt::Assign { name, value } => Stmt::Assign {
                name: name.target.strip_span().spanned_default(),
                value: value.target.strip_span().spanned_default(),
            },

            Stmt::If {
                cond,
                then_block,
                elif_blocks,
                else_block,
            } => Stmt::If {
                cond: cond.target.strip_span().spanned_default(),
                then_block: then_block
                    .iter()
                    .map(|stmt| stmt.target.strip_span().spanned_default())
                    .collect(),
                elif_blocks: elif_blocks
                    .iter()
                    .map(|(cond, block)| {
                        (
                            cond.target.strip_span().spanned_default(),
                            block
                                .iter()
                                .map(|stmt| stmt.target.strip_span().spanned_default())
                                .collect(),
                        )
                    })
                    .collect(),
                else_block: else_block.as_ref().map(|block| {
                    block
                        .iter()
                        .map(|stmt| stmt.target.strip_span().spanned_default())
                        .collect()
                }),
            },

            Stmt::For {
                init,
                from,
                to,
                body,
            } => Stmt::For {
                init: init.clone(),
                from: from.target.strip_span().spanned_default(),
                to: to.target.strip_span().spanned_default(),
                body: body
                    .iter()
                    .map(|stmt| stmt.target.strip_span().spanned_default())
                    .collect(),
            },

            Stmt::While { cond, body } => Stmt::While {
                cond: cond.target.strip_span().spanned_default(),
                body: body
                    .iter()
                    .map(|stmt| stmt.target.strip_span().spanned_default())
                    .collect(),
            },
        }
    }
}

impl<T> Spanned<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            target: f(self.target),
            location: self.location,
        }
    }

    pub fn flat_map<U, F: FnOnce(T) -> Spanned<U>>(self, f: F) -> Spanned<U> {
        let spanned = f(self.target);
        Spanned {
            target: spanned.target,
            location: self.location,
        }
    }

    pub fn map_with_span<U, E, F: FnOnce(T) -> Result<U, E>>(self, f: F) -> Result<Spanned<U>, E> {
        let spanned = f(self.target)?;
        Ok(Spanned {
            target: spanned,
            location: self.location,
        })
    }
}
