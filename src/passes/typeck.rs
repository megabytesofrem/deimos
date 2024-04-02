// Type checker pass
// 1. We take the AST
// 2. We check the types of the AST and report errors
// 3. We return a THIR (Typed HIR) representation of the AST

use super::context::ContextStack;
use crate::{
    spanned,
    syntax::{
        ast::{Block, Expr, Literal, Program, Stmt, ToplevelStmt, Ty},
        lexer::{BinOp, SourceLoc, UnOp},
        span::Spanned,
    },
};

use super::thir::*;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeckError {
    #[error("Type mismatch between {expected:?} and {found:?}")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("Undefined local {name} being used before declaration")]
    UndefinedLocal { name: String, location: SourceLoc },

    #[error("Undefined function {name} being used before declaration")]
    UndefinedFunction { name: String, location: SourceLoc },

    #[error("Arity mismatch between {expected} and {found}")]
    ArityMismatch {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    #[error("Return type mismatch between {expected:?} and {found:?}")]
    ReturnTypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },
}

type TypeckResult<T> = Result<T, TypeckError>;

// Type checker pass
#[derive(Debug, Clone)]
pub struct Typeck<'cx> {
    ctx: ContextStack,
    marker: std::marker::PhantomData<&'cx ()>,
}

impl<'cx> Typeck<'cx> {
    pub fn new() -> Self {
        Typeck {
            ctx: ContextStack::new(),
            marker: std::marker::PhantomData,
        }
    }

    // Inference functions

    fn infer_binop(&self, lhs: &Spanned<Expr>, op: BinOp, rhs: &Spanned<Expr>) -> TypeckResult<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        if lhs_ty != rhs_ty {
            return Err(TypeckError::TypeMismatch {
                expected: lhs_ty,
                found: rhs_ty,
                location: lhs.location.clone(),
            });
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if lhs_ty != Ty::Int && rhs_ty != Ty::Float && rhs_ty != Ty::Double {
                    return Err(TypeckError::TypeMismatch {
                        expected: Ty::Int,
                        found: lhs_ty,
                        location: lhs.location.clone(),
                    });
                }

                Ok(lhs_ty)
            }
            BinOp::Eq
            | BinOp::BangEq
            | BinOp::Less
            | BinOp::LessEq
            | BinOp::Greater
            | BinOp::GreaterEq
            | BinOp::And
            | BinOp::Or => Ok(Ty::Bool),
        }
    }

    fn infer_array(&self, elems: &[Spanned<Expr>]) -> TypeckResult<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Unknown)));
        }

        // Infer the overall type of the array from the first element
        let elem_ty = self.infer_expr(&elems[0])?;
        elems.iter().skip(1).try_for_each(|elem| {
            let ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                return Err(TypeckError::TypeMismatch {
                    expected: elem_ty.clone(),
                    found: ty,
                    location: elem.location.clone(),
                });
            }
            Ok(())
        })?;

        Ok(Ty::Array(Box::new(elem_ty)))
    }

    fn infer_tuple(&self, elems: &[Spanned<Expr>]) -> TypeckResult<Ty> {
        let mut tys = Vec::new();
        for elem in elems {
            tys.push(self.infer_expr(elem)?);
        }

        Ok(Ty::Tuple(tys))
    }

    fn infer_arraylike_index(
        &self,
        indexable: &Spanned<Expr>,
        index: &Spanned<Expr>,
    ) -> TypeckResult<Ty> {
        let indexable_ty = self.infer_expr(indexable)?;
        let index_ty = self.infer_expr(index)?;

        // Check if `index_ty` is a valid index type or not
        if !index_ty.is_valid_index_type() {
            return Err(TypeckError::TypeMismatch {
                expected: Ty::Int,
                found: index_ty,
                location: index.location.clone(),
            });
        }

        match indexable_ty {
            Ty::Array(ty) => Ok(*ty),
            _ => Err(TypeckError::TypeMismatch {
                expected: Ty::Array(Box::new(Ty::Unknown)),
                found: indexable_ty,
                location: indexable.location.clone(),
            }),
        }
    }

    fn infer_expr(&self, expr: &Spanned<Expr>) -> TypeckResult<Ty> {
        match &expr.target {
            Expr::Literal(lit) => self.check_literal(lit),
            Expr::Variable(name) => self.check_variable(name, expr.location.clone()),
            Expr::BinOp(lhs, op, rhs) => self.infer_binop(lhs, op.clone(), rhs),
            Expr::Array(elems) => self.infer_array(elems),
            Expr::Tuple(elems) => self.infer_tuple(elems),
            Expr::ArrayIndex { array, index } => self.infer_arraylike_index(array, index),
            Expr::Call { .. } => todo!(),
            _ => todo!(),
        }
    }

    // Checking functions

    fn check_variable(&self, name: &str, location: SourceLoc) -> TypeckResult<Ty> {
        self.ctx
            .get(name)
            .cloned()
            .ok_or_else(|| TypeckError::UndefinedLocal {
                name: name.to_string(),
                location,
            })
    }

    fn check_literal(&self, lit: &Literal) -> TypeckResult<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Int),
            Literal::Float(_) => Ok(Ty::Float),
            Literal::Double(_) => Ok(Ty::Double),
            Literal::Bool(_) => Ok(Ty::Bool),
            Literal::String(_) => Ok(Ty::String),
        }
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> TypeckResult<THIRExpr> {
        let ty = self.infer_expr(expr)?;
        match &expr.target {
            Expr::Literal(lit) => {
                return Ok(THIRExpr::Literal(lit.clone(), ty));
            }
            Expr::Variable(name) => {
                let ty2 = self.check_variable(name, expr.location.clone())?;
                if ty != ty2 {
                    return Err(TypeckError::TypeMismatch {
                        expected: ty,
                        found: ty2,
                        location: expr.location.clone(),
                    });
                } else {
                    // Variable is valid
                    return Ok(THIRExpr::Variable(name.clone()));
                }
            }
            Expr::BinOp(lhs, op, rhs) => {
                let lhs_ = spanned!(self.check_expr(lhs)?, lhs.location.clone());
                let rhs_ = spanned!(self.check_expr(rhs)?, rhs.location.clone());
                return Ok(THIRExpr::BinOp(Box::new(lhs_), op.clone(), Box::new(rhs_)));
            }
            Expr::UnOp(op, expr) => {
                let expr_ = spanned!(self.check_expr(expr)?, expr.location.clone());
                return Ok(THIRExpr::UnOp(op.clone(), Box::new(expr_)));
            }
            Expr::Array(elems) => {
                let mut elems_ = Vec::new();
                for elem in elems {
                    elems_.push(spanned!(self.check_expr(elem)?, elem.location.clone()));
                }

                return Ok(THIRExpr::Array { elems: elems_ });
            }
            Expr::Tuple(elems) => {
                let mut elems_ = Vec::new();
                for elem in elems {
                    elems_.push(spanned!(self.check_expr(elem)?, elem.location.clone()));
                }

                return Ok(THIRExpr::Tuple { elems: elems_ });
            }
            Expr::ArrayIndex { array, index } => {
                let array_ = spanned!(self.check_expr(array)?, array.location.clone());
                let index_ = spanned!(self.check_expr(index)?, index.location.clone());
                return Ok(THIRExpr::ArrayIndex {
                    array: Box::new(array_),
                    index: Box::new(index_),
                });
            }
            Expr::Call { callee, args } => {
                let callee_ = spanned!(self.check_expr(callee)?, callee.location.clone());
                let mut args_ = Vec::new();
                for arg in args {
                    args_.push(spanned!(self.check_expr(arg)?, arg.location.clone()));
                }

                return Ok(THIRExpr::Call {
                    callee: Box::new(callee_),
                    args: args_,
                });
            }
            _ => unimplemented!(),
        }
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> TypeckResult<()> {
        match &stmt.target {
            Stmt::Expr(expr) => {
                self.check_expr(expr)?;
                Ok(())
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(expr)?;
                }

                Ok(())
            }
            Stmt::Local { name, ty, value } => {
                let ty = ty.clone().unwrap_or(Ty::Unknown);
                let value = value.as_ref().map(|v| self.check_expr(v));

                self.ctx.insert(name.clone(), ty);
                Ok(())
            }
            Stmt::Assign { target, value } => {
                let target_ty = self.infer_expr(target)?;
                let value_ty = self.infer_expr(value)?;

                if target_ty != value_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: target_ty,
                        found: value_ty,
                        location: value.location.clone(),
                    });
                }

                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn check_block(&mut self, block: &Block) -> TypeckResult<()> {
        for stmt in block {
            self.check_stmt(&stmt)?;
        }

        Ok(())
    }

    fn check_toplevel_stmt(&mut self, stmt: &Spanned<ToplevelStmt>) -> TypeckResult<()> {
        match &stmt.target {
            ToplevelStmt::Stmt(stmt) => self.check_stmt(stmt),
            ToplevelStmt::Import { .. } => Ok(()),
            ToplevelStmt::FunctionDecl {
                name,
                return_ty,
                params,
                body,
            } => {
                self.ctx.insert(name.clone(), return_ty.clone());
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    pub fn check(ast: Program) -> TypeckResult<()> {
        let mut typeck = Typeck::new();

        for stmt in ast {
            typeck.check_toplevel_stmt(&stmt)?;
        }

        Ok(())
    }
}

impl Default for Typeck<'_> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
pub mod typeck_tests {
    use super::*;

    #[test]
    fn expr_with_int_float() {
        let mut typeck = Typeck::new();
        let expr = spanned!(
            Expr::BinOp(
                Box::new(spanned!(
                    Expr::Literal(Literal::Int(42)),
                    SourceLoc::default()
                )),
                BinOp::Add,
                Box::new(spanned!(
                    Expr::Literal(Literal::Float(42.0)),
                    SourceLoc::default()
                ))
            ),
            SourceLoc::default()
        );

        let typed = typeck.check_expr(&expr);

        // cannot add int and float
        assert!(typed.is_err());
    }

    #[test]
    fn tuple_holds_compatible_types() {
        let mut typeck = Typeck::new();
        let expr = spanned!(
            Expr::Tuple(vec![
                spanned!(Expr::Literal(Literal::Int(42)), SourceLoc::default()),
                spanned!(Expr::Literal(Literal::Int(42)), SourceLoc::default()),
            ]),
            SourceLoc::default()
        );

        let typed = typeck.check_expr(&expr);
        assert!(typed.is_ok());
    }

    #[test]
    fn array_holds_compatible_types() {
        let mut typeck = Typeck::new();
        let expr = spanned!(
            Expr::Array(vec![
                spanned!(Expr::Literal(Literal::Int(42)), SourceLoc::default()),
                spanned!(Expr::Literal(Literal::Int(42)), SourceLoc::default()),
            ]),
            SourceLoc::default()
        );

        let typed = typeck.check_expr(&expr);
        assert!(typed.is_ok());
    }
}
