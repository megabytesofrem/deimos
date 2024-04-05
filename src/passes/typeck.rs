// Type checker pass
// 1. We take the AST
// 2. We check the types of the AST and report errors
// 3. We return a typed representation of the AST

use super::context::ContextStack;
use crate::{
    spanned,
    syntax::{
        ast::{Block, Expr, Literal, Program, Stmt, ToplevelStmt, Ty},
        lexer::{BinOp, SourceLoc},
        span::{self, Spanned},
    },
};

use super::typed::*;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum TypeckError {
    #[error("Type mismatch between {expected:?} and {found:?}")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("Redefinition of local {name}")]
    Redefinition { name: String, location: SourceLoc },

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

// Holds the type that is returned from the type checker
type Return<T> = Result<T, TypeckError>;

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

    fn infer_binop(&self, lhs: &Spanned<Expr>, op: BinOp, rhs: &Spanned<Expr>) -> Return<Ty> {
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

    fn infer_array(&self, elems: &[Spanned<Expr>]) -> Return<Ty> {
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

    fn infer_tuple(&self, elems: &[Spanned<Expr>]) -> Return<Ty> {
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
    ) -> Return<Ty> {
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

    fn infer_expr(&self, expr: &Spanned<Expr>) -> Return<Ty> {
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
    // We operate on the AST wrapped in Spanned<T> and return a THIR lowered
    // equivalent, e.g Spanned<Stmt> -> Spanned<THIRStmt>.

    fn check_variable(&self, name: &str, location: SourceLoc) -> Return<Ty> {
        self.ctx
            .get(name)
            .cloned()
            .ok_or_else(|| TypeckError::UndefinedLocal {
                name: name.to_string(),
                location,
            })
    }

    fn check_literal(&self, lit: &Literal) -> Return<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Int),
            Literal::Float(_) => Ok(Ty::Float),
            Literal::Double(_) => Ok(Ty::Double),
            Literal::Bool(_) => Ok(Ty::Bool),
            Literal::String(_) => Ok(Ty::String),
        }
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Return<Spanned<TExpr>> {
        let ty = self.infer_expr(expr)?;
        match &expr.target {
            Expr::Literal(lit) => {
                return Ok(spanned!(
                    TExpr::Literal(lit.clone(), ty),
                    expr.location.clone()
                ));
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
                    return Ok(spanned!(
                        TExpr::Variable(name.clone()),
                        expr.location.clone()
                    ));
                }
            }
            Expr::BinOp(lhs, op, rhs) => {
                return Ok(spanned!(
                    TExpr::BinOp(
                        Box::new(self.check_expr(lhs)?),
                        op.clone(),
                        Box::new(self.check_expr(rhs)?)
                    ),
                    expr.location.clone()
                ));
            }
            Expr::UnOp(op, expr) => {
                let expr = self.check_expr(expr)?;
                return Ok(spanned!(
                    TExpr::UnOp(op.clone(), Box::new(expr.clone())),
                    expr.location.clone()
                ));
            }
            Expr::Array(elems) | Expr::Tuple(elems) => {
                let mut array = Vec::new();
                for elem in elems {
                    array.push(self.check_expr(elem)?);
                }

                return Ok(spanned!(
                    TExpr::Array { elems: array },
                    expr.location.clone()
                ));
            }
            Expr::ArrayIndex { array, index } => {
                let array = self.check_expr(array)?;
                let index = self.check_expr(index)?;
                return Ok(spanned!(
                    TExpr::ArrayIndex {
                        array: Box::new(array),
                        index: Box::new(index),
                    },
                    expr.location.clone()
                ));
            }
            Expr::Call { callee, args } => {
                let callee = self.check_expr(expr)?;
                let mut args_ = Vec::new();
                for arg in args {
                    args_.push(self.check_expr(arg)?);
                }

                return Ok(spanned!(
                    TExpr::Call {
                        callee: Box::new(callee),
                        args: args_,
                    },
                    expr.location.clone()
                ));
            }
            _ => unimplemented!(),
        }
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Return<Spanned<TStmt>> {
        match &stmt.target {
            Stmt::Expr(expr) => {
                let expr = self.check_expr(expr)?;
                Ok(spanned!(TStmt::Expr(expr.target), stmt.location.clone()))
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let expr = self.check_expr(expr)?;
                    return Ok(spanned!(TStmt::Return(Some(expr)), stmt.location.clone()));
                }

                Ok(spanned!(TStmt::Return(None), stmt.location.clone()))
            }
            Stmt::Local { name, ty, value } => {
                let ty = ty.clone().unwrap_or(Ty::Unknown);
                let value = value.as_ref().map(|v| self.check_expr(v));

                // Check if the local is not already defined
                if self.ctx.get(name).is_some() {
                    return Err(TypeckError::Redefinition {
                        name: name.clone(),
                        location: stmt.location.clone(),
                    });
                }

                self.ctx.insert(name.clone(), ty.clone());
                Ok(spanned!(
                    TStmt::Local {
                        name: name.clone(),
                        ty: Some(ty),
                        value: value.map(|v| v.unwrap()),
                    },
                    stmt.location.clone()
                ))
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

                Ok(spanned!(
                    TStmt::Assign {
                        target: self.check_expr(target)?.target,
                        value: self.check_expr(value)?,
                    },
                    stmt.location.clone()
                ))
            }
            _ => unimplemented!(),
        }
    }

    fn check_block(&mut self, block: &Block) -> Return<Vec<Spanned<TStmt>>> {
        let block_ = block
            .iter()
            .map(|stmt| self.check_stmt(stmt))
            .collect::<Return<Vec<Spanned<TStmt>>>>()?;

        Ok(block_)
    }

    fn check_toplevel_stmt(
        &mut self,
        stmt: &Spanned<ToplevelStmt>,
    ) -> Return<Spanned<TToplevelStmt>> {
        match &stmt.target {
            ToplevelStmt::Stmt(stmt) => {
                let stmt = self.check_stmt(stmt)?;
                Ok(spanned!(
                    TToplevelStmt::Stmt(stmt.clone()),
                    stmt.location.clone()
                ))
            }
            _ => unimplemented!(),
        }
    }

    pub fn check(ast: Program) -> Return<TProgram> {
        let mut typeck = Typeck::new();

        let mut thir = Vec::new();
        for stmt in ast {
            let stmt = typeck.check_toplevel_stmt(&stmt)?;
            thir.push(stmt);
        }

        Ok(thir)
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
    fn redefinition_of_local() {
        let mut typeck = Typeck::new();
        let stmt = spanned!(
            Stmt::Local {
                name: "x".to_string(),
                ty: None,
                value: None,
            },
            SourceLoc::default()
        );

        typeck.check_stmt(&stmt).unwrap();
        let result = typeck.check_stmt(&stmt);

        assert!(result.is_err());
    }

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
