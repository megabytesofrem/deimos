use std::collections::HashMap;

use thiserror::Error;

use crate::syntax::lexer::SourceLoc;
use crate::syntax::{ast::*, lexer::BinOp};

use super::ty_ast::{TyBlock, TyExpr, TyProgram, TyStmt, TyTopLevelStmt};

/// Result of typechecking
type TypeckResult<T> = Result<T, TypeckError>;

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

#[derive(Debug, Clone)]
pub struct Typeck {
    // Hashmap of scopes which each hold locals
    scopes: Vec<HashMap<String, Ty>>,
}

impl Typeck {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    // Name resolution and scope management
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn push_local(&mut self, name: String, ty: Ty) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    fn get_local(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    // Typechecker logic

    fn mk_typed(&self, ty: Ty, expr: Expr) -> TyExpr {
        TyExpr { ty, expr }
    }

    fn check_variable(&self, id: &str) -> TypeckResult<Ty> {
        if let Some(ty) = self.get_local(id) {
            Ok(ty)
        } else {
            Err(TypeckError::UndefinedLocal {
                name: id.to_string(),
                location: SourceLoc::default(),
            })
        }
    }

    fn check_block(&mut self, blocks: &Vec<Stmt>) -> TypeckResult<TyBlock> {
        self.push_scope();
        let mut ty_block = Vec::new();
        for stmt in blocks.iter() {
            ty_block.push(self.check_stmt(stmt)?);
        }
        self.scopes.pop();
        Ok(ty_block)
    }

    pub fn check_expr(&mut self, expr: &Expr) -> TypeckResult<TyExpr> {
        // Type check infered type with expected type
        let infer_ty = self.infer_expr(expr)?;

        match expr {
            Expr::Int(..) | Expr::Float(..) | Expr::Bool(..) | Expr::String(..) => {
                return Ok(self.mk_typed(infer_ty, expr.clone()))
            }
            Expr::Variable(id) => {
                let ty = self.check_variable(id)?;
                if ty != infer_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: ty,
                        found: infer_ty,
                        location: SourceLoc::default(),
                    });
                }
            }
            Expr::Array(elems) => {
                let ty = self.infer_array(elems)?;
                if ty != infer_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: ty,
                        found: infer_ty,
                        location: SourceLoc::default(),
                    });
                }
            }
            Expr::BinOp(lhs, op, rhs) => {
                let lhs_ty = self.check_expr(lhs)?.ty;
                let rhs_ty = self.check_expr(rhs)?.ty;
                if lhs_ty != rhs_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: lhs_ty,
                        found: rhs_ty,
                        location: SourceLoc::default(),
                    });
                }
            }
            Expr::UnOp(_, expr) => {
                let ty = self.check_expr(expr)?.ty;
                if ty != infer_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: ty,
                        found: infer_ty,
                        location: SourceLoc::default(),
                    });
                }
            }
            Expr::ArrayIndex { array, index } => {
                let array_ty = self.check_expr(array)?.ty;
                let index_ty = self.check_expr(index)?.ty;
                if array_ty != Ty::Array(Box::new(index_ty.clone())) {
                    return Err(TypeckError::TypeMismatch {
                        expected: Ty::Array(Box::new(index_ty)),
                        found: array_ty,
                        location: SourceLoc::default(),
                    });
                }
            }
            _ => todo!("owo"),
        }

        // Just wrap the expression along with its type
        Ok(self.mk_typed(infer_ty, expr.clone()))
    }

    pub fn check_stmt(&mut self, stmt: &Stmt) -> TypeckResult<TyStmt> {
        // Typecheck statements and return a TyStmt

        match stmt {
            Stmt::Expr(expr) => {
                let tyexp = self.check_expr(expr)?;
                Ok(TyStmt::Expr(tyexp))
            }
            Stmt::Return(expr) => {
                let ty = expr
                    .as_ref()
                    .map(|expr| self.check_expr(expr))
                    .transpose()?;

                Ok(TyStmt::Return(ty.map(|tyexp| TyExpr {
                    ty: tyexp.ty,
                    expr: expr.clone().unwrap(),
                })))
            }
            Stmt::Local { name, ty, value } => {
                let ty = ty.clone().unwrap_or(Ty::Any);
                let value = value.as_ref().map(|expr| {
                    self.check_expr(&expr).map(|tyexp| TyExpr {
                        ty: tyexp.ty,
                        expr: expr.clone(),
                    })
                });

                self.push_local(name.clone(), ty.clone());
                Ok(TyStmt::Local {
                    name: name.clone(),
                    ty: Some(ty),
                    value: value.transpose()?,
                })
            }
            Stmt::Assign { target, value } => {
                let value_ty = self.check_expr(value)?.ty;
                let target_ty = self.check_expr(target)?.ty;

                if target_ty != value_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: target_ty,
                        found: value_ty,
                        location: SourceLoc::default(),
                    });
                }

                Ok(TyStmt::Assign {
                    target: target.clone(),
                    value: TyExpr {
                        ty: value_ty,
                        expr: value.clone(),
                    },
                })
            }
            Stmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_ty = self.check_expr(cond)?.ty;
                if cond_ty != Ty::Bool {
                    return Err(TypeckError::TypeMismatch {
                        expected: Ty::Bool,
                        found: cond_ty,
                        location: SourceLoc::default(),
                    });
                }

                let then_block = self.check_block(then_block)?;
                let else_block = else_block
                    .as_ref()
                    .map(|block| self.check_block(block))
                    .transpose()?;

                Ok(TyStmt::If {
                    cond: TyExpr {
                        ty: Ty::Bool,
                        expr: cond.clone(),
                    },
                    then_block,
                    else_block,
                })
            }
            Stmt::For {
                init,
                from,
                to,
                body,
            } => {
                let from_ty = self.check_expr(from)?.ty;
                let to_ty = self.check_expr(to)?.ty;

                if from_ty != to_ty {
                    return Err(TypeckError::TypeMismatch {
                        expected: from_ty,
                        found: to_ty,
                        location: SourceLoc::default(),
                    });
                }

                self.push_local(init.clone(), from_ty.clone());
                let body = self.check_block(body)?;

                Ok(TyStmt::For {
                    init: init.clone(),
                    from: TyExpr {
                        ty: from_ty,
                        expr: from.clone(),
                    },
                    to: TyExpr {
                        ty: to_ty,
                        expr: to.clone(),
                    },
                    body,
                })
            }
            Stmt::While { cond, block } => {
                let cond_ty = self.check_expr(cond)?.ty;
                if cond_ty != Ty::Bool {
                    return Err(TypeckError::TypeMismatch {
                        expected: Ty::Bool,
                        found: cond_ty,
                        location: SourceLoc::default(),
                    });
                }

                let block = self.check_block(block)?;

                Ok(TyStmt::While {
                    cond: TyExpr {
                        ty: Ty::Bool,
                        expr: cond.clone(),
                    },
                    block,
                })
            }
            _ => todo!(),
        }
    }

    pub fn check_program(&mut self, ast: Program) -> TypeckResult<TyProgram> {
        let mut program = Vec::new();

        for stmt in ast {
            match stmt.clone() {
                ToplevelStmt::Stmt(stmt) => {
                    program.push(TyTopLevelStmt::Stmt(self.check_stmt(&stmt)?))
                }
                ToplevelStmt::FunctionDecl {
                    name,
                    params,
                    body,
                    return_ty,
                } => {
                    self.push_scope();
                    for (name, ty) in params {
                        self.push_local(name, ty);
                    }

                    let body = self.check_block(&body)?;
                    program.push(TyTopLevelStmt::ToplevelStmt(stmt));
                    self.scopes.pop();
                }
                _ => todo!(),
            }
        }

        Ok(program)
    }

    fn infer_binop(&mut self, lhs: &Expr, op: &BinOp, rhs: &Expr) -> TypeckResult<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        // This must always hold true to infer binary operations
        if lhs_ty != rhs_ty {
            return Err(TypeckError::TypeMismatch {
                expected: lhs_ty,
                found: rhs_ty,
                location: SourceLoc::default(),
            });
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if lhs_ty != Ty::Int && lhs_ty != Ty::Float {
                    return Err(TypeckError::TypeMismatch {
                        expected: Ty::Int,
                        found: lhs_ty,
                        location: SourceLoc::default(),
                    });
                }
                Ok(lhs_ty)
            }
            BinOp::Eq | BinOp::BangEq => Ok(Ty::Bool),
            BinOp::Less | BinOp::LessEq | BinOp::Greater | BinOp::GreaterEq => Ok(Ty::Bool),
            _ => todo!(),
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> TypeckResult<Ty> {
        match expr {
            Expr::Int(..) => Ok(Ty::Int),
            Expr::Float(..) => Ok(Ty::Float),
            Expr::Bool(..) => Ok(Ty::Bool),
            Expr::String(..) => Ok(Ty::String),
            Expr::Variable(id) => Ok(Ty::UserDefined(id.to_string())),
            Expr::Array(elems) => self.infer_array(elems),
            Expr::BinOp(lhs, op, rhs) => self.infer_binop(lhs, op, rhs),
            _ => todo!(),
        }
    }

    fn infer_array(&mut self, elems: &[Expr]) -> TypeckResult<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Any)));
        }

        let location = SourceLoc::default();
        let elem_ty = self.infer_expr(&elems[0])?;
        for elem in elems.iter().skip(1) {
            let ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                return Err(TypeckError::TypeMismatch {
                    expected: elem_ty,
                    found: ty,
                    location: location.clone(),
                });
            }
        }

        Ok(Ty::Array(Box::new(elem_ty)))
    }
}

#[cfg(test)]
pub mod typeck_tests {
    use super::*;

    #[test]
    fn typeck_local() {
        let mut typeck = Typeck::new();

        let stmt = Stmt::Local {
            name: "x".to_string(),
            ty: Some(Ty::Int),
            value: Some(Expr::Int(42)),
        };

        let decl = typeck.check_stmt(&stmt);
        let local = typeck.get_local("x");

        assert!(decl.is_ok() && local.is_some());
    }

    #[test]
    fn typeck_expr_with_int_float() {
        let mut typeck = Typeck::new();
        let expr = Expr::BinOp(
            Box::new(Expr::Int(42)),
            BinOp::Add,
            Box::new(Expr::Float(14.0)),
        );
        let typed = typeck.check_expr(&expr);

        // cannot add int and float
        assert!(typed.is_err());
    }

    #[test]
    fn typeck_comparison_between() {
        let mut typeck = Typeck::new();
        let expr = Expr::BinOp(
            Box::new(Expr::Int(42)),
            BinOp::Less,
            Box::new(Expr::Int(14)),
        );
        let typed = typeck.check_expr(&expr);
        assert!(typed.is_ok());
    }

    #[test]
    fn typeck_array_holds_compatible_types() {
        let mut typeck = Typeck::new();
        let expr = Expr::Array(vec![Expr::Int(42), Expr::String("Hi".to_string())]);
        let typed = typeck.check_expr(&expr);

        // cannot have array of int and string
        assert!(typed.is_err());
    }
}
