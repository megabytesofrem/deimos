//! Implements the type system typer

use crate::syntax::ast::{Expr, Literal};
use crate::syntax::lexer::{BinOp, SourceLoc};
use crate::syntax::types::{FunctionInfo, Sized, StructureInfo, StructureKind, Ty};
use crate::utils::{spanned, Spanned};

use super::sema_error::SemanticError;
use super::typecheck::{Return, Typechecker};

// The inferer is seperate and it's job is to evaluate and assign types to expressions
// and statements.
//
// All functions take an unchecked Expr or Stmt and return a type

impl<'t> Typechecker {
    /// Returns Ok if the types are the same based on equality, otherwise an error
    pub fn equal(&self, t1: &Ty, t2: &Ty, location: &SourceLoc) -> Return<()> {
        if t1 != t2 {
            return Err(SemanticError::TypeMismatch {
                expected: t1.clone(),
                found: t2.clone(),
                location: location.clone(),
            });
        }

        Ok(())
    }

    // TODO: As part of the type checker pass we are going to resolve/unify any generic types to a concrete type
    // We need to know their context to do this, so it will be a task for later

    pub fn check_equal(&self, t1: &Ty, t2: &Ty, location: &SourceLoc) -> bool {
        self.equal(t1, t2, location).is_ok()
    }

    fn can_perform_cast(&self, t1: &Ty, t2: &Ty, location: &SourceLoc) -> Return<()> {
        match (t1, t2) {
            (_, _other) if self.equal(t1, t2, location).is_ok() => Ok(()),
            (Ty::Number(_), Ty::Number(_)) => Ok(()),
            (Ty::Pointer(_), Ty::Pointer(_)) => Ok(()),
            (Ty::Array(_), Ty::Array(_)) => Ok(()),
            (Ty::UserDefined(_), Ty::UserDefined(_)) => Ok(()),
            (_, Ty::Unchecked) | (Ty::Unchecked, _) => Ok(()),
            (_, Ty::Void) | (Ty::Void, _) => Ok(()),

            _ => Err(SemanticError::CannotCast {
                from: t1.clone(),
                to: t2.clone(),
                location: location.clone(),
            }),
        }
    }

    pub fn cast_expr(&mut self, expr: &Spanned<Expr>, target_ty: &Ty) -> Return<Spanned<Expr>> {
        match &expr.target {
            // Special-case struct constructors to wrap the whole expression in a cast node
            Expr::StructCons { fields: _ } => self._cast_expr(expr, target_ty, true),
            _ => self._cast_expr(expr, target_ty, false),
        }
    }

    fn _cast_expr(
        &self,
        expr: &Spanned<Expr>,
        target_ty: &Ty,
        just_wrap_it: bool,
    ) -> Return<Spanned<Expr>> {
        // just_wrap_it specifies if we should cast the expression on the type level or not
        match &expr.target {
            Expr::Literal(lit) => {
                let lit_ty = self.infer_literal(lit);
                self.can_perform_cast(&lit_ty, target_ty, &expr.location)?;

                if lit_ty == *target_ty {
                    return Ok(expr.clone());
                }

                let casted = if just_wrap_it {
                    Expr::Cast(Box::new(expr.clone()), target_ty.clone())
                } else {
                    let literal = self.cast_literal(lit, &lit_ty, target_ty);
                    Expr::Literal(literal)
                };

                Ok(spanned(casted, expr.location.clone()))
            }
            _ => unimplemented!(),
        }
    }

    fn cast_literal(&self, lit: &Literal, src_ty: &Ty, target_ty: &Ty) -> Literal {
        match (src_ty, target_ty) {
            (Ty::Number(Sized::F32), Ty::Number(Sized::F64)) => match lit {
                Literal::Float32(f) => Literal::Float64(*f as f64),
                _ => unreachable!(),
            },
            (Ty::Number(Sized::F64), Ty::Number(Sized::F32)) => match lit {
                Literal::Float64(f) => Literal::Float32(*f as f32),
                _ => unreachable!(),
            },
            (Ty::Number(Sized::F32), Ty::Number(Sized::I32)) => match lit {
                Literal::Float32(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            (Ty::Number(Sized::F64), Ty::Number(Sized::I32)) => match lit {
                Literal::Float64(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            _ => lit.clone(),
        }
    }

    pub fn infer_literal(&self, lit: &Literal) -> Ty {
        match lit {
            Literal::Int(_) => Ty::Number(Sized::I32),
            Literal::Float32(_) => Ty::Number(Sized::F32),
            Literal::Float64(_) => Ty::Number(Sized::F64),
            Literal::Bool(_) => Ty::Bool,
            Literal::String(_) => Ty::String,
        }
    }

    fn infer_binop_expr(&self, lhs: &Spanned<Expr>, op: &BinOp, rhs: &Spanned<Expr>) -> Return<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        let location = lhs.location.clone();
        self.equal(&lhs_ty, &rhs_ty, &location)?;

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if !lhs_ty.is_numeric() && rhs_ty != lhs_ty {
                    return Err(SemanticError::TypeMismatch {
                        expected: lhs_ty.clone(),
                        found: rhs_ty.clone(),
                        location: lhs.location.clone(),
                    });
                }

                Ok(lhs_ty)
            }
            BinOp::And
            | BinOp::Or
            | BinOp::Eq
            | BinOp::BangEq
            | BinOp::Less
            | BinOp::LessEq
            | BinOp::Greater
            | BinOp::GreaterEq => {
                self.equal(&lhs_ty, &Ty::Bool, &location)?;
                Ok(Ty::Bool)
            }

            _ => unimplemented!(),
        }
    }

    pub fn infer_expr(&self, value: &Spanned<Expr>) -> Return<Ty> {
        match &value.target {
            Expr::Literal(lit) => Ok(self.infer_literal(lit)),
            Expr::Member(_name) => {
                todo!("Implement member lookup in infer_expr");
                // self.lookup_name(name, value.location.clone()),
            }
            Expr::BinOp(lhs, op, rhs) => self.infer_binop_expr(lhs, op, rhs),
            Expr::Array(elems) => self.infer_array_literal(elems),
            Expr::Cast(expr, ty) => {
                let expr_ty = self.infer_expr(expr)?;

                // TODO: Add casting logic
                if !self.check_equal(&expr_ty, ty, &value.location) {
                    return Err(SemanticError::CannotCast {
                        from: expr_ty,
                        to: ty.clone(),
                        location: value.location.clone(),
                    });
                }

                Ok(ty.clone())
            }
            Expr::StructCons { fields } => {
                let mut fields_: Vec<(String, Ty)> = Vec::new();
                for (name, expr) in fields {
                    fields_.push((name.clone(), self.infer_expr(expr)?));
                }

                Ok(Ty::Struct(StructureInfo {
                    kind: StructureKind::Struct,
                    name: "_struct".to_string(),
                    fields: fields_,
                }))
            }
            Expr::ArrayIndex { array, index } => self.infer_array_like_index(array, index),
            Expr::Call { callee, args: _ } => match &callee.target {
                Expr::Member(_name) => {
                    todo!("Implement member lookup in infer_expr");
                    // self.infer_call_expr(&name, args)
                }
                _ => panic!("Cannot call a non-function value"),
            },
            expr => unimplemented!("Unimplemented {:?}", expr),
        }
    }

    pub fn infer_call_expr(&self, name: &'t str, args: &Vec<Spanned<Expr>>) -> Return<Ty> {
        let callee = {
            let binding = self.resolver.borrow();
            binding
                .resolve_name(name)
                .ok_or_else(|| SemanticError::NotInScope {
                    name: name.to_string(),
                    location: Default::default(),
                })?
                .clone()
        };

        match callee {
            Ty::Function(info) => {
                if args.len() != info.params.len() {
                    return Err(SemanticError::InvalidArity {
                        expected: info.params.len(),
                        found: args.len(),
                        location: Default::default(),
                    });
                }

                // Ensure that all the arguments match the expected types
                for (arg, param_ty) in args.iter().zip(info.params.iter()) {
                    let arg_ty = self.infer_expr(arg)?;
                    if arg_ty != param_ty.1 {
                        return Err(SemanticError::TypeMismatch {
                            expected: param_ty.1.clone(),
                            found: arg_ty,
                            location: arg.location.clone(),
                        });
                    }
                }

                Ok(info.return_ty.clone())
            }
            _ => Err(SemanticError::TypeMismatch {
                expected: Ty::Function(Box::new(FunctionInfo::named_function(
                    "_function".to_string(),
                    Vec::new(),
                    Ty::Unchecked,
                ))),
                found: callee.clone(),
                location: Default::default(),
            }),
        }
    }

    pub fn infer_array_literal(&self, elems: &[Spanned<Expr>]) -> Return<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Unchecked)));
        }

        // Infer the overall type of the array from the first element
        let ty = self.infer_expr(&elems[0])?;
        elems.iter().skip(1).try_for_each(|elem| {
            let elem_ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                return Err(SemanticError::TypeMismatch {
                    expected: elem_ty,
                    found: ty.clone(),
                    location: elem.location.clone(),
                });
            }

            Ok(())
        })?;

        Ok(Ty::Array(Box::new(ty)))
    }

    pub fn infer_array_like_index(
        &self,
        value: &Spanned<Expr>,
        index: &Spanned<Expr>,
    ) -> Return<Ty> {
        let value_ty = self.infer_expr(value)?;
        let index_ty = self.infer_expr(index)?;

        if !index_ty.is_index_type() {
            return Err(SemanticError::TypeMismatch {
                expected: Ty::Number(Sized::I32),
                found: index_ty,
                location: index.location.clone(),
            });
        }

        match value_ty {
            Ty::Array(ty) => Ok(*ty),
            _ => Err(SemanticError::TypeMismatch {
                expected: Ty::Array(Box::new(Ty::Unchecked)),
                found: value_ty,
                location: value.location.clone(),
            }),
        }
    }
}
