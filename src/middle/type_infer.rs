//! Inference functions for type checking
//! This is split from `typechk.rs` to keep the codebase clean and organized

use crate::middle::typecheck::Typecheck;
use crate::syntax::ast::{Expr, Literal, Numeric, Ty};
use crate::syntax::lexer::{BinOp, SourceLoc};
use crate::utils::{spanned, Spanned};

use super::typecheck::{self, TypeError};
use super::typed_ast::TExpr;

impl<'tc> Typecheck<'tc> {
    // Inference engine functions

    pub fn cast_expr(
        &mut self,
        expr: &Spanned<Expr>,
        target_ty: &Ty,
        just_wrap_in_cast: bool,
    ) -> typecheck::Return<Spanned<Expr>> {
        // just_wrap_in_cast specifies if we should cast down to the type on the AST level,
        // or just wrap the conversion in a cast node.

        match &expr.target {
            Expr::Literal(lit) => {
                let src_ty = self.infer_literal(&lit)?;

                // Check if we can cast from a source type to a target type
                assert!(src_ty.can_cast_into(target_ty));

                if src_ty == *target_ty {
                    return Ok(expr.clone());
                }

                let casted_expr = if just_wrap_in_cast {
                    Expr::Cast(Box::new(expr.clone()), target_ty.clone())
                } else {
                    let literal = self.cast_literal(lit, &src_ty, target_ty);
                    Expr::Literal(literal)
                };

                Ok(spanned(casted_expr, expr.location.clone()))
            }
            Expr::QualifiedName(name) => {
                let src_ty = self.check_variable(name, expr.location.clone())?;

                // Check if we can cast from a source type to a target type
                assert!(src_ty.can_cast_into(target_ty));

                if src_ty == *target_ty {
                    return Ok(expr.clone());
                }

                let casted_expr = if just_wrap_in_cast {
                    Expr::Cast(Box::new(expr.clone()), target_ty.clone())
                } else {
                    Expr::QualifiedName(name.clone())
                };

                Ok(spanned(casted_expr, expr.location.clone()))
            }
            Expr::BinOp(lhs, op, rhs) => {
                let lhs = self.cast_expr(lhs, target_ty, just_wrap_in_cast)?;
                let rhs = self.cast_expr(rhs, target_ty, just_wrap_in_cast)?;

                Ok(spanned(
                    Expr::BinOp(Box::new(lhs), op.clone(), Box::new(rhs)),
                    expr.location.clone(),
                ))
            }
            Expr::UnOp(op, exp) => {
                let expr = self.cast_expr(exp, target_ty, just_wrap_in_cast)?;

                Ok(spanned(
                    Expr::UnOp(op.clone(), Box::new(expr.clone())),
                    expr.location.clone(),
                ))
            }
            Expr::Array(elems) => {
                let mut casted_elems = Vec::new();
                for elem in elems {
                    let casted_elem = self.cast_expr(elem, target_ty, just_wrap_in_cast)?;
                    casted_elems.push(casted_elem);
                }

                Ok(spanned(Expr::Array(casted_elems), expr.location.clone()))
            }
            Expr::Tuple(elems) => {
                let mut casted_elems = Vec::new();
                for elem in elems {
                    let casted_elem = self.cast_expr(elem, target_ty, just_wrap_in_cast)?;
                    casted_elems.push(casted_elem);
                }

                Ok(spanned(Expr::Tuple(casted_elems), expr.location.clone()))
            }
            Expr::StructCons { fields: _ } => {
                // We cant directly cast a struct to another struct, we can only wrap it in a cast node
                if !just_wrap_in_cast {
                    return Err(TypeError::InvalidCast {
                        from: Ty::Unchecked,
                        into: target_ty.clone(),
                        location: expr.location.clone(),
                    });
                } else {
                    Ok(spanned(
                        Expr::Cast(Box::new(expr.clone()), target_ty.clone()),
                        expr.location.clone(),
                    ))
                }
            }

            _ => Err(TypeError::InvalidCast {
                from: Ty::Unchecked,
                into: target_ty.clone(),
                location: expr.location.clone(),
            }),
        }
    }

    fn cast_literal(&self, lit: &Literal, src_ty: &Ty, target_ty: &Ty) -> Literal {
        match (src_ty, target_ty) {
            (Ty::Number(Numeric::F32), Ty::Number(Numeric::F64)) => match lit {
                Literal::Float32(f) => Literal::Float64(*f as f64),
                _ => unreachable!(),
            },
            (Ty::Number(Numeric::F64), Ty::Number(Numeric::F32)) => match lit {
                Literal::Float64(f) => Literal::Float32(*f as f32),
                _ => unreachable!(),
            },
            (Ty::Number(Numeric::F32), Ty::Number(Numeric::I32)) => match lit {
                Literal::Float32(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            (Ty::Number(Numeric::F64), Ty::Number(Numeric::I32)) => match lit {
                Literal::Float64(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            _ => lit.clone(),
        }
    }

    pub fn raw_cast_expr(
        &mut self,
        expr: &Spanned<Expr>,
        target_ty: &Ty,
    ) -> typecheck::Return<Spanned<Expr>> {
        self.cast_expr(expr, target_ty, false)
    }

    pub(crate) fn infer_literal(&mut self, lit: &Literal) -> typecheck::Return<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Number(Numeric::I32)),
            Literal::Float32(_) => Ok(Ty::Number(Numeric::F32)),
            Literal::Float64(_) => Ok(Ty::Number(Numeric::F64)),
            Literal::Bool(_) => Ok(Ty::Bool),
            Literal::String(_) => Ok(Ty::String),
        }
    }

    pub(crate) fn infer_binop(
        &mut self,
        lhs: &Spanned<Expr>,
        op: BinOp,
        rhs: &Spanned<Expr>,
    ) -> typecheck::Return<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        if lhs_ty != rhs_ty {
            return Err(TypeError::TypeMismatch {
                expected: lhs_ty.clone(),
                found: rhs_ty.clone(),
                location: lhs.location.clone(),
            });
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if !lhs_ty.is_numeric() && rhs_ty != lhs_ty {
                    return Err(TypeError::TypeMismatch {
                        expected: lhs_ty.clone(),
                        found: rhs_ty.clone(),
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

    pub(crate) fn infer_function_call(
        &mut self,
        name: String,
        args: &Vec<Spanned<Expr>>,
    ) -> typecheck::Return<Ty> {
        let callee =
            self.get_typing_context()
                .get(&name)
                .ok_or_else(|| TypeError::UndefinedFunction {
                    name: name.clone(),
                    location: SourceLoc::default(),
                })?;

        match callee {
            Ty::Function(return_ty, param_tys) => {
                // Make sure the number of arguments match the number of parameters
                if args.len() != param_tys.len() {
                    return Err(TypeError::ArityMismatch {
                        expected: param_tys.len(),
                        found: args.len(),
                        location: SourceLoc::default(),
                    });
                }

                // Check if the types of the arguments match the types of the parameters
                for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                    let arg_ty = self.infer_expr(arg)?;
                    if arg_ty != *param_ty {
                        return Err(TypeError::TypeMismatch {
                            expected: param_ty.clone(),
                            found: arg_ty.clone(),
                            location: arg.location.clone(),
                        });
                    }
                }

                return Ok(*return_ty);
            }
            _ => {
                return Err(TypeError::TypeMismatch {
                    expected: Ty::Function(Box::new(Ty::Unchecked), Vec::new()),
                    found: callee.clone(),
                    location: SourceLoc::default(),
                });
            }
        }
    }

    pub(crate) fn infer_array(&mut self, elems: &[Spanned<Expr>]) -> typecheck::Return<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Unchecked)));
        }

        // Infer the overall type of the array from the first element
        let elem_ty = self.infer_expr(&elems[0])?;
        elems.iter().skip(1).try_for_each(|elem| {
            let ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                return Err(TypeError::TypeMismatch {
                    expected: elem_ty.clone(),
                    found: ty.clone(),
                    location: elem.location.clone(),
                });
            }
            Ok(())
        })?;

        Ok(Ty::Array(Box::new(elem_ty)))
    }

    pub(crate) fn infer_tuple(&mut self, elems: &[Spanned<Expr>]) -> typecheck::Return<Ty> {
        let mut tys = Vec::new();
        for elem in elems {
            tys.push(self.infer_expr(elem)?);
        }

        Ok(Ty::Tuple(tys))
    }

    pub(crate) fn infer_arraylike_index(
        &mut self,
        indexable: &Spanned<Expr>,
        index: &Spanned<Expr>,
    ) -> typecheck::Return<Ty> {
        let indexable_ty = self.infer_expr(indexable)?;
        let index_ty = self.infer_expr(index)?;

        // Check if `index_ty` is a valid index type or not
        if !index_ty.is_index_type() {
            return Err(TypeError::TypeMismatch {
                expected: Ty::Number(Numeric::I32),
                found: index_ty.clone(),
                location: index.location.clone(),
            });
        }

        match indexable_ty {
            Ty::Array(ty) => Ok(*ty),

            _ => {
                return Err(TypeError::TypeMismatch {
                    expected: Ty::Array(Box::new(Ty::Unchecked)),
                    found: indexable_ty.clone(),
                    location: indexable.location.clone(),
                });
            }
        }
    }

    pub(crate) fn infer_expr(&mut self, expr: &Spanned<Expr>) -> typecheck::Return<Ty> {
        match &expr.target {
            Expr::Literal(lit) => self.check_literal(lit),
            Expr::QualifiedName(name) => self.check_variable(name, expr.location.clone()),
            Expr::BinOp(lhs, op, rhs) => self.infer_binop(lhs, op.clone(), rhs),
            Expr::Array(elems) => self.infer_array(elems),
            Expr::Tuple(elems) => self.infer_tuple(elems),
            Expr::Cast(expr, ty) => {
                println!("Cast: {:?} -> {:?}", expr, ty);

                let expr_ty = self.infer_expr(expr)?;
                if expr_ty.can_cast_into(ty) {
                    println!(
                        "Cast is valid, inferred type: {:?} and target type: {:?}",
                        expr_ty, ty
                    );
                    Ok(ty.clone())
                } else {
                    Err(TypeError::InvalidCast {
                        from: expr_ty,
                        into: ty.clone(),
                        location: expr.location.clone(),
                    })
                }
            }
            Expr::ArrayIndex { array, index } => self.infer_arraylike_index(array, index),
            Expr::Call { callee, args } => match &callee.target {
                Expr::QualifiedName(name) => self.infer_function_call(name.clone(), args),

                // Cannot call functions on anything other than variables
                _ => unimplemented!(),
            },
            e => todo!("{:?}", e),
        }
    }
}
