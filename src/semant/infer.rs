//! Inference functions for type checking
//! This is split from `typechk.rs` to keep the codebase clean and organized

use crate::bubble_err;
use crate::semant::typechk::Typeck;
use crate::syntax::lexer::{BinOp, SourceLoc};
use crate::syntax::{
    ast::{Expr, Literal, Numeric, Ty},
    span::Spanned,
};

use super::typechk::{self, TypeError};

impl<'cx> Typeck<'cx> {
    // Inference functions

    pub(crate) fn infer_literal(&mut self, lit: &Literal) -> typechk::Return<Ty> {
        match lit {
            Literal::Int(_) => Ok(Ty::Numeric(Numeric::I32)),
            Literal::Float(_) => Ok(Ty::Numeric(Numeric::F32)),
            Literal::Double(_) => Ok(Ty::Numeric(Numeric::F64)),
            Literal::Bool(_) => Ok(Ty::Bool),
            Literal::String(_) => Ok(Ty::String),
        }
    }

    pub(crate) fn infer_binop(
        &mut self,
        lhs: &Spanned<Expr>,
        op: BinOp,
        rhs: &Spanned<Expr>,
    ) -> typechk::Return<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        if lhs_ty != rhs_ty {
            bubble_err!(
                self,
                TypeError::TypeMismatch {
                    expected: lhs_ty.clone(),
                    found: rhs_ty.clone(),
                    location: lhs.location.clone(),
                }
            );
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if !lhs_ty.is_numeric() && rhs_ty != lhs_ty {
                    bubble_err!(
                        self,
                        TypeError::TypeMismatch {
                            expected: lhs_ty.clone(),
                            found: rhs_ty.clone(),
                            location: lhs.location.clone(),
                        }
                    );
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
    ) -> typechk::Return<Ty> {
        let callee =
            self.get_context()
                .get(&name)
                .cloned()
                .ok_or_else(|| TypeError::UndefinedFunction {
                    name: name.clone(),
                    location: SourceLoc::default(),
                })?;

        match callee {
            Ty::Function(return_ty, param_tys) => {
                // Make sure the number of arguments match the number of parameters
                if args.len() != param_tys.len() {
                    bubble_err!(
                        self,
                        TypeError::ArityMismatch {
                            expected: param_tys.len(),
                            found: args.len(),
                            location: SourceLoc::default(),
                        }
                    );
                }

                // Check if the types of the arguments match the types of the parameters
                for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                    let arg_ty = self.infer_expr(arg)?;
                    if arg_ty != *param_ty {
                        bubble_err!(
                            self,
                            TypeError::TypeMismatch {
                                expected: param_ty.clone(),
                                found: arg_ty.clone(),
                                location: arg.location.clone(),
                            }
                        );
                    }
                }

                return Ok(*return_ty);
            }
            _ => {
                bubble_err!(
                    self,
                    TypeError::TypeMismatch {
                        expected: Ty::Function(Box::new(Ty::Unknown), Vec::new()),
                        found: callee.clone(),
                        location: SourceLoc::default(),
                    }
                );
            }
        }
    }

    pub(crate) fn infer_array(&mut self, elems: &[Spanned<Expr>]) -> typechk::Return<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Unknown)));
        }

        // Infer the overall type of the array from the first element
        let elem_ty = self.infer_expr(&elems[0])?;
        elems.iter().skip(1).try_for_each(|elem| {
            let ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                bubble_err!(
                    self,
                    TypeError::TypeMismatch {
                        expected: elem_ty.clone(),
                        found: ty.clone(),
                        location: elem.location.clone(),
                    }
                );
            }
            Ok(())
        })?;

        Ok(Ty::Array(Box::new(elem_ty)))
    }

    pub(crate) fn infer_tuple(&mut self, elems: &[Spanned<Expr>]) -> typechk::Return<Ty> {
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
    ) -> typechk::Return<Ty> {
        let indexable_ty = self.infer_expr(indexable)?;
        let index_ty = self.infer_expr(index)?;

        // Check if `index_ty` is a valid index type or not
        if !index_ty.is_index_type() {
            bubble_err!(
                self,
                TypeError::TypeMismatch {
                    expected: Ty::Numeric(Numeric::I32),
                    found: index_ty.clone(),
                    location: index.location.clone(),
                }
            );
        }

        match indexable_ty {
            Ty::Array(ty) => Ok(*ty),

            #[rustfmt::skip]
        _ => {
            bubble_err!(self, TypeError::TypeMismatch {
                expected: Ty::Array(Box::new(Ty::Unknown)),
                found: indexable_ty.clone(),
                location: indexable.location.clone(),
            });
        }
        }
    }

    pub(crate) fn infer_expr(&mut self, expr: &Spanned<Expr>) -> typechk::Return<Ty> {
        match &expr.target {
            Expr::Literal(lit) => self.check_literal(lit),
            Expr::Variable(name) => self.check_variable(name, expr.location.clone()),
            Expr::BinOp(lhs, op, rhs) => self.infer_binop(lhs, op.clone(), rhs),
            Expr::Array(elems) => self.infer_array(elems),
            Expr::Tuple(elems) => self.infer_tuple(elems),
            Expr::ArrayIndex { array, index } => self.infer_arraylike_index(array, index),
            Expr::Call { callee, args } => match &callee.target {
                Expr::Variable(name) => self.infer_function_call(name.clone(), args),

                // Cannot call functions on anything other than variables
                _ => unimplemented!(),
            },
            _ => todo!(),
        }
    }
}
