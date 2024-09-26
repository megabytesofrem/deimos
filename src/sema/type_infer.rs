//! Implements the type system typer

use std::collections::HashMap;

use crate::spanned::{spanned, Spanned};
use crate::syntax::ast::{Expr, Literal};
use crate::syntax::ast_types::{FunctionInfo, SizedNumber, StructureInfo, StructureKind, Ty};
use crate::syntax::lexer::{Op, SourceLoc};

use super::sema_error::SemanticError;
use super::typecheck::{Return, Typechecker};

// The inferer is seperate and it's job is to evaluate and assign types to expressions
// and statements.
//
// All functions take an unchecked Expr or Stmt and return a type

// The substitution environment represents a mapping of type variables and concrete types.
pub type SubstitutionEnv = HashMap<usize, Ty>;

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

    fn stringify_type(&self, ty: &Ty) -> String {
        match ty {
            Ty::Number(SizedNumber::I16) => "i16".to_string(),
            Ty::Number(SizedNumber::I32) => "i32".to_string(),
            Ty::Number(SizedNumber::I64) => "i64".to_string(),
            Ty::Number(SizedNumber::U16) => "u16".to_string(),
            Ty::Number(SizedNumber::U32) => "u32".to_string(),
            Ty::Number(SizedNumber::U64) => "u64".to_string(),
            Ty::Number(SizedNumber::F32) => "f32".to_string(),
            Ty::Number(SizedNumber::F64) => "f64".to_string(),
            Ty::Bool => "b".to_string(),
            Ty::Char => "c".to_string(),
            Ty::String => "s".to_string(),
            Ty::Void => "v".to_string(),

            _ => "_".to_string(),
        }
    }

    fn resolve_generic(&self, ty: &Ty) -> Return<Ty> {
        match ty {
            Ty::TVar(tv) => {
                if let Some(bound_ty) = self.subst.borrow().get(tv) {
                    Ok(bound_ty.clone())
                } else {
                    // If the type variable is unbound, return an Error
                    todo!()
                }
            }

            // Recurse to resolve complex types
            Ty::Array(inner) => Ok(Ty::Array(Box::new(self.resolve_generic(inner)?))),
            Ty::Pointer(inner) => Ok(Ty::Pointer(Box::new(self.resolve_generic(inner)?))),
            Ty::Optional(inner) => Ok(Ty::Optional(Box::new(self.resolve_generic(inner)?))),
            _ => Ok(ty.clone()),
        }
    }

    fn specialize_function(&self, func: &FunctionInfo) -> Return<Ty> {
        // Generate a unique bound name
        let typename = self.stringify_type(&func.return_ty);

        let unique_name = format!("{}_{}", func.name, typename);

        let concrete_params = func
            .params
            .iter()
            .map(|(name, ty)| {
                let concrete_ty = self.resolve_generic(ty)?;
                Ok((name.clone(), concrete_ty))
            })
            .collect::<Return<Vec<_>>>()?;

        let concrete_return = self.resolve_generic(&func.return_ty)?;

        let specialized_func = FunctionInfo {
            name: unique_name,
            params: concrete_params,
            return_ty: concrete_return,
            body: func.body.clone(),
        };

        Ok(Ty::Function(Box::new(specialized_func)))
    }

    // TODO: As part of the type checker pass we are going to resolve/unify any generic types to a concrete type
    // We need to know their context to do this, so it will be a task for later

    // Unification and substitution below
    fn contains_typevar(&self, ty: &Ty, tv: usize) -> bool {
        match ty {
            // Check if tv matches the other type variable (tv2)
            Ty::TVar(tv2) => tv == *tv2,

            // Recurse to check if the type variable is contained in the type
            Ty::Function(f) => {
                f.params.iter().any(|(_, ty)| self.contains_typevar(ty, tv))
                    || self.contains_typevar(&f.return_ty, tv)
            }
            Ty::Pointer(ty) | Ty::Array(ty) | Ty::Optional(ty) => self.contains_typevar(ty, tv),
            Ty::Struct(s) | Ty::Enum(s) => {
                s.fields.iter().any(|(_, ty)| self.contains_typevar(ty, tv))
            }

            _ => false,
        }
    }

    fn unify_typevar(&self, ty: &Ty, tv: usize, location: &SourceLoc) -> Return<()> {
        // Check for infinite type expansion (Occurs check)
        if self.contains_typevar(ty, tv) {
            return Err(SemanticError::InfTypeExpansion {
                location: location.clone(),
            });
        }

        // Fetch the type variable from the substitution environment and unify it with the type given
        // If the type variable is not present in the substitution environment, insert it
        if let Some(bound_ty) = self.subst.borrow().get(&tv).cloned() {
            self.unify(ty, &bound_ty, location)
        } else {
            self.subst.borrow_mut().insert(tv, ty.clone());
            Ok(())
        }
    }

    pub fn unify(&self, t1: &Ty, t2: &Ty, location: &SourceLoc) -> Return<()> {
        match (t1, t2) {
            // Unify identical types
            (Ty::Number(ty1), Ty::Number(ty2)) if ty1 == ty2 => Ok(()),
            (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Char, Ty::Char) => Ok(()),
            (Ty::String, Ty::String) => Ok(()),
            (Ty::Unchecked, _) | (_, Ty::Unchecked) => Ok(()),
            _ty_pair if t1 == t2 => Ok(()),

            // Unify type variables
            (Ty::TVar(v1), t) | (t, Ty::TVar(v1)) => self.unify_typevar(t, *v1, location),

            // Recurse to unify pointers and arrays
            (Ty::Pointer(a1), Ty::Pointer(a2)) => self.unify(a1, a2, location),
            (Ty::Array(a1), Ty::Array(a2)) => self.unify(a1, a2, location),
            (Ty::Optional(a1), Ty::Optional(a2)) => self.unify(a1, a2, location),

            // Unify function types
            (Ty::Function(f1), Ty::Function(f2)) => {
                // Check if f1 and f2 contain generics, we need to monomorphize the function
                // if this is the case.
                if f1.has_generics() || f2.has_generics() {
                    let _f1 = self.specialize_function(f1)?;
                    let _f2 = self.specialize_function(f2)?;

                    // Unify the specialized functions together
                    self.unify(&_f1, &_f2, location)?;
                }
                // Existing unification logic for non generic functions
                else {
                    // Unify the return type
                    self.unify(&f1.return_ty, &f2.return_ty, location)?;

                    // Unify the argument types
                    if f1.params.len() != f2.params.len() {
                        return Err(SemanticError::TypeMismatch {
                            expected: t1.clone(),
                            found: t2.clone(),
                            location: location.clone(),
                        });
                    }

                    for (a1, a2) in f1.params.iter().zip(f2.params.iter()) {
                        // Recurse to unify the argument types of both functions
                        self.unify(&a1.1, &a2.1, location)?;
                    }
                }

                Ok(())
            }

            // Unify struct or enum types
            (Ty::Struct(s1), Ty::Struct(s2)) | (Ty::Enum(s1), Ty::Enum(s2)) => {
                if s1.fields.len() != s2.fields.len() {
                    return Err(SemanticError::TypeMismatch {
                        expected: t1.clone(),
                        found: t2.clone(),
                        location: location.clone(),
                    });
                }

                for ((name1, ty1), (name2, ty2)) in s1.fields.iter().zip(s2.fields.iter()) {
                    // Check to make sure the field names match up
                    if name1 != name2 {
                        return Err(SemanticError::TypeMismatch {
                            expected: t1.clone(),
                            found: t2.clone(),
                            location: location.clone(),
                        });
                    }

                    // Recurse to unify the field types of both structs
                    self.unify(ty1, ty2, location)?;
                }

                Ok(())
            }

            // Unify user-defined types
            (Ty::UserDefined(u1), Ty::UserDefined(u2)) if u1 == u2 => Ok(()),

            _ => Err(SemanticError::TypeMismatch {
                expected: t1.clone(),
                found: t2.clone(),

                // No way to get the location information here
                location: location.clone(),
            }),
        }
    }

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

    // NOTE: Internal _cast_expr function used by cast_expr above, use the user-facing one instead.
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
                self.unify(&lit_ty, target_ty, &expr.location)?;
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
            (Ty::Number(SizedNumber::F32), Ty::Number(SizedNumber::F64)) => match lit {
                Literal::Float32(f) => Literal::Float64(*f as f64),
                _ => unreachable!(),
            },
            (Ty::Number(SizedNumber::F64), Ty::Number(SizedNumber::F32)) => match lit {
                Literal::Float64(f) => Literal::Float32(*f as f32),
                _ => unreachable!(),
            },
            (Ty::Number(SizedNumber::F32), Ty::Number(SizedNumber::I32)) => match lit {
                Literal::Float32(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            (Ty::Number(SizedNumber::F64), Ty::Number(SizedNumber::I32)) => match lit {
                Literal::Float64(f) => Literal::Int(*f as i32),
                _ => unreachable!(),
            },
            _ => lit.clone(),
        }
    }

    pub fn infer_literal(&self, lit: &Literal) -> Ty {
        match lit {
            Literal::Int(_) => Ty::Number(SizedNumber::I32),
            Literal::Float32(_) => Ty::Number(SizedNumber::F32),
            Literal::Float64(_) => Ty::Number(SizedNumber::F64),
            Literal::Bool(_) => Ty::Bool,
            Literal::String(_) => Ty::String,
        }
    }

    fn infer_binop_expr(&self, lhs: &Spanned<Expr>, op: &Op, rhs: &Spanned<Expr>) -> Return<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        let location = lhs.location.clone();

        self.unify(&lhs_ty, &rhs_ty, &location)?;
        self.equal(&lhs_ty, &rhs_ty, &location)?;

        match op {
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Mod => {
                if !lhs_ty.is_numeric() && rhs_ty != lhs_ty {
                    return Err(SemanticError::TypeMismatch {
                        expected: lhs_ty.clone(),
                        found: rhs_ty.clone(),
                        location: lhs.location.clone(),
                    });
                }

                Ok(lhs_ty)
            }
            Op::And
            | Op::Or
            | Op::Eq
            | Op::BangEq
            | Op::Less
            | Op::LessEq
            | Op::Greater
            | Op::GreaterEq => {
                self.equal(&lhs_ty, &Ty::Bool, &location)?;
                Ok(Ty::Bool)
            }
            _ => panic!("Unexpected operator {:?}", op),
        }
    }

    pub fn infer_expr(&self, value: &Spanned<Expr>) -> Return<Ty> {
        match &value.target {
            Expr::Literal(lit) => Ok(self.infer_literal(lit)),
            Expr::Ident(name) => self.lookup_name(name, value.location.clone()),
            Expr::Member(member) => {
                self.lookup_member_access(member, member.target.location.clone())
            }
            Expr::BinOp(lhs, op, rhs) => self.infer_binop_expr(lhs, op, rhs),
            Expr::Array(elems) => self.infer_array_literal(elems),
            Expr::Cast(expr, ty) => {
                let expr_ty = self.infer_expr(expr)?;
                self.unify(&expr_ty, ty, &value.location)?;

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
            Expr::Call { callee, args } => match &callee.target {
                Expr::Ident(name) => self.infer_call_expr(name, args),
                Expr::Member(_member) => {
                    todo!("Implement member lookup in infer_expr for function calls");
                    //self.infer_call_expr(&member, args)
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
                // Ensure that the number of arguments matches the number of parameters
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
                    self.unify(&arg_ty, &param_ty.1, &arg.location)?;
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
            self.unify(&ty, &elem_ty, &elem.location)?;

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
                expected: Ty::Number(SizedNumber::I32),
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
