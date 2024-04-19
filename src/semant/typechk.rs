//! Type checker pass for the AST

use thiserror::Error;

use crate::syntax::{
    ast::{Ast, Block, Expr, Literal, Stmt, ToplevelStmt, Ty},
    lexer::{BinOp, SourceLoc},
    span::Spanned,
};
use crate::syntax::ast::Numeric;

use super::scope::ScopeStack;
use super::typed_ast::*;

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("{location:?}: Type mismatch between '{expected:?}' and '{found:?}'")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location:?}: Redefinition of variable or type '{name}'")]
    Redefinition { name: String, location: SourceLoc },

    #[error("{location:?}: Undefined local '{name}' being used before declaration")]
    UndefinedLocal { name: String, location: SourceLoc },

    #[error("{location:?}: Undefined function '{name}' being used before declaration")]
    UndefinedFunction { name: String, location: SourceLoc },

    #[error("{location:?}: Arity mismatch between '{expected}' and '{found}'")]
    ArityMismatch {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    #[error("{location:?}: Return type mismatch between '{expected:?}' and '{found:?}'")]
    ReturnTypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },
}

// Holds the type that is returned from the type checker
pub(crate) type Return<T> = Result<T, TypeError>;
pub(crate) type ReturnMany<'cx, T> = anyhow::Result<T, Vec<TypeError>>;


// Macro to return an error while also pushing it to the error list,
// so we can bubble them up instead of early returning. We do need to `clone` everything when using
// the macro though.
#[macro_export]
macro_rules! bubble_err {
    ($self:expr, $err:expr) => {
        let err = $err.clone();
        $self.errors.push(err);
        return Err($err);
    };
}

// Type checker pass
#[derive(Debug, Clone)]
pub struct Typeck<'cx> {
    ctx: ScopeStack,
    marker: std::marker::PhantomData<&'cx ()>,

    errors: Vec<TypeError>,
}

impl<'cx> Typeck<'cx> {
    pub fn new() -> Self {
        Typeck {
            ctx: ScopeStack::new(),
            marker: std::marker::PhantomData,
            errors: Vec::new(),
        }
    }

    // Inference functions

    fn infer_binop(&mut self, lhs: &Spanned<Expr>, op: BinOp, rhs: &Spanned<Expr>) -> Return<Ty> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;

        if lhs_ty != rhs_ty {
            bubble_err!(self, TypeError::TypeMismatch {
                expected: lhs_ty.clone(),
                found: rhs_ty.clone(),
                location: lhs.location.clone(),
            });
        }

        match op {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if !lhs_ty.is_numeric() && rhs_ty != lhs_ty {
                    bubble_err!(self, TypeError::TypeMismatch {
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

    fn infer_array(&mut self, elems: &[Spanned<Expr>]) -> Return<Ty> {
        if elems.is_empty() {
            return Ok(Ty::Array(Box::new(Ty::Unknown)));
        }

        // Infer the overall type of the array from the first element
        let elem_ty = self.infer_expr(&elems[0])?;
        elems.iter().skip(1).try_for_each(|elem| {
            let ty = self.infer_expr(elem)?;
            if ty != elem_ty {
                bubble_err!(self, TypeError::TypeMismatch {
                    expected: elem_ty.clone(),
                    found: ty.clone(),
                    location: elem.location.clone(),
                });
            }
            Ok(())
        })?;

        Ok(Ty::Array(Box::new(elem_ty)))
    }

    fn infer_tuple(&mut self, elems: &[Spanned<Expr>]) -> Return<Ty> {
        let mut tys = Vec::new();
        for elem in elems {
            tys.push(self.infer_expr(elem)?);
        }

        Ok(Ty::Tuple(tys))
    }

    fn infer_arraylike_index(
        &mut self,
        indexable: &Spanned<Expr>,
        index: &Spanned<Expr>,
    ) -> Return<Ty> {
        let indexable_ty = self.infer_expr(indexable)?;
        let index_ty = self.infer_expr(index)?;

        // Check if `index_ty` is a valid index type or not
        if !index_ty.is_index_type() {
            bubble_err!(self, TypeError::TypeMismatch {
                expected: Ty::Numeric(Numeric::I32),
                found: index_ty.clone(),
                location: index.location.clone(),
            });
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


    fn infer_expr(&mut self, expr: &Spanned<Expr>) -> Return<Ty> {
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
    // NOTE: We operate on the AST wrapped in Spanned<T>

    fn check_variable(&self, name: &str, location: SourceLoc) -> Return<Ty> {
        self.ctx
            .get(name)
            .cloned()
            .ok_or_else(|| TypeError::UndefinedLocal {
                name: name.to_string(),
                location,
            })
    }

    fn check_literal(&self, literal: &Literal) -> Return<Ty> {
        match literal {
            Literal::Int(_) => Ok(Ty::Numeric(Numeric::I32)),
            Literal::Float(_) => Ok(Ty::Numeric(Numeric::F32)),
            Literal::Double(_) => Ok(Ty::Numeric(Numeric::F64)),
            Literal::Bool(_) => Ok(Ty::Bool),
            Literal::String(_) => Ok(Ty::String),
        }
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Return<Spanned<TExpr>> {
        let ty = self.infer_expr(expr)?;

        expr.clone().flat_map_spanned(|target| {
            match target {
                Expr::Literal(lit) => {
                    return Ok(TExpr::Literal(lit.clone(), ty.clone()));
                }
                Expr::Variable(name) => {
                    let ty2 = self.check_variable(&name, expr.location.clone())?;
                    if ty != ty2 {
                        bubble_err!(self, TypeError::TypeMismatch {
                            expected: ty.clone(),
                            found: ty2.clone(),
                            location: expr.location.clone(),
                        });
                    } else {
                        // Variable is valid
                        return Ok(TExpr::Variable(name.clone(), ty.clone()));
                    }
                }
                Expr::BinOp(lhs, op, rhs) => {
                    return Ok(TExpr::BinOp(
                        Box::new(self.check_expr(&lhs)?),
                        op.clone(),
                        Box::new(self.check_expr(&rhs)?),
                    ));
                }
                Expr::UnOp(op, expr) => {
                    let expr = self.check_expr(&expr)?;
                    return Ok(TExpr::UnOp(op.clone(), Box::new(expr.clone())));
                }
                Expr::Array(elems) | Expr::Tuple(elems) => {
                    let mut array = Vec::new();
                    for elem in elems {
                        array.push(self.check_expr(&elem)?);
                    }

                    return Ok(TExpr::Array { elems: array });
                }
                Expr::ArrayIndex { array, index } => {
                    let array = self.check_expr(&array)?;
                    let index = self.check_expr(&index)?;
                    return Ok(TExpr::ArrayIndex {
                        array: Box::new(array),
                        index: Box::new(index),
                    });
                }
                Expr::Call { callee, args } => {
                    let callee = self.check_expr(expr)?;
                    let mut args_ = Vec::new();
                    for arg in args {
                        args_.push(self.check_expr(&arg)?);
                    }

                    return Ok(TExpr::Call {
                        callee: Box::new(callee),
                        args: args_,
                    });
                }
                _ => unimplemented!(),
            }
        })
    }

    fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Return<Spanned<TStmt>> {
        stmt.clone().flat_map_spanned(|target| {
            match target {
                Stmt::Expr(expr) => {
                    let expr = self.check_expr(&expr)?;
                    Ok(TStmt::Expr(expr.target))
                }
                Stmt::Return(expr) => {
                    if let Some(expr) = expr {
                        let expr = self.check_expr(&expr)?;
                        return Ok(TStmt::Return(Some(expr)));
                    }

                    Ok(TStmt::Return(None))
                }
                Stmt::Let { name, ty, value } => {
                    let ty = ty.clone().unwrap_or(Ty::Unknown);
                    let value = value.as_ref().map(|v| self.check_expr(v));

                    // Check if the local is not already defined
                    if self.ctx.get(&name).is_some() {
                        bubble_err!(self, TypeError::Redefinition {
                            name: name.clone(),
                            location: stmt.location.clone(),
                        });
                    }

                    self.ctx.insert(name.clone(), ty.clone());
                    Ok(TStmt::VarDecl {
                        name: name.clone(),
                        ty: Some(ty),
                        value: value.map(|v| v.unwrap()),
                    })
                }
                Stmt::Assign { target, value } => {
                    let target_ty = self.infer_expr(&target)?;
                    let value_ty = self.infer_expr(&value)?;

                    if target_ty != value_ty {
                        bubble_err!(self, TypeError::TypeMismatch {
                            expected: target_ty.clone(),
                            found: value_ty.clone(),
                            location: value.location.clone(),
                        });
                    }

                    Ok(TStmt::Assign {
                        target: self.check_expr(&target)?.target,
                        value: self.check_expr(&value)?,
                    })
                }
                _ => unimplemented!(),
            }
        })
    }

    fn check_block(&mut self, block: &Block) -> Return<Vec<Spanned<TStmt>>> {
        let block_ = block
            .iter()
            .map(|stmt| self.check_stmt(stmt))
            .collect::<Return<Vec<Spanned<TStmt>>>>()?;

        Ok(block_)
    }

    fn check_struct_declare(
        &mut self,
        name: &str,
        fields: &[(String, Ty)],
    ) -> Return<TToplevelStmt> {
        // Check if the struct is not already defined
        if self.ctx.get(name).is_some() {
            bubble_err!(self, TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the struct into the context
        //self.ctx.insert(name.to_string(), Ty::Struct(name.to_string(), fields.to_vec()));

        Ok(TToplevelStmt::StructDecl {
            name: name.to_string(),
            fields: fields.to_vec(),
        })
    }

    fn check_enum_declare(
        &mut self,
        name: &str,
        fields: &[String],
    ) -> Return<TToplevelStmt> {
        // Check if the enum is not already defined
        if self.ctx.get(name).is_some() {
            bubble_err!(self, TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the enum into the context
        //self.ctx.insert(name.to_string(), Ty::Enum(name.to_string(), fields.to_vec()));

        // Insert the enum fields into the context
        for field in fields {
            self.ctx.insert(field.clone(), Ty::Numeric(Numeric::I32))
        }

        Ok(TToplevelStmt::EnumDecl {
            name: name.to_string(),
            fields: fields.to_vec(),
        })
    }

    fn check_function_declare(
        &mut self,
        name: &str,
        return_ty: Ty,
        params: &[(String, Ty)],
        body: &Block,
    ) -> Return<TToplevelStmt> {
        // Check if the function is not already defined
        if self.ctx.get(name).is_some() {
            bubble_err!(self, TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the function into the context
        let param_tys = params.iter().map(|(_, ty)| ty.clone()).collect();
        self.ctx.insert(name.to_string(), Ty::Function(Box::new(return_ty.clone()), param_tys));

        // Insert the function parameters into the context
        for (name, ty) in params {
            self.ctx.insert(name.clone(), ty.clone());
        }

        // Check the function body
        let body = self.check_block(body)?;

        Ok(TToplevelStmt::FunctionDecl {
            name: name.to_string(),
            return_ty,
            params: params.to_vec(),
            body,
        })
    }

    fn check_toplevel_stmt(
        &mut self,
        stmt: &Spanned<ToplevelStmt>,
    ) -> Return<Spanned<TToplevelStmt>> {
        stmt.clone().flat_map_spanned(|target| {
            match target {
                ToplevelStmt::Import { path, alias } => {
                    Ok(TToplevelStmt::Import {
                        path: path.clone(),
                        alias: alias.clone(),
                    })
                }
                ToplevelStmt::StructDecl { name, fields } => {
                    self.check_struct_declare(&name, &fields)
                }
                ToplevelStmt::EnumDecl { name, fields } => {
                    self.check_enum_declare(&name, &fields)
                }
                ToplevelStmt::FunctionDecl {
                    name,
                    return_ty,
                    params,
                    body,
                } => self.check_function_declare(&name, return_ty.clone(), &params, &body),
                ToplevelStmt::Stmt(stmt) => {
                    let stmt = self.check_stmt(&stmt)?;
                    Ok(TToplevelStmt::Stmt(stmt))
                }
                _ => unimplemented!(),
            }
        })
    }

    pub fn check(ast: Ast) -> ReturnMany<'cx, TypedAst> {
        let mut typeck = Typeck::new();
        let mut nodes = Vec::new();

        for node in ast.nodes {
            let node = typeck.check_toplevel_stmt(&node);
            match node {
                Ok(node) => nodes.push(node),
                Err(err) => typeck.errors.push(err),
            }
        }

        if !typeck.errors.is_empty() {
            return Err(typeck.errors);
        }

        Ok(TypedAst { nodes })
    }
}

impl Default for Typeck<'_> {
    fn default() -> Self {
        Self::new()
    }
}