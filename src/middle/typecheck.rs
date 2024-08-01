//! Type checker pass for the AST. Produces a typed AST from an untyped AST

use thiserror::Error;

use crate::syntax::ast::Numeric;
use crate::syntax::ast::{Ast, Block, Expr, Literal, Stmt, ToplevelStmt, Ty};
use crate::syntax::lexer::SourceLoc;
use crate::utils::Spanned;

use super::name_resolver::Resolver;
use super::typed_ast::*;

#[derive(Debug, Clone, Error, PartialEq)]
pub enum TypeError {
    #[error("{location}: Type mismatch between '{expected:?}' and '{found:?}'")]
    TypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location}: Redefinition of variable or type '{name}'")]
    Redefinition { name: String, location: SourceLoc },

    #[error("{location}: Undefined local '{name}' being used before declaration")]
    UndefinedLocal { name: String, location: SourceLoc },

    #[error("{location}: Undefined function '{name}' being used before declaration")]
    UndefinedFunction { name: String, location: SourceLoc },

    #[error("{location}: Arity mismatch between '{expected}' and '{found}'")]
    ArityMismatch {
        expected: usize,
        found: usize,
        location: SourceLoc,
    },

    #[error("{location}: Return type mismatch between '{expected:?}' and '{found:?}'")]
    ReturnTypeMismatch {
        expected: Ty,
        found: Ty,
        location: SourceLoc,
    },

    #[error("{location}: Invalid type cast between '{from:?}' and '{into:?}'")]
    InvalidCast {
        from: Ty,
        into: Ty,
        location: SourceLoc,
    },
}

// Holds the type that is returned from the type checker
pub(crate) type Return<T> = anyhow::Result<T, TypeError>;
pub(crate) type Returns<'cx, T> = anyhow::Result<T, Vec<TypeError>>;

// Type checker pass
#[derive(Debug, Clone)]
pub struct Typecheck<'tc> {
    ctx: Resolver,
    marker: std::marker::PhantomData<&'tc ()>,

    pub errors: Vec<TypeError>,
}

impl<'tc> Typecheck<'tc> {
    pub fn new() -> Self {
        Typecheck {
            ctx: Resolver::new(),
            marker: std::marker::PhantomData,
            errors: Vec::new(),
        }
    }

    pub(crate) fn get_typing_context(&self) -> &Resolver {
        &self.ctx
    }

    // Checking functions
    // NOTE: We operate on the AST wrapped in Spanned<T>

    pub(crate) fn check_variable(&self, name: &str, location: SourceLoc) -> Return<Ty> {
        self.ctx.get(name).ok_or_else(|| TypeError::UndefinedLocal {
            name: name.to_string(),
            location,
        })
    }

    pub(crate) fn check_literal(&mut self, literal: &Literal) -> Return<Ty> {
        self.infer_literal(literal)
    }

    fn check_expr(&mut self, expr: &Spanned<Expr>) -> Return<Spanned<TExpr>> {
        let ty = self.infer_expr(expr)?;

        expr.clone().flat_map_spanned(|target| {
            match target {
                Expr::Literal(lit) => {
                    //println!("check_expr literal: {:?}", lit);
                    return Ok(TExpr::Literal(lit.clone(), ty.clone()));
                }
                Expr::QualifiedName(name) => {
                    let ty2 = self.check_variable(&name, expr.location.clone())?;
                    if ty != ty2 {
                        return Err(TypeError::TypeMismatch {
                            expected: ty.clone(),
                            found: ty2.clone(),
                            location: expr.location.clone(),
                        });
                    } else {
                        // Variable is valid
                        return Ok(TExpr::Name(name.clone(), ty.clone()));
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

                    return Ok(TExpr::Array(array));
                }
                Expr::Cast(expr, ty) => {
                    let expr = self.check_expr(&expr)?;
                    println!("TCast: {:?} -> {:?}", expr, ty);

                    let t = Ok(TExpr::Cast(Box::new(expr), ty.clone()));
                    println!("TCast: {:?}", t);
                    return t;
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
                    let mut args_ = Vec::new();
                    for arg in args {
                        args_.push(self.check_expr(&arg)?);
                    }

                    return Ok(TExpr::Call {
                        callee: Box::new(self.check_expr(&callee)?),
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
                Stmt::BlockTerminator => Ok(TStmt::BlockTerminator),
                Stmt::Let { name, ty, value } => {
                    let ty = ty.clone().unwrap_or(Ty::Unchecked);

                    // Perform type casting *before* we call check_expr
                    let casted_value = value
                        .as_ref()
                        .map(|v| self.raw_cast_expr(v, &ty))
                        .transpose()?;

                    let value_ = casted_value.as_ref().map(|v| self.check_expr(v));

                    // Check if the local is not already defined
                    if self.ctx.get(&name).is_some() {
                        return Err(TypeError::Redefinition {
                            name: name.clone(),
                            location: stmt.location.clone(),
                        });
                    }

                    self.ctx.insert(name.clone(), ty.clone());
                    Ok(TStmt::Let {
                        name: name.clone(),
                        ty: Some(ty),
                        value: value_.map(|v| v.unwrap()),
                    })
                }
                Stmt::Assign { target, value } => {
                    let target_ty = self.infer_expr(&target)?;
                    let value_ty = self.infer_expr(&value)?;

                    if target_ty != value_ty {
                        return Err(TypeError::TypeMismatch {
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
                Stmt::If {
                    cond,
                    then_block,
                    elif_blocks: _,
                    else_block,
                } => {
                    let cond = self.check_expr(&cond)?;
                    let then_block = self.check_block(&then_block)?;
                    let else_block = else_block
                        .as_ref()
                        .map(|block| self.check_block(block))
                        .transpose()?;

                    Ok(TStmt::If {
                        cond,
                        then_block,
                        else_block,
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

    fn check_extern_declare(
        &mut self,
        name: &str,
        params: &[(String, Ty)],
        return_ty: Ty,
    ) -> Return<TToplevelStmt> {
        // Check if the function is not already defined
        if self.ctx.get(name).is_some() {
            return Err(TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the function into the context
        let param_tys = params.iter().map(|(_, ty)| ty.clone()).collect();
        self.ctx.insert(
            name.to_string(),
            Ty::Function(Box::new(return_ty.clone()), param_tys),
        );

        // Insert the function parameters into the context
        for (name, ty) in params {
            self.ctx.insert(name.clone(), ty.clone());
        }

        Ok(TToplevelStmt::ExternDecl {
            name: name.to_string(),
            params: params.to_vec(),
            return_ty,
        })
    }

    fn check_struct_declare(
        &mut self,
        name: &str,
        fields: &[(String, Ty)],
    ) -> Return<TToplevelStmt> {
        // Check if the struct is not already defined
        if self.ctx.get(name).is_some() {
            return Err(TypeError::Redefinition {
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

    fn check_enum_declare(&mut self, name: &str, fields: &[String]) -> Return<TToplevelStmt> {
        // Check if the enum is not already defined
        if self.ctx.get(name).is_some() {
            return Err(TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the enum into the context
        //self.ctx.insert(name.to_string(), Ty::Enum(name.to_string(), fields.to_vec()));

        // Insert the enum fields into the context
        for field in fields {
            self.ctx.insert(field.clone(), Ty::Number(Numeric::I32))
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
            return Err(TypeError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        // Insert the function into the context
        let param_tys = params.iter().map(|(_, ty)| ty.clone()).collect();
        self.ctx.insert(
            name.to_string(),
            Ty::Function(Box::new(return_ty.clone()), param_tys),
        );

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
        stmt.clone().flat_map_spanned(|target| match target {
            ToplevelStmt::Import { path, alias } => Ok(TToplevelStmt::Import {
                path: path.clone(),
                alias: alias.clone(),
            }),
            ToplevelStmt::StructDecl { name, fields } => self.check_struct_declare(&name, &fields),
            ToplevelStmt::EnumDecl { name, fields } => self.check_enum_declare(&name, &fields),
            ToplevelStmt::FunctionDecl {
                name,
                return_ty,
                params,
                body,
            } => self.check_function_declare(&name, return_ty.clone(), &params, &body),
            ToplevelStmt::ExternDecl {
                name,
                params,
                return_ty,
            } => self.check_extern_declare(&name, &params, return_ty.clone()),

            ToplevelStmt::Stmt(stmt) => {
                let stmt = self.check_stmt(&stmt)?;
                Ok(TToplevelStmt::Stmt(stmt))
            }
        })
    }

    pub fn check(ast: Ast) -> Returns<'tc, TypedAst> {
        let mut typecheck = Typecheck::new();
        let mut nodes = Vec::new();

        for node in ast.nodes {
            let node = typecheck.check_toplevel_stmt(&node);
            match node {
                Ok(node) => nodes.push(node),
                Err(err) => typecheck.errors.push(err),
            }
        }

        if !typecheck.errors.is_empty() {
            return Err(typecheck.errors);
        }

        Ok(TypedAst { nodes })
    }
}

impl Default for Typecheck<'_> {
    fn default() -> Self {
        Self::new()
    }
}
