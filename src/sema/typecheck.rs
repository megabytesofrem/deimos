//! Type checker pass for the compiler.

use std::{cell::RefCell, rc::Rc};

use crate::sema::typed_ast::TExpr;
use crate::syntax::ast::*;
use crate::syntax::types::{FunctionInfo, StructureInfo, StructureKind};
use crate::syntax::{lexer::SourceLoc, types::Sized, types::Ty};
use crate::utils::Spanned;

use super::resolver::Resolver;
use super::sema_error::SemanticError;
use super::typed_ast::{TStmt, TToplevelStmt, TypedAst};

pub(crate) type Return<'r, T> = anyhow::Result<T, SemanticError>;
pub(crate) type ReturnErrors<'r, T> = anyhow::Result<T, Vec<SemanticError>>;

#[derive(Debug, Clone)]
pub struct Typechecker {
    pub resolver: Rc<RefCell<Resolver>>,
    errors: Vec<SemanticError>,
}

impl<'t> Typechecker {
    pub fn new(resolver: Resolver) -> Self {
        Self {
            resolver: Rc::new(RefCell::new(resolver)),
            errors: Vec::new(),
        }
    }

    pub fn lookup_name(&self, name: &str, location: SourceLoc) -> Return<Ty> {
        let resolver = self.resolver.borrow_mut();

        match resolver.resolve_name(name) {
            Some(symbol) => Ok(symbol.clone()),
            None => Err(SemanticError::NotInScope {
                name: name.to_string(),
                location,
            }),
        }
    }

    pub fn check_literal(&mut self, lit: &Literal) -> Return<Ty> {
        Ok(self.infer_literal(lit))
    }

    pub fn check_expr(&mut self, expr: &Spanned<Expr>) -> Return<Spanned<TExpr>> {
        let ty = self.infer_expr(expr)?;

        expr.clone().map_with_span(|texpr| match texpr {
            Expr::Literal(lit) => Ok(TExpr::Literal(lit.clone(), ty)),

            Expr::QualifiedName(name) => {
                let ty2 = self.lookup_name(&name, expr.location.clone())?;
                if ty != ty2 {
                    return Err(SemanticError::TypeMismatch {
                        expected: ty,
                        found: ty2,
                        location: expr.location.clone(),
                    });
                }

                Ok(TExpr::Name(name, ty))
            }
            Expr::BinOp(lhs, op, rhs) => Ok(TExpr::BinOp(
                Box::new(self.check_expr(&lhs)?),
                op.clone(),
                Box::new(self.check_expr(&rhs)?),
            )),
            Expr::UnOp(op, expr) => {
                let expr = self.check_expr(&expr)?;
                Ok(TExpr::UnOp(op.clone(), Box::new(expr)))
            }
            Expr::Array(elems) => {
                let mut array = Vec::new();

                for elem in elems {
                    array.push(self.check_expr(&elem)?);
                }

                Ok(TExpr::Array(array))
            }
            Expr::Cast(expr, ty) => {
                let expr = self.check_expr(&expr)?;
                Ok(TExpr::Cast(Box::new(expr), ty.clone()))
            }
            Expr::StructCons { fields } => {
                let mut fields_: Vec<(String, Spanned<TExpr>)> = Vec::new();
                for (name, expr) in fields {
                    fields_.push((name.clone(), self.check_expr(&expr)?));
                }

                Ok(TExpr::StructCons { fields: fields_ })
            }
            Expr::ArrayIndex { array, index } => Ok(TExpr::ArrayIndex {
                array: Box::new(self.check_expr(&array)?),
                index: Box::new(self.check_expr(&index)?),
            }),
            Expr::Call { callee, args } => {
                let callee = self.check_expr(&callee)?;
                let mut args_: Vec<Spanned<TExpr>> = Vec::new();
                for arg in args {
                    args_.push(self.check_expr(&arg)?);
                }

                Ok(TExpr::Call {
                    callee: Box::new(callee),
                    args: args_,
                })
            }

            expr => unimplemented!("Unimplemented {:?}", expr),
        })
    }

    pub fn check_stmt(&mut self, stmt: &Spanned<Stmt>) -> Return<Spanned<TStmt>> {
        stmt.clone().map_with_span(|tstmt| match tstmt {
            Stmt::Expr(expr) => Ok(TStmt::Expr(self.check_expr(&expr)?.target)),
            Stmt::Let { name, ty, value } => {
                let ty = ty.clone().unwrap_or(Ty::Unchecked);

                // TODO: Perform type casting before we call check_expr here
                if let Some(value) = value {
                    let value = self.check_expr(&value)?;
                    Ok(TStmt::Let {
                        name: name.clone(),
                        ty: Some(ty),
                        value: Some(value),
                    })
                } else {
                    panic!()
                }
            }
            Stmt::Assign { name, value } => {
                let location = stmt.location.clone();
                let name_ty = self.infer_expr(&name)?;
                let value_ty = self.infer_expr(&value)?;

                if !self.check_equal(&name_ty, &value_ty, &location) {
                    return Err(SemanticError::TypeMismatch {
                        expected: name_ty,
                        found: value_ty,
                        location,
                    });
                }

                Ok(TStmt::Assign {
                    name: self.check_expr(&name)?.target,
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

                Ok(TStmt::If {
                    cond,
                    then_block,
                    elif_blocks: Vec::new(),
                    else_block: else_block
                        .as_ref()
                        .map(|block| self.check_block(block))
                        .transpose()?,
                })
            }
            Stmt::For {
                init,
                from,
                to,
                body,
            } => {
                // Insert the temporary initalizer in the current scope
                let init_ty = self.infer_expr(&from)?;
                self.resolver
                    .borrow_mut()
                    .insert_name(&init, init_ty.clone())?;

                let from = self.check_expr(&from)?;
                let to = self.check_expr(&to)?;
                let body = self.check_block(&body)?;

                // Remove the temporary initializer from the current scope
                self.resolver.borrow_mut().remove_name(&init)?;

                Ok(TStmt::For {
                    init,
                    from,
                    to,
                    body,
                })
            }
            Stmt::While { cond, body } => {
                let cond = self.check_expr(&cond)?;
                let body = self.check_block(&body)?;

                Ok(TStmt::While { cond, body })
            }
            _ => unimplemented!(),
        })
    }

    fn check_block(&mut self, block: &Block) -> Return<Vec<Spanned<TStmt>>> {
        let block = block
            .iter()
            .map(|stmt| self.check_stmt(stmt))
            .collect::<Return<Vec<Spanned<TStmt>>>>()?;

        Ok(block)
    }

    fn check_function_declare(
        &mut self,
        name: &str,
        params: &[(String, Ty)],
        return_ty: Ty,
        body: &Block,
    ) -> Return<TToplevelStmt> {
        if self.resolver.borrow().is_declared(name) {
            return Err(SemanticError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        let function_info = FunctionInfo {
            name: name.to_string(),
            params: params.to_vec(),
            return_ty: return_ty.clone(),
            body: None,
        };

        self.resolver
            .borrow_mut()
            .insert_name(name, Ty::Function(Box::new(function_info)))?;

        let body = self.check_block(body)?;
        Ok(TToplevelStmt::FunctionDecl {
            name: name.to_string(),
            params: params.to_vec(),
            return_ty: return_ty.clone(),
            body: body.clone(),
        })
    }

    fn check_struct_declare(
        &mut self,
        name: &str,
        fields: &[(String, Ty)],
    ) -> Return<TToplevelStmt> {
        if self.resolver.borrow().is_declared(name) {
            // TODO: Add location to the error
            return Err(SemanticError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        let structure_info = StructureInfo {
            kind: StructureKind::Struct,
            name: name.to_string(),
            fields: fields.to_vec(),
        };

        self.resolver
            .borrow_mut()
            .insert_name(name, Ty::Struct(structure_info))?;

        // For now, we just pollute the global scope with the fields
        // TODO: Scope this to the structure itself
        for (field_name, field_ty) in fields {
            self.resolver
                .borrow_mut()
                .insert_name(field_name, field_ty.clone())?;
        }

        Ok(TToplevelStmt::StructDecl {
            name: name.to_string(),
            fields: fields.to_vec(),
        })
    }

    fn check_enum_declare(&mut self, name: &str, fields: &[String]) -> Return<TToplevelStmt> {
        if self.resolver.borrow().is_declared(name) {
            // TODO: Add location to the error
            return Err(SemanticError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        let structure_info = StructureInfo {
            kind: StructureKind::Enum,
            name: name.to_string(),
            fields: fields
                .to_vec()
                .into_iter()
                .map(|f| (f, Ty::Number(Sized::I32)))
                .collect(),
        };

        self.resolver
            .borrow_mut()
            .insert_name(name, Ty::Struct(structure_info))?;

        // For now, we just pollute the global scope with the fields
        // TODO: Scope this to the structure itself
        for field in fields {
            self.resolver
                .borrow_mut()
                .insert_name(field, Ty::Number(Sized::I32))?;
        }

        Ok(TToplevelStmt::EnumDecl {
            name: name.to_string(),
            fields: fields.to_vec(),
        })
    }

    fn check_toplevel_stmt(
        &mut self,
        stmt: &Spanned<ToplevelStmt>,
    ) -> ReturnErrors<Spanned<TToplevelStmt>> {
        let mut errors = Vec::new();

        let result = stmt.clone().map_with_span(|tstmt| match tstmt {
            ToplevelStmt::Stmt(stmt) => self
                .check_stmt(&stmt)
                .map(TToplevelStmt::Stmt)
                .map_err(|e| errors.push(e)),
            ToplevelStmt::Import { path, alias } => Ok(TToplevelStmt::Import { path, alias }),
            ToplevelStmt::StructDecl { name, fields } => self
                .check_struct_declare(&name, &fields)
                .map_err(|e| errors.push(e)),
            ToplevelStmt::EnumDecl { name, fields } => self
                .check_enum_declare(&name, &fields)
                .map_err(|e| errors.push(e)),
            ToplevelStmt::FunctionDecl {
                name,
                params,
                return_ty,
                body,
            } => self
                .check_function_declare(&name, &params, return_ty.clone(), &body)
                .map_err(|e| errors.push(e)),

            _ => unimplemented!(),
        });

        // Check if there were any errors
        if errors.is_empty() {
            Ok(result.unwrap())
        } else {
            // Return all accumulated errors
            Err(errors)
        }
    }

    pub fn check(&mut self, ast: &Ast) -> ReturnErrors<TypedAst> {
        // Run the resolver on the AST, to populate the symbol table
        self.resolver.borrow_mut().reset();

        // match self.resolver.borrow_mut().resolve(ast) {
        //     Ok(_) => (),
        //     Err(e) => self.errors.extend(e),
        // }

        let mut nodes = Vec::new();

        for node in &ast.nodes {
            match self.check_toplevel_stmt(node) {
                Ok(node) => nodes.push(node),
                Err(errors) => {
                    for error in errors {
                        self.errors.push(error);
                    }
                }
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        } else {
            Ok(TypedAst { nodes })
        }
    }
}
