//! Type checker pass for the compiler.
//!
//! In reality, this is one _giant_ pass that performs type checking, type inference and substitution
//! of type variables to allow for generics later down the line.

use std::{cell::RefCell, rc::Rc};

use crate::sema::typed_ast::TExpr;
use crate::spanned::Spanned;
use crate::syntax::ast::*;

use crate::syntax::{
    ast_types::{FunctionInfo, SizedNumber, StructureInfo, StructureKind, Ty},
    lexer::SourceLoc,
};

use super::resolver::Resolver;
use super::sema_error::SemanticError;
use super::type_infer::SubstitutionEnv;
use super::typed_ast::{TStmt, TToplevelStmt, TypedAst};

pub(crate) type Return<'r, T> = anyhow::Result<T, SemanticError>;
pub(crate) type ReturnErrors<'r, T> = anyhow::Result<T, Vec<SemanticError>>;

#[derive(Debug, Clone)]
pub struct Typechecker {
    // The resolver holds information about the current model, declarations, and scopes
    // (Rc<RefCell> so we can allow multiple, concurrent mutable borrows)
    pub resolver: Rc<RefCell<Resolver>>,

    // The substitution environment represents a mapping of type variables and concrete types.
    pub subst: RefCell<SubstitutionEnv>,

    errors: Vec<SemanticError>,
}

impl<'t> Typechecker {
    pub fn new(filename: &str, resolver: Resolver) -> Self {
        Self {
            resolver: Rc::new(RefCell::new(resolver.clone())),
            subst: RefCell::new(SubstitutionEnv::new()),
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

    pub fn lookup_member_access(&self, member: &Member, _location: SourceLoc) -> Return<Ty> {
        // println!("Looking up member access {:?}", member);
        let target_ty = self.infer_expr(&member.target)?;

        // Member could be recursive so we will need to take this into account

        match target_ty {
            Ty::Struct(ref struct_info) | Ty::Enum(ref struct_info) => {
                if let Ok(field_ty) = struct_info.lookup_field(&member.name, _location.clone()) {
                    // If there is another member after this, recurse
                    if let Expr::Member(ref next_member) = member.target.target {
                        self.lookup_member_access(next_member, _location)
                    } else {
                        Ok(field_ty)
                    }
                } else {
                    Err(SemanticError::FieldNotFound {
                        struct_name: struct_info.name.clone(),
                        field: member.name.clone(),
                        location: _location.clone(),
                    })
                }
            }

            _ => Err(SemanticError::NotAValidStructure {
                found: target_ty,
                location: _location,
            }),
        }
    }

    pub fn check_literal(&mut self, lit: &Literal) -> Return<Ty> {
        Ok(self.infer_literal(lit))
    }

    pub fn check_expr(&mut self, expr: &Spanned<Expr>) -> Return<Spanned<TExpr>> {
        // This function pretty much just does type checking, and wraps it in a TExpr
        // to build a typed form of the AST, for future codegen usage

        let ty = self.infer_expr(expr)?;

        expr.clone().map_with_span(|texpr| match texpr {
            Expr::Literal(lit) => Ok(TExpr::Literal(lit.clone(), ty)),
            Expr::Ident(name) => {
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
            Expr::Member(member) => {
                let ty2 = self.lookup_member_access(&member, expr.location.clone())?;
                if ty != ty2 {
                    return Err(SemanticError::TypeMismatch {
                        expected: ty,
                        found: ty2,
                        location: expr.location.clone(),
                    });
                }

                Ok(TExpr::Member(member, ty))
                //todo!("Member access not implemented yet");
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
                let maybe_value = value.as_ref().or(None);
                let value = maybe_value
                    .map(|value| self.check_expr(value))
                    .transpose()?;

                // Insert the name into the resolver
                self.resolver.borrow_mut().insert_name(&name, ty.clone())?;

                Ok(TStmt::Let {
                    name: name.clone(),
                    ty: Some(ty),
                    value,
                })
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

        // Enter a new scope for the function
        self.resolver.borrow_mut().push_scope();

        for (param_name, param_ty) in params {
            self.resolver
                .borrow_mut()
                .insert_name(param_name, param_ty.clone())?;
        }

        // Check the function body in the scope
        let body = self.check_block(body)?;

        // Exit the created scope
        self.resolver.borrow_mut().pop_scope();

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
            .insert_modulewide_name(name, Ty::Struct(structure_info))?;

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
                .map(|f| (f, Ty::Number(SizedNumber::I32)))
                .collect(),
        };

        self.resolver
            .borrow_mut()
            .insert_modulewide_name(name, Ty::Struct(structure_info))?;

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
            ToplevelStmt::Import { path, alias } => {
                // Handle the import

                // NOTE: Paths are assumed to be relative
                let dotted_path = path.join(".");
                let imported_module = self.resolver.borrow().parse_module(&path.join("/"));

                if !self
                    .resolver
                    .borrow()
                    .imported_modules
                    .contains_key(&dotted_path)
                {
                    self.resolver
                        .borrow_mut()
                        .imported_modules
                        .insert(dotted_path, imported_module);
                }

                //
                // NOTE: We need to map it to its node in the `TypedAst`, despite this being a mirror
                // due to how our typesystem works.
                Ok(TToplevelStmt::Import { path, alias })
            }
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
        // Clear the resolver out
        self.resolver.borrow_mut().reset();
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
