//! Name resolution pass for the compiler.

use std::collections::{HashMap, HashSet};

use super::sema_error::SemanticError;
use crate::syntax::ast::{Ast, Block, Expr, Literal, Stmt, ToplevelStmt};
use crate::syntax::lexer::SourceLoc;
use crate::syntax::types::{FunctionInfo, Sized, StructureInfo, StructureKind, Ty};
use crate::utils::Spanned;

/// Result type for the name resolver
pub(crate) type Return<'r, T> = anyhow::Result<T, SemanticError>;
pub(crate) type ReturnErrors<'r, T> = anyhow::Result<T, Vec<SemanticError>>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Resolver {
    curr_module: String,
    errors: Vec<SemanticError>,

    scopes: Vec<HashMap<String, Ty>>,

    // Keep track of already declared names for redeclaration
    declared: HashSet<String>,
}

impl Resolver {
    pub fn new(module_name: &str) -> Self {
        Resolver {
            curr_module: module_name.to_string(),
            errors: Vec::new(),
            scopes: vec![HashMap::new()],
            declared: HashSet::new(),
        }
    }

    // Reset the resolver state
    pub fn reset(&mut self) {
        self.scopes.clear();
        self.scopes.push(HashMap::new());
        self.declared.clear();
    }

    // Push a new scope onto the stack
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Pop the current scope from the stack
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("Cannot pop the global scope");
    }

    pub fn is_declared(&self, name: &str) -> bool {
        self.declared.contains(name)
    }

    pub fn insert_name(&mut self, name: &str, ty: Ty) -> Return<()> {
        self.scopes
            .last_mut()
            .expect("No scope to insert into")
            .insert(name.to_string(), ty);

        if self.declared.contains(name) {
            return Err(SemanticError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        self.declared.insert(name.to_string());
        Ok(())
    }

    pub fn remove_name(&mut self, name: &str) -> Return<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.remove(name).is_some() {
                return Ok(());
            }
        }

        Err(SemanticError::NotInScope {
            name: name.to_string(),
            location: SourceLoc::default(),
        })
    }

    pub fn update_name(&mut self, name: &str, ty: Ty) -> Return<()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                *entry = ty;
                return Ok(());
            }
        }

        // TODO: Keep track of the previous symbol prior to this call so we can refer to it

        Err(SemanticError::NotInScope {
            name: name.to_string(),
            location: SourceLoc::default(),
        })
    }

    // Resolve a name in the current scope starting from the innermost scope
    pub(crate) fn resolve_name(&self, name: &str) -> Option<&Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }

    fn resolve_literal(&mut self, lit: &Literal) -> Return<Ty> {
        let result = match lit {
            Literal::Int(_) => Ty::Number(Sized::I32),
            Literal::Float32(_) => Ty::Number(Sized::F32),
            Literal::Float64(_) => Ty::Number(Sized::F64),
            Literal::Bool(_) => Ty::Bool,
            Literal::String(_) => Ty::String,
            _ => Ty::Unchecked,
        };

        Ok(result)
    }

    fn resolve_expr(&mut self, expr: &Spanned<Expr>) -> Return<()> {
        match &expr.target {
            Expr::Literal(lit) => {
                self.resolve_literal(&lit)?;
            }
            Expr::QualifiedName(name) => {
                if self.resolve_name(&name).is_none() {
                    return Err(SemanticError::NotInScope {
                        name: name.clone(),
                        location: expr.location.clone(),
                    });
                }
            }
            Expr::Reference(expr) => {
                self.resolve_expr(&expr)?;
            }
            Expr::BinOp(lhs, _op, rhs) => {
                self.resolve_expr(&lhs)?;
                self.resolve_expr(&rhs)?;
            }
            Expr::UnOp(_op, expr) => {
                self.resolve_expr(&expr)?;
            }
            Expr::Array(elems) => {
                for elem in elems {
                    self.resolve_expr(&elem)?;
                }
            }
            Expr::Cast(expr, _ty) => {
                self.resolve_expr(&expr)?;
            }
            Expr::StructCons { fields } => {
                for (_name, value) in fields {
                    self.resolve_expr(&value)?;
                }
            }
            Expr::ArrayIndex { array, index } => {
                self.resolve_expr(&array)?;
                self.resolve_expr(&index)?;
            }
            Expr::Call { callee, args } => {
                self.resolve_expr(&callee)?;

                for arg in args {
                    self.resolve_expr(&arg)?;
                }
            }

            _ => {}
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Spanned<Stmt>) -> Return<()> {
        match &stmt.target {
            Stmt::Expr(expr) => {
                // Resolve the expression
                self.resolve_expr(&expr)?;
            }
            Stmt::Return(expr) => {
                // Resolve the expression
                if let Some(expr) = expr {
                    self.resolve_expr(&expr)?;
                }
            }
            Stmt::BlockTerminator => {}
            Stmt::Let { name, ty, value } => {
                // Resolve the value
                if let Some(value) = value {
                    self.resolve_expr(&value)?;
                }

                if let Some(ty) = ty {
                    self.insert_name(&name, ty.clone())?;
                } else {
                    // Insert an unchecked type since we don't know the type yet
                    self.insert_name(&name, Ty::Unchecked)?;
                }
            }
            Stmt::Assign {
                name: target,
                value,
            } => {
                // Resolve the target and value
                self.resolve_expr(&target)?;
                self.resolve_expr(&value)?;
            }
            Stmt::If {
                cond,
                then_block,
                elif_blocks,
                else_block,
            } => {
                // Resolve the condition and blocks
                self.resolve_expr(&cond)?;
                self.resolve_block(&then_block)?;

                for (cond, block) in elif_blocks {
                    self.resolve_expr(&cond)?;
                    self.resolve_block(&block)?;
                }

                if let Some(block) = else_block {
                    self.resolve_block(&block)?;
                }
            }
            Stmt::For {
                init,
                from,
                to,
                body,
            } => {
                self.resolve_expr(&from)?;
                self.resolve_expr(&to)?;

                // Push a new scope for the loop body
                self.push_scope();
                self.insert_name(&init, Ty::Unchecked)?;

                self.resolve_block(&body)?;

                // Pop the loop body scope
                self.pop_scope();
            }
            Stmt::While { cond, body } => {
                self.resolve_expr(&cond)?;

                // Push a new scope for the loop body
                self.push_scope();
                self.resolve_block(&body)?;

                // Pop the loop body scope
                self.pop_scope();
            }
        }

        Ok(())
    }

    fn resolve_block(&mut self, block: &Block) -> Return<()> {
        for stmt in block {
            self.resolve_stmt(&stmt)?;
        }

        Ok(())
    }

    fn resolve_toplevel(&mut self, toplevel: &Spanned<ToplevelStmt>) -> ReturnErrors<()> {
        let mut errors = Vec::new();

        match &toplevel.target {
            ToplevelStmt::Import { path, alias } => {
                // TODO: Recursively resolve the imported module and merge the symbol tables
                todo!()
            }
            ToplevelStmt::Stmt(stmt) => {
                if let Err(err) = self.resolve_stmt(stmt) {
                    errors.push(err);
                }
            }
            ToplevelStmt::EnumDecl { name, fields } => {
                // Push the enum name onto the module info
                let mut enum_fields: Vec<(String, Ty)> = Vec::new();

                // Check if the symbol has already been declared
                // if self.module_info.symbols.contains_key(name) {
                //     errors.push(SemanticError::Redefinition {
                //         name: name.clone(),
                //         location: toplevel.location.clone(),
                //     });
                // }

                self.push_scope();
                for name in fields {
                    enum_fields.push((name.clone(), Ty::Number(Sized::I32)));
                    if let Err(err) = self.insert_name(name, Ty::Number(Sized::I32)) {
                        errors.push(err);
                    }
                }

                // Insert the enum into the module symbol table
                // self.module_info.insert_symbol(
                //     name.clone(),
                //     Ty::Enum(StructureInfo {
                //         name: name.clone(),
                //         fields: enum_fields,
                //         kind: StructureKind::Enum,
                //     }),
                // );

                // Pop after we have resolved the enum fields
                self.pop_scope();
            }
            ToplevelStmt::StructDecl { name, fields } => {
                let mut struct_fields: Vec<(String, Ty)> = Vec::new();

                // Check if the symbol has already been declared
                // if self.module_info.symbols.contains_key(name) {
                //     self.errors.push(SemanticError::Redefinition {
                //         name: name.clone(),
                //         location: toplevel.location.clone(),
                //     });
                // }

                self.push_scope();
                for (name, ty) in fields {
                    struct_fields.push((name.clone(), ty.clone()));
                    if let Err(err) = self.insert_name(name, ty.clone()) {
                        errors.push(err);
                    }
                }

                // Insert the struct into the module symbol table

                // self.module_info.insert_symbol(
                //     name.clone(),
                //     Ty::Struct(StructureInfo {
                //         name: name.clone(),
                //         fields: struct_fields,
                //         kind: StructureKind::Struct,
                //     }),
                // );

                // Pop after we have resolved the struct fields
                self.pop_scope();
            }

            // ExternDecl will be deprecated soon in favor of modules
            ToplevelStmt::ExternDecl {
                name: _,
                params: _,
                return_ty: _,
            } => {}

            ToplevelStmt::FunctionDecl {
                name,
                params,
                return_ty,
                body,
            } => {
                // Resolve the function body
                self.push_scope();

                for (name, ty) in params {
                    if let Err(err) = self.insert_name(name, ty.clone()) {
                        errors.push(err);
                    }
                }

                if let Err(err) = self.resolve_block(body) {
                    errors.push(err);
                }

                self.pop_scope();

                // Insert the function into the symbol table
                let result = self.insert_name(
                    name,
                    Ty::Function(Box::new(FunctionInfo {
                        name: name.clone(),
                        params: params.clone(),
                        return_ty: return_ty.clone(),

                        // We will replace this when we have typechecked the function
                        body: None,
                    })),
                );

                if let Err(err) = result {
                    errors.push(err)
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }

    pub fn resolve(&mut self, ast: &Ast) -> ReturnErrors<()> {
        // Clear previous errors before starting resolution
        self.errors.clear();

        for node in &ast.nodes {
            // Try to resolve the top-level node
            if let Err(err) = self.resolve_toplevel(node) {
                self.errors.extend(err);
            }
        }

        // After processing all nodes, return either Ok or accumulated errors
        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(())
        }
    }
}
