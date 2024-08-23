//! Name resolution pass for the compiler.

use std::collections::{HashMap, HashSet};

use super::sema_error::SemanticError;
use crate::syntax::lexer::SourceLoc;
use crate::syntax::types::Ty;

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
    pub fn resolve_name(&self, name: &str) -> Option<&Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }
}
