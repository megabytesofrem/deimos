//! Name resolution pass for the compiler.

use std::collections::{HashMap, HashSet};

use super::module::ModuleInfo;
use crate::syntax::types::Ty;

#[derive(Debug, Clone)]
pub struct Resolver {
    curr_module: String,

    scopes: Vec<HashMap<String, Ty>>,
    modules: HashMap<String, ModuleInfo>,
    declared: HashSet<String>,
}

/// Result type for the name resolver
pub(crate) type Return<'r, T> = anyhow::Result<T, String>;

impl Resolver {
    pub fn new(module_name: &str) -> Self {
        Resolver {
            curr_module: module_name.to_string(),
            scopes: vec![HashMap::new()],
            modules: HashMap::new(),
            declared: HashSet::new(),
        }
    }

    // Push a new scope onto the stack
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Pop the current scope from the stack
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("Cannot pop the global scope");
    }

    pub(crate) fn insert_entry(&mut self, name: &str, ty: Ty) -> Return<()> {
        if self.declared.contains(name) {
            panic!("Variable {} already declared", name);
        }

        self.scopes
            .last_mut()
            .expect("No scope to insert into")
            .insert(name.to_string(), ty);

        self.declared.insert(name.to_string());
        Ok(())
    }

    // Resolve a name in the current scope starting from the innermost scope
    pub(crate) fn resolve_name(&mut self, name: &str) -> Option<&Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }

        None
    }

    // Resolve a module and its symbols
}
