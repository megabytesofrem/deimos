//! Name resolution pass for the compiler.

use std::collections::HashMap;

use super::module_info::ModuleInfo;
use crate::syntax::types::Ty;

#[derive(Debug, Clone)]
pub struct Resolver {
    // Stack of scopes, where each scope is a hashmap of names to types
    scopes: Vec<HashMap<String, Ty>>,

    // Module info (used for lookup)
    module_info: ModuleInfo,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![HashMap::new()],
            module_info: ModuleInfo::new("unnamed".to_string()),
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn insert(&mut self, name: String, ty: Ty) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    pub fn remove(&mut self, name: &str) {
        self.scopes.last_mut().unwrap().remove(name);
    }

    pub fn get(&self, name: &str) -> Option<Ty> {
        // Lookup the name in the current scope
        // File-level scope is used for module resolution

        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }

        None
    }
}
