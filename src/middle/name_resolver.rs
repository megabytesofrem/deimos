//! Name resolution pass for the compiler.

use std::collections::HashMap;

use crate::syntax::ast::Ty;

#[derive(Debug, Clone)]
pub struct NameInfo {
    pub name: String,
    pub ty: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct ScopeStack {
    // Stack of scopes, where each scope is a hashmap of names to types
    scopes: Vec<HashMap<String, Ty>>,

    // File scope, used for module resolution
    file_scope: HashMap<String, Ty>,
}

impl ScopeStack {
    pub fn new() -> Self {
        ScopeStack {
            scopes: vec![HashMap::new()],
            file_scope: HashMap::new(),
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

        if name.contains("::") {
            let mut parts = name.split("::");
            let module_name = parts.next()?;
            let function_name = parts.next()?;

            // Lookup the module name in the current scope
            if let Some(module_ty) = self.file_scope.get(module_name) {
                // if let Ty::Module(module_info) = module_ty {
                //     // Lookup the function name in the module
                //     if let Some(function_info) = module_info.get_function(function_name) {
                //         // Insert the function type into the current scope
                //         return Some(function_info.clone().into());
                //     }
                // }
            }

            return None;
        }

        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }

        None
    }
}
