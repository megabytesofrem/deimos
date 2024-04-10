use std::collections::HashMap;

use crate::syntax::ast::Ty;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<HashMap<String, Ty>>,
}

impl ScopeStack {
    pub fn new() -> Self {
        ScopeStack {
            scopes: vec![HashMap::new()],
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

    pub fn get(&self, name: &str) -> Option<&Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }
}
