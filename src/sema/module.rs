use std::collections::HashMap;
use std::fs;

use crate::spanned::Spanned;
use crate::syntax::ast::{Ast, ToplevelStmt};
use crate::syntax::ast_types::{FunctionInfo, StructureInfo, Ty};
use crate::syntax::parser::Parser;

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub filename: String,
    pub module_name: String,

    pub exports: HashMap<String, Ty>,
    pub imports: Vec<String>,
}

// TODO: Implement a module system so we can deprecate externs and allow for proper imports

impl ModuleInfo {
    pub fn new(filename: &str, module_name: &str) -> Self {
        ModuleInfo {
            filename: filename.to_string(),
            module_name: module_name.to_string(),
            exports: HashMap::new(),
            imports: Vec::new(),
        }
    }

    pub fn insert_type(&mut self, name: &str, ty: &Ty) {
        self.exports.insert(name.to_string(), ty.clone());
    }
}
