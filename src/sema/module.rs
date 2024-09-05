use crate::syntax::ast_types::{FunctionInfo, StructureInfo};

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub filename: String,
    pub module_name: String,

    pub child_modules: Vec<ModuleInfo>,
    pub functions: Vec<FunctionInfo>,
    pub structures: Vec<StructureInfo>,
}

// TODO: Implement a module system so we can deprecate externs and allow for proper imports

impl ModuleInfo {
    pub fn new(filename: String, module_name: String) -> Self {
        ModuleInfo {
            filename,
            module_name,
            child_modules: Vec::new(),
            functions: Vec::new(),
            structures: Vec::new(),
        }
    }
}
