use crate::syntax::types::{FunctionInfo, StructureInfo};

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub filename: String,
    pub module_name: String,

    pub child_modules: Vec<ModuleInfo>,
    pub functions: Vec<FunctionInfo>,
    pub structures: Vec<StructureInfo>,
}

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
