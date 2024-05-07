//! Module system for the backend of Deimos.
//!
//! ```lua
//! module summer
//!     function sum_ints(a:i32, b:i32):i32
//!         return a + b
//!     end
//! end
//!
//! summer.sum_ints(1, 2)
//! ```

use crate::syntax::ast::Ty;

use super::pretty_print::PrettyPrinter;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub args: Vec<(String, Ty)>,
    pub return_type: Ty,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInfo {
    pp: PrettyPrinter,

    pub name: String,
    pub functions: Vec<FunctionInfo>,
}

impl ModuleInfo {
    pub fn new(name: String) -> Self {
        Self {
            pp: PrettyPrinter::new(),
            name,
            functions: Vec::new(),
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionInfo> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn add_function(&mut self, name: String, args: Vec<(String, Ty)>, return_type: Ty) {
        self.functions.push(FunctionInfo {
            name,
            args,
            return_type,
        });
    }

    fn prefix_module_name(&self, module_name: &str, func_name: &str) -> String {
        format!("{}_{}", module_name, func_name)
    }

    // Transpile the `ModuleInfo` into a C header file. Each module contains a `ModuleInfo` which
    // contains the structure of the module, including the function signatures.
    pub fn compile_header(&mut self) -> String {
        let module_name = self.name.to_uppercase();
        let comment_string = format!("// Generated header for module: {}\n", self.name);

        // self.pp.emit_line(&comment_string);
        // self.pp.emit_line(&format!("#ifndef {}_H\n", module_name));
        // self.pp.emit_line(&format!("#define {}_H\n\n", module_name));
        //
        // // Walk through the functions and generate prototypes
        // for func in &self.functions {
        //     let mut prototype = String::new();
        //     prototype.push_str(&format!(
        //         "{} {}(",
        //         self.pp.to_typename(&func.return_type),
        //         // Prefix the function name with the module name
        //         self.prefix_module_name(&self.name, &func.name)
        //     ));
        //
        //     for (i, (arg_name, arg_ty)) in func.args.iter().enumerate() {
        //         prototype.push_str(&format!("{} {}", self.pp.to_typename(arg_ty), arg_name));
        //
        //         if i < func.args.len() - 1 {
        //             prototype.push_str(", ");
        //         }
        //     }
        //
        //     prototype.push_str(");");
        //     self.pp.emit_line(&prototype);
        // }
        //
        // self.pp.emit_line("#endif");
        //
        // self.pp.lines.join("\n")

        todo!()
    }
}

impl From<FunctionInfo> for Ty {
    fn from(func: FunctionInfo) -> Self {
        Ty::Function(
            Box::new(func.return_type),
            func.args.iter().map(|(_, ty)| ty.clone()).collect(),
        )
    }
}
