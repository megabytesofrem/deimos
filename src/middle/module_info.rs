//! Module information builder, walks the type-annotated AST and collects module information
use crate::{
    syntax::types::{Numeric, StructureKind, Ty},
    utils::Spanned,
};

use super::typed_ast::{TBlock, TStmt, TToplevelStmt, TypedAst};

#[derive(Debug, Clone)]
pub struct StructureInfo {
    // Kind of data structure (struct or enum)
    pub kind: StructureKind,

    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub return_type: Ty,

    // Function body
    pub body: TBlock,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub filename: String,
    pub name: String,

    pub child_modules: Vec<ModuleInfo>,
    pub functions: Vec<FunctionInfo>,
    pub structures: Vec<StructureInfo>,
}

impl ModuleInfo {
    pub fn new(name: String) -> Self {
        Self {
            filename: String::new(),
            name,
            child_modules: vec![],
            functions: vec![],
            structures: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleBuilder {
    // Unmangled module information, used for lookup
    module_info: ModuleInfo,

    // Mangled module information, used for C codegen
    mangled_module_info: ModuleInfo,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            module_info: ModuleInfo::new("unnamed".to_string()),
            mangled_module_info: ModuleInfo::new("unnamed".to_string()),
        }
    }

    pub fn new_with_name(name: String) -> Self {
        Self {
            module_info: ModuleInfo::new(name.clone()),
            mangled_module_info: ModuleInfo::new(name),
        }
    }

    /// Get the unmangled module information
    pub fn get_unmangled_info(&self) -> ModuleInfo {
        self.module_info.clone()
    }

    /// Get the mangled module information
    pub fn get_mangled_info(&self) -> ModuleInfo {
        self.mangled_module_info.clone()
    }

    // We perform mangling to encode type information in the function name
    // since we lose this during transformation to C code
    pub(crate) fn mangle_type(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Number(_) => "i".to_string(),
            Ty::Bool => "b".to_string(),
            Ty::Char => "c".to_string(),
            Ty::String => "s".to_string(),
            Ty::Void => "v".to_string(),
            Ty::Unchecked => "u".to_string(),
            Ty::Function(_, _) => "F".to_string(),
            Ty::Pointer(inner) => "P".to_string() + &self.mangle_type(inner),
            _ => String::new(),
        }
    }

    pub(crate) fn mangle_function(&mut self, function: &FunctionInfo) -> String {
        // sum_i_i
        let mut mangled_name = function.name.clone();

        function.params.iter().for_each(|(_, ty)| {
            mangled_name.push_str(&self.mangle_type(ty));
        });

        mangled_name.push_str("_");
        mangled_name.push_str(&self.mangle_type(&function.return_type));

        format!("{}_{}", self.module_info.name, mangled_name)
    }

    pub fn build_module<'a>(&mut self, filename: &'a str, ast: TypedAst) -> ModuleInfo {
        // Extract the module name from the filename
        let module_name = filename
            .split('/')
            .last()
            .unwrap()
            .split('.')
            .next()
            .unwrap()
            .to_string();

        let mut info = self.walk_file(&filename, ast);
        let mut mangled_info = ModuleInfo::new(module_name.clone());

        info.filename = filename.to_string();
        info.name = module_name.clone();

        // Copy the module information
        mangled_info.filename = info.filename.clone();
        mangled_info.name = info.name.clone();
        self.module_info = info.clone();

        // Build mangled structure information
        for structure in info.structures.iter() {
            let structure_name = structure.name.clone();

            match structure.kind {
                StructureKind::Struct => {
                    let mangled_name = format!("{}_{}", module_name, structure_name);
                    mangled_info.structures.push(StructureInfo {
                        kind: StructureKind::Struct,
                        name: mangled_name,
                        fields: structure.fields.clone(),
                    });
                }

                StructureKind::Enum => {
                    let mangled_name = format!("{}_{}", module_name, structure_name);
                    mangled_info.structures.push(StructureInfo {
                        kind: StructureKind::Enum,
                        name: mangled_name,
                        fields: structure.fields.clone(),
                    });
                }
            }
        }

        // Build mangled function information
        for function in info.functions.iter() {
            let mangled_name = self.mangle_function(function);

            mangled_info.functions.push(FunctionInfo {
                name: mangled_name,
                params: function.params.clone(),
                return_type: function.return_type.clone(),
                body: function.body.clone(),
            });
        }

        // Return the module information
        self.mangled_module_info = mangled_info;
        self.mangled_module_info.clone()
    }

    // Walk the type-annotated AST of a file and collect module information (imports, functions, etc.)
    fn walk_file<'a>(&mut self, filename: &'a str, ast: TypedAst) -> ModuleInfo {
        self.module_info.name = filename.to_string();

        for node in ast.nodes {
            self.walk_toplevel(node.target);
        }

        self.module_info.clone()
    }

    fn walk_toplevel(&mut self, stmt: TToplevelStmt) {
        match stmt {
            TToplevelStmt::Import { path, alias: _ } => {
                // Add the child module to the current module
                let child_module = ModuleInfo::new(path.join("_"));

                // TODO: Walk the child module

                self.module_info.child_modules.push(child_module);
            }

            TToplevelStmt::Stmt(stmt) => self.walk_stmt(stmt),

            TToplevelStmt::EnumDecl { name, fields } => {
                let structure_info = StructureInfo {
                    kind: StructureKind::Enum,
                    name,
                    fields: fields
                        .into_iter()
                        .map(|f| (f, Ty::Number(Numeric::I32)))
                        .collect(),
                };

                self.module_info.structures.push(structure_info);
            }

            TToplevelStmt::StructDecl { name, fields } => {
                let structure_info = StructureInfo {
                    kind: StructureKind::Struct,
                    name,
                    fields,
                };

                self.module_info.structures.push(structure_info);
            }

            TToplevelStmt::FunctionDecl {
                name,
                params,
                return_ty,
                body,
            } => {
                let function_info = FunctionInfo {
                    name,
                    params,
                    return_type: return_ty,
                    body,
                };

                self.module_info.functions.push(function_info);
            }

            // We do not care about enums and structs for now
            _ => {}
        }
    }

    fn walk_stmt(&mut self, _stmt: Spanned<TStmt>) {
        // No need to walk statements since we are only interested in function declarations,
        // and structs/enum definitions
    }
}
