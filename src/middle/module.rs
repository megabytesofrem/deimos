use std::collections::HashMap;

use super::typed_ast::{TBlock, TStmt, TToplevelStmt, TypedAst};
use crate::syntax::types::{FunctionInfo, Numeric, StructureInfo, StructureKind, Ty};

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub module_name: String,
    pub symbols: HashMap<String, Ty>,
}

impl ModuleInfo {
    pub fn new() -> Self {
        Self {
            module_name: String::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn new_with_name(name: String) -> Self {
        Self {
            module_name: name,
            symbols: HashMap::new(),
        }
    }

    fn mangle_type(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Number(_) => "i".to_string(),
            Ty::Bool => "b".to_string(),
            Ty::Char => "c".to_string(),
            Ty::String => "s".to_string(),
            Ty::Void => "v".to_string(),
            Ty::Unchecked => "U".to_string(),
            Ty::Function(f) => {
                // foo_ii
                let mut mangled = "f".to_string();
                for param in &f.params.clone() {
                    mangled.push_str(&self.mangle_type(&param.1));
                }
                mangled
            }
            Ty::Pointer(inner) => "P".to_string() + &self.mangle_type(inner),
            _ => String::new(),
        }
    }

    fn mangle_function(&mut self, func: &FunctionInfo) -> String {
        let mut mangled = func.name.clone();

        for param in &func.params {
            mangled.push_str(&self.mangle_type(&param.1));
        }

        mangled.push_str("_");
        mangled.push_str(&self.mangle_type(&func.return_type));

        format!("{}_{}", self.module_name, mangled)
    }

    pub fn walk_module<'m>(&mut self, filename: &'m str, ast: TypedAst) -> ModuleInfo {
        let module_name = filename
            .split('/')
            .last()
            .unwrap()
            .split('.')
            .next()
            .unwrap();

        let mut info = self.walk_file(filename, ast);
        let mut mangled_info = ModuleInfo::new_with_name(module_name.to_string());

        info.module_name = module_name.to_string();

        // Copy the module information
        mangled_info.module_name = info.module_name.clone();

        // Build mangled structure information
        for (_, symbol_ty) in info.symbols.iter() {
            match symbol_ty {
                Ty::Struct(struct_info) => {
                    let mangled_name = format!("{}_{}", module_name, struct_info.name);
                    mangled_info.symbols.insert(
                        mangled_name.clone(),
                        Ty::Struct(StructureInfo {
                            kind: struct_info.kind.clone(),
                            name: mangled_name,
                            fields: struct_info.fields.clone(),
                        }),
                    );
                }
                Ty::Enum(enum_info) => {
                    let mangled_name = format!("{}_{}", module_name, enum_info.name);
                    mangled_info.symbols.insert(
                        mangled_name.clone(),
                        Ty::Struct(StructureInfo {
                            kind: StructureKind::Enum,
                            name: mangled_name,
                            fields: enum_info.fields.clone(),
                        }),
                    );
                }
                Ty::Function(func_info) => {
                    let mangled_name = self.mangle_function(func_info);
                    mangled_info.symbols.insert(
                        mangled_name.clone(),
                        Ty::Function(Box::new(FunctionInfo {
                            name: mangled_name,
                            params: func_info.params.clone(),
                            return_type: func_info.return_type.clone(),
                            body: func_info.body.clone(),
                        })),
                    );
                }

                // Ignore other symbols
                _ => {}
            }
        }

        self.clone()
    }

    pub fn walk_file<'m>(&mut self, filename: &'m str, ast: TypedAst) -> ModuleInfo {
        self.module_name = filename.to_string();

        for stmt in ast.nodes {
            self.walk_toplevel(stmt.target);
        }

        self.clone()
    }

    pub fn walk_toplevel(&mut self, stmt: TToplevelStmt) {
        match stmt {
            TToplevelStmt::Import { path, alias } => todo!(),
            TToplevelStmt::Stmt(stmt) => self.walk_stmt(stmt.target),

            TToplevelStmt::EnumDecl { name, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|f| (f, Ty::Number(Numeric::I32)))
                    .collect();
                self.symbols.insert(
                    name.clone(),
                    Ty::Struct(StructureInfo {
                        kind: StructureKind::Enum,
                        name,
                        fields,
                    }),
                );
            }
            TToplevelStmt::StructDecl { name, fields } => {
                let fields = fields.into_iter().map(|(name, ty)| (name, ty)).collect();
                self.symbols.insert(
                    name.clone(),
                    Ty::Struct(StructureInfo {
                        kind: StructureKind::Struct,
                        name,
                        fields,
                    }),
                );
            }
            TToplevelStmt::FunctionDecl {
                name,
                params,
                return_type,
                body,
            } => {
                self.symbols.insert(
                    name.clone(),
                    Ty::Function(Box::new(FunctionInfo {
                        name,
                        params,
                        return_type,
                        body: Some(body),
                    })),
                );
            }
            _ => todo!(),
        }
    }

    pub fn walk_stmt(&mut self, stmt: TStmt) {
        // TODO: Only export variables marked as public e.g `public let x = 10`
    }
}
