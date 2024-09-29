//! Name resolution pass for the compiler.

use std::collections::{HashMap, HashSet};

use super::module::ModuleInfo;
use super::sema_error::SemanticError;
use crate::syntax::ast::ToplevelStmt;
use crate::syntax::ast_types::{StructureInfo, Ty};
use crate::syntax::lexer::SourceLoc;
use crate::syntax::parser::Parser;

/// Result type for the name resolver
pub(crate) type Return<'r, T> = anyhow::Result<T, SemanticError>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Resolver {
    // Keep track of the current and imported modules
    pub curr_module: Option<ModuleInfo>,
    pub imported_modules: HashMap<String, ModuleInfo>,

    errors: Vec<SemanticError>,
    scopes: Vec<HashMap<String, Ty>>,

    // Keep track of already declared names for redeclaration
    declared: HashSet<String>,
}

impl Resolver {
    pub fn new(module_name: &str) -> Self {
        Resolver {
            curr_module: None,
            imported_modules: HashMap::new(),

            errors: Vec::new(),
            scopes: vec![HashMap::new()],
            declared: HashSet::new(),
        }
    }

    // Reset the resolver state
    pub fn reset(&mut self) {
        self.scopes.clear();
        self.scopes.push(HashMap::new());
        self.declared.clear();
    }

    // Push a new scope onto the stack
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Pop the current scope from the stack
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("Cannot pop the global scope");
    }

    pub fn is_declared(&self, name: &str) -> bool {
        self.declared.contains(name)
    }

    pub fn insert_name(&mut self, name: &str, ty: Ty) -> Return<()> {
        // Each file maps to a module, so we will need to take this into account later

        self.scopes
            .last_mut()
            .expect("No scope to insert into")
            .insert(name.to_string(), ty);

        if self.declared.contains(name) {
            return Err(SemanticError::Redefinition {
                name: name.to_string(),
                location: SourceLoc::default(),
            });
        }

        self.declared.insert(name.to_string());
        Ok(())
    }

    pub fn remove_name(&mut self, name: &str) -> Return<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.remove(name).is_some() {
                return Ok(());
            }
        }

        Err(SemanticError::NotInScope {
            name: name.to_string(),
            location: SourceLoc::default(),
        })
    }

    pub fn update_name(&mut self, name: &str, ty: Ty) -> Return<()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                *entry = ty;
                return Ok(());
            }
        }

        Err(SemanticError::NotInScope {
            name: name.to_string(),
            location: SourceLoc::default(),
        })
    }

    // Import resolution
    // TODO: Implement this
    //

    fn parse_module(&self, filename: &str) -> ModuleInfo {
        // Parse a module into a `ModuleInfo` structure
        //
        let mut module_info = ModuleInfo::new(filename, filename);
        let contents = std::fs::read_to_string(filename).expect("Failed to open file");
        let ast = Parser::parse(&contents).expect("Failed to parse module");

        for node in ast.nodes {
            match node.target {
                ToplevelStmt::Import { path, alias: _ } => {
                    // Parse the module and add it, calling ourselves recursively in the process

                    let normalized_path = path.join("/");
                    let _imported_module = self.parse_module(&normalized_path);

                    println!("Imported module: {:#?}", _imported_module);

                    module_info.imports.push(normalized_path);
                }

                ToplevelStmt::StructDecl { name, fields } => module_info.insert_type(
                    &name,
                    &Ty::Struct(StructureInfo::new_struct_with_fields(&name, fields)),
                ),

                _ => todo!(),
            }
        }

        module_info
    }

    fn resolve_in_curr_module(&self, name: &str) -> Option<&Ty> {
        self.curr_module.as_ref()?.exports.get(name)
    }

    fn resolve_in_imports<'a>(&self, name: &str) -> Option<&Ty> {
        self.imported_modules
            .iter()
            .find_map(|module| module.1.exports.get(name))
    }

    fn resolve_in_scopelist<'a>(&self, name: &str) -> Option<&Ty> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
    }

    fn resolve_nested_field_name<'a>(
        &self,
        mut base_ty: &'a Ty,
        tail: Vec<&str>,
    ) -> Option<&'a Ty> {
        for field_name in tail {
            match base_ty {
                Ty::Struct(ref struct_info) => {
                    base_ty = struct_info.fields.iter().find_map(|(name, ty)| {
                        if name == field_name {
                            Some(ty)
                        } else {
                            None
                        }
                    })?;
                }
                Ty::Enum(ref struct_info) => {
                    base_ty = struct_info.fields.iter().find_map(|(name, ty)| {
                        if name == field_name {
                            Some(ty)
                        } else {
                            None
                        }
                    })?;
                }

                // We didn't find a field
                _ => return None,
            }
        }

        Some(base_ty)
    }

    // Resolve a name in the current scope starting from the innermost scope
    pub fn resolve_name(&self, name: &str) -> Option<&Ty> {
        let parts: Vec<&str> = name.split('.').collect();

        // Resolve the base name (e.g., `Foo` in `Foo.a`) according to the below priorities:
        // 1. First try to resolve it as part of the current module
        // 2. Then try to resolve it as an imported module
        // 3. Finally try to resolve it in the current scope
        let mut base_ty = self
            .resolve_in_curr_module(parts[0])
            .or_else(|| self.resolve_in_imports(parts[0]))
            .or_else(|| self.resolve_in_scopelist(parts[0]))?;

        // Resolve the remaining parts of the name. If the base type is a struct or enum,
        // we resolve it's fields here - otherwise we did not find a field so we return `None`.
        self.resolve_nested_field_name(&mut base_ty, parts[1..].to_vec())
    }
}

#[cfg(test)]
mod resolver_tests {
    use crate::syntax::ast_types::Ty;

    use super::Resolver;

    #[test]
    fn resolve_in_scope() {
        let mut resolver = Resolver::new("test_suite");
        resolver.insert_name("foo", Ty::String).expect("failure");

        assert!(resolver.resolve_name("foo").is_some())
    }

    #[test]
    fn resolve_module_import() {
        let mut resolver = Resolver::new("module_one");
    }
}
