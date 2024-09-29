use crate::sema::{sema_error::SemanticError, typed_ast::TBlock};
use serde::{Deserialize, Serialize};

use super::lexer::SourceLoc;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    // A sized number type
    Number(SizedNumber),

    Bool,
    Char,
    String,
    Void,

    // Types that are not known yet are marked as unchecked, to be resolved later.
    // This was a hold over from the original design, but is not currently used
    Unchecked,

    Function(Box<FunctionInfo>),

    // A type variable (used in type inference)
    TVar(usize),

    // Arrays decay into pointers ala C
    Pointer(Box<Ty>),
    Array(Box<Ty>),

    // Optional types are implemented at a compiler level
    Optional(Box<Ty>),

    Struct(StructureInfo),
    Enum(StructureInfo),

    // A user defined type (alias of a struct or enum)
    UserDefined(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SizedNumber {
    I16,
    I32,
    I64,
    U16,
    U32,
    U64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StructureKind {
    Struct,
    Enum,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructureInfo {
    // Kind of data structure (struct or enum)
    pub kind: StructureKind,

    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
    pub params: Vec<(String, Ty)>,
    pub return_ty: Ty,

    // Optional function body
    pub body: Option<TBlock>,
}

// Type conversion rules used in the typechecker

impl Ty {
    pub fn is_primitive(&self) -> bool {
        matches!(self, Ty::Number(_) | Ty::Bool | Ty::Char | Ty::String)
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Ty::Number(_))
    }

    pub fn is_signed_int(&self) -> bool {
        matches!(
            self,
            Ty::Number(SizedNumber::I16)
                | Ty::Number(SizedNumber::I32)
                | Ty::Number(SizedNumber::I64)
        )
    }

    pub fn is_unsigned_int(&self) -> bool {
        matches!(
            self,
            Ty::Number(SizedNumber::U16)
                | Ty::Number(SizedNumber::U32)
                | Ty::Number(SizedNumber::U64)
        )
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Ty::Pointer(_))
    }

    pub fn is_index_type(&self) -> bool {
        matches!(self, Ty::Number(_) | Ty::UserDefined(_))
    }

    pub fn is_indexable_type(&self) -> bool {
        matches!(self, Ty::Array(_) | Ty::Pointer(_) | Ty::UserDefined(_))
    }
}

impl StructureInfo {
    pub fn new_struct(name: &str) -> Self {
        StructureInfo {
            kind: StructureKind::Struct,
            name: name.to_string(),
            fields: vec![],
        }
    }

    pub fn new_enum(name: &str) -> Self {
        StructureInfo {
            kind: StructureKind::Enum,
            name: name.to_string(),
            fields: vec![],
        }
    }

    /// Helper function to construct a struct with populated fields
    pub fn new_struct_with_fields(name: &str, fields: Vec<(String, Ty)>) -> Self {
        StructureInfo {
            kind: StructureKind::Struct,
            name: name.to_string(),
            fields,
        }
    }

    /// Helper function to construct a enum with populated fields
    pub fn new_enum_with_fields(name: &str, fields: Vec<(String, Ty)>) -> Self {
        StructureInfo {
            kind: StructureKind::Enum,
            name: name.to_string(),
            fields,
        }
    }

    pub fn insert_field(&mut self, field_name: &str, field_ty: &Ty) {
        self.fields.push((field_name.to_string(), field_ty.clone()));
    }

    /// Look up a field in a struct or enum and return its type (or an error)
    pub fn lookup_field(&self, field_name: &str, location: SourceLoc) -> Result<Ty, SemanticError> {
        self.fields
            .iter()
            .find(|(name, _)| name == field_name)
            .map(|(_, ty)| ty.clone())
            .ok_or_else(|| SemanticError::FieldNotFound {
                struct_name: self.name.clone(),
                field: field_name.to_string(),
                location,
            })
    }
}

impl FunctionInfo {
    pub fn has_generics(&self) -> bool {
        // Check if any parameter is a type variable
        if self.params.iter().any(|(_, ty)| Self::contains_typevar(ty)) {
            return true;
        }

        // Check if the return type is a type variable
        Self::contains_typevar(&self.return_ty)
    }

    fn contains_typevar(ty: &Ty) -> bool {
        // Helper function to check whether a type contains any type variables
        match ty {
            Ty::TVar(_) => true,
            Ty::Function(f) => {
                f.params.iter().any(|(_, t)| Self::contains_typevar(t))
                    || Self::contains_typevar(&f.return_ty)
            }
            Ty::Pointer(inner) | Ty::Array(inner) | Ty::Optional(inner) => {
                Self::contains_typevar(inner)
            }
            Ty::Struct(s) | Ty::Enum(s) => {
                s.fields.iter().any(|(_, ty)| (Self::contains_typevar(ty)))
            }
            _ => false,
        }
    }

    pub fn named_function(name: String, params: Vec<(String, Ty)>, return_ty: Ty) -> Self {
        Self {
            name,
            params,
            return_ty,
            body: None,
        }
    }
}
