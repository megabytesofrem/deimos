use crate::sema::{sema_error::SemanticError, typed_ast::TBlock};
use serde::{Deserialize, Serialize};

use super::lexer::SourceLoc;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Ty {
    // A sized number type
    Number(NumericSize),

    Bool,
    Char,
    String,
    Void,

    // Types that are not known yet are marked as unchecked, to be resolved later.
    // This was a hold over from the original design, but is not currently used
    Unchecked,

    // Type variable used for generics later down the line
    TVar(String),

    Function(Box<FunctionInfo>),

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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum NumericSize {
    I16,
    I32,
    I64,
    U16,
    U32,
    U64,
    F32,
    F64,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum StructureKind {
    Struct,
    Enum,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct StructureInfo {
    // Kind of data structure (struct or enum)
    pub kind: StructureKind,

    pub name: String,
    pub fields: Vec<(String, Ty)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
    pub fn named_function(name: String, params: Vec<(String, Ty)>, return_ty: Ty) -> Self {
        Self {
            name,
            params,
            return_ty,
            body: None,
        }
    }
}
