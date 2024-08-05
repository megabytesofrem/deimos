use crate::middle::typed_ast::TBlock;

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Number(Numeric),
    Bool,
    Char,
    String,
    Void,
    Unchecked,

    Function(Box<FunctionInfo>),

    // Arrays decay into pointers ala C
    Pointer(Box<Ty>),
    Array(Box<Ty>),

    // (ty1, ty2, ...)
    Tuple(Vec<Ty>),

    // Optional types are implemented at a compiler level
    Optional(Box<Ty>),

    Struct(StructureInfo),

    Enum(StructureInfo),

    UserDefined(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Numeric {
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
    pub return_type: Ty,

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

    pub fn can_cast_into(&self, other: &Ty) -> bool {
        match (self, other) {
            (_, other) if self == other => true,
            (Ty::Number(_), Ty::Number(_)) => true,
            (Ty::Pointer(_), Ty::Pointer(_)) => true,
            (Ty::Array(_), Ty::Array(_)) => true,
            (Ty::UserDefined(_), Ty::UserDefined(_)) => true,
            (_, Ty::Unchecked) | (Ty::Unchecked, _) => true,
            (_, Ty::Void) | (Ty::Void, _) => true,

            // TODO: Check if the struct fields match
            (Ty::Struct(info), Ty::UserDefined(other_name))
            | (Ty::UserDefined(other_name), Ty::Struct(info)) => {
                info.name == *other_name || info.name == "_anon"
            }

            // TODO: Check if the enum fields match
            (Ty::Enum(info), Ty::UserDefined(other_name))
            | (Ty::UserDefined(other_name), Ty::Enum(info)) => {
                info.name == *other_name || info.name == "_anon"
            }
            _ => false,
        }
    }
}
