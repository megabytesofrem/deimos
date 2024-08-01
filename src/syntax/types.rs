#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Number(Numeric),
    Bool,
    Char,
    String,
    Void,
    Unchecked,

    Function(Box<Ty>, Vec<Ty>),

    // Arrays decay into pointers ala C
    Pointer(Box<Ty>),
    Array(Box<Ty>),

    // (ty1, ty2, ...)
    Tuple(Vec<Ty>),

    // Optional types are implemented at a compiler level
    Optional(Box<Ty>),

    Struct {
        name: String,
        fields: Vec<(String, Ty)>,
    },

    Enum {
        name: String,
        fields: Vec<String>,
    },

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
            (Ty::Struct { name, fields: _ }, Ty::UserDefined(other_name))
            | (Ty::UserDefined(other_name), Ty::Struct { name, fields: _ }) => {
                name == other_name || name == "_anon"
            }

            // TODO: Check if the enum fields match
            (Ty::Enum { name, fields: _ }, Ty::UserDefined(other_name))
            | (Ty::UserDefined(other_name), Ty::Enum { name, fields: _ }) => {
                name == other_name || name == "_anon"
            }
            _ => false,
        }
    }
}
