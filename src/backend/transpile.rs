use crate::syntax::types::{Numeric, Ty};

#[derive(Debug, Clone)]
pub struct PrettyPrinter {
    indent: String,
    pub lines: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Transpiler {
    printer: PrettyPrinter,
}

#[allow(dead_code)]
impl PrettyPrinter {
    pub fn new() -> Self {
        PrettyPrinter {
            indent: String::new(),
            lines: Vec::new(),
        }
    }

    pub fn emit(&mut self, str: &str) {
        let formatted = format!("{}{}", self.indent, str);
        self.lines.push(formatted);
    }

    pub fn emit_line(&mut self, str: &str) {
        self.emit(str);
    }

    fn indent(&mut self) {
        self.indent.push_str("    ");
    }

    fn dedent(&mut self) {
        self.indent.truncate(self.indent.len() - 4);
    }
}

impl Ty {
    pub fn to_prototype(&self, printer: &PrettyPrinter) -> String {
        // c prototype: return_type (*function_name)(args)
        // match self {
        //     Ty::Function(ret, args) => {
        //         let mut prototype = format!("{} (*_anon_fn)(", ret.to_c_type(printer));
        //         for arg in args {
        //             prototype.push_str(&arg.to_c_type(printer));
        //             prototype.push_str(", ");
        //         }
        //         prototype.push_str(")");
        //         prototype
        //     }
        //     _ => "".to_string(),
        // }
        todo!()
    }

    pub fn to_c_type(&self, printer: &PrettyPrinter) -> String {
        match self {
            Ty::Number(n) => match n {
                Numeric::I16 => "i16".to_string(),
                Numeric::I32 => "i32".to_string(),
                Numeric::I64 => "i64".to_string(),
                Numeric::U16 => "u16".to_string(),
                Numeric::U32 => "u32".to_string(),
                Numeric::U64 => "u64".to_string(),
                Numeric::F32 => "f32".to_string(),
                Numeric::F64 => "f64".to_string(),
            },
            Ty::Bool => "bool".to_string(),
            Ty::Char => "char".to_string(),
            Ty::String => "char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Unchecked => "void".to_string(),
            ///Ty::Function(_ret, _args) => self.to_prototype(printer),
            _ => "void".to_string(),
        }
    }
}

impl Transpiler {}
