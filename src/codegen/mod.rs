use crate::syntax::ast::Literal;

#[derive(Debug, Clone, PartialEq)]
pub struct Compiler {
    code: String,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler { code: String::new() }
    }
    
    fn emit(&mut self, code: &str) {
        self.code.push_str(code);
    }

    fn bool_to_c(&mut self, b: bool) -> String {
        match b {
            true => "TRUE".to_string(),
            false => "FALSE".to_string(),
        }
    }

    fn compile_literal(&mut self, literal: Literal) {
        match literal {
            Literal::Int(i) => self.emit(&format!("{}", i)),
            Literal::Float(f) => self.emit(&format!("{}f", f)),
            Literal::Double(f) => self.emit(&format!("{}", f)),
            Literal::Bool(b) => self.emit(&format!("{}", b)),
            Literal::String(s) => self.emit(&format!("{:?}", s)),
        }
    }

    pub fn compile(&self) {
        println!("Compiling file");
    }
}
