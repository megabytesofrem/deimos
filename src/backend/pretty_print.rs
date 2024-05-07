#[derive(Debug, Clone, PartialEq)]
pub(crate) struct PrettyPrinter {
    indent: String,
    pub lines: Vec<String>,
}

// Pretty printer for the generated C code
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
        self.emit("\n");
    }

    fn increase_indent(&mut self) {
        self.indent.push_str("    "); // Assuming 4 spaces for indentation
    }

    fn decrease_indent(&mut self) {
        self.indent.truncate(self.indent.len() - 4); // Remove the last 4 spaces
    }
}
