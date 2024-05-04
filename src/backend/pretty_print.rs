use crate::middle::typed_ast::{TBlock, TExpr, TStmt, TToplevelStmt, TypedAst};
use crate::syntax::ast::{Literal, Numeric, Ty};

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

    pub(crate) fn to_typename(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Numeric(Numeric::I16) => "i16".to_string(),
            Ty::Numeric(Numeric::I32) => "i32".to_string(),
            Ty::Numeric(Numeric::I64) => "i64".to_string(),
            Ty::Numeric(Numeric::U16) => "u16".to_string(),
            Ty::Numeric(Numeric::U32) => "u32".to_string(),
            Ty::Numeric(Numeric::U64) => "u64".to_string(),
            Ty::Numeric(Numeric::F32) => "f32".to_string(),
            Ty::Numeric(Numeric::F64) => "f64".to_string(),
            Ty::Bool => "bool".to_string(),

            // TODO: Later on replace with the proper, native string type
            Ty::String => "char *".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Function(ret_ty, params) => self.gen_function_pointer(ret_ty, &params),
            Ty::Array(ty) => format!("{}*", self.to_typename(ty)),
            Ty::Pointer(ty) => format!("{}*", self.to_typename(ty)),
            Ty::UserDefined(name) => name.to_string(),
            _ => unimplemented!("typename"),
        }
    }

    fn gen_function_pointer(&mut self, ret_ty: &Ty, params: &Vec<Ty>) -> String {
        let mut code = String::new();
        code.push_str("(");
        for (index, param) in params.iter().enumerate() {
            code.push_str(&self.to_typename(param));

            if index < params.len() - 1 {
                code.push_str(", ");
            }
        }
        code.push_str(")(");
        code.push_str(&self.to_typename(ret_ty));
        code.push_str(")");
        code
    }

    fn gen_literal(&mut self, lit: &Literal) -> String {
        match lit {
            Literal::Int(i) => i.to_string(),
            Literal::Float32(f) => f.to_string(),
            Literal::Float64(d) => d.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::String(s) => format!("{:?}", s),
        }
    }

    fn gen_expr(&mut self, expr: &TExpr) -> String {
        match expr {
            TExpr::Literal(lit, _) => self.gen_literal(lit),
            TExpr::Name(name, _) => name.clone(),
            TExpr::BinOp(lhs, op, rhs) => {
                let lhs_str = self.gen_expr(&lhs.target);
                let rhs_str = self.gen_expr(&rhs.target);
                format!("({} {} {})", lhs_str, op.to_str(), rhs_str)
            }
            TExpr::UnOp(op, expr) => {
                let expr_str = self.gen_expr(&expr.target);
                format!("{}({})", op.to_str(), expr_str)
            }
            TExpr::Array { elems } => {
                let elems_str = elems
                    .iter()
                    .map(|elem| self.gen_expr(&elem.target))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{{}}}", elems_str)
            }
            TExpr::StructCons { fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, expr)| format!(".{} = {}", name, self.gen_expr(&expr.target)))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{{{}}}", fields_str)
            }
            TExpr::ArrayIndex { array, index } => {
                let array_str = self.gen_expr(&array.target);
                let index_str = self.gen_expr(&index.target);
                format!("{}[{}]", array_str, index_str)
            }
            TExpr::Call { callee, args } => {
                let callee_str = self.gen_expr(&callee.target);
                let args_str = args
                    .iter()
                    .map(|arg| self.gen_expr(&arg.target))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}({})", callee_str, args_str)
            }

            // Add more cases as needed
            _ => unimplemented!("texpr"),
        }
    }

    fn gen_stmt(&mut self, stmt: &TStmt) -> String {
        match stmt {
            TStmt::Expr(expr) => {
                let expr_str = self.gen_expr(&expr);
                format!("{};", expr_str)
            }
            TStmt::Return(expr) => {
                let expr_str = expr
                    .as_ref()
                    .map(|e| self.gen_expr(&e.target))
                    .unwrap_or_default();
                format!("return {};", expr_str)
            }
            TStmt::Let { name, ty, value } => {
                let ty_str = self.to_typename(&ty.clone().unwrap_or(Ty::Void));
                let value_str = value
                    .as_ref()
                    .map(|v| self.gen_expr(&v.target))
                    .unwrap_or_default();
                format!("{} {} = {};", ty_str, name, value_str)
            }
            TStmt::Assign { target, value } => {
                let target_str = self.gen_expr(&target);
                let value_str = self.gen_expr(&value.target);
                format!("{} = {};", target_str, value_str)
            }
            TStmt::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond_str = self.gen_expr(&cond.target);
                let then_block_str = self.gen_block(then_block);
                let else_block_str = else_block
                    .as_ref()
                    .map(|block| self.gen_block(block))
                    .unwrap_or_default();

                let else_str = if else_block.is_some() {
                    format!("else {}", else_block_str)
                } else {
                    String::new()
                };

                format!("if ({}) {} {}", cond_str, then_block_str, else_str)
            }
            // Add more cases as needed
            _ => unimplemented!("tstmt"),
        }
    }

    fn gen_block(&mut self, block: &TBlock) -> String {
        let mut block_str = String::new();
        self.increase_indent();
        block_str.push_str("{\n");
        for stmt in &block.clone() {
            let stmt_str = self.gen_stmt(&stmt.target);
            block_str.push_str(&format!("{}{}\n", self.indent, &stmt_str));
        }
        block_str.push_str("}");
        self.decrease_indent();
        block_str
    }

    pub fn gen_toplevel_stmt(&mut self, stmt: &TToplevelStmt) -> String {
        match stmt {
            TToplevelStmt::FunctionDecl {
                name,
                params,
                return_ty,
                body,
            } => {
                let params_str = params
                    .iter()
                    .map(|(name, ty)| format!("{} {}", self.to_typename(ty), name))
                    .collect::<Vec<String>>()
                    .join(", ");
                let body_str = self.gen_block(body);
                format!(
                    "{} {} ({}) {}",
                    self.to_typename(return_ty),
                    name,
                    params_str,
                    body_str
                )
            }
            TToplevelStmt::ExternDecl {
                name,
                params,
                return_ty,
            } => {
                let params_str = params
                    .iter()
                    .map(|(name, ty)| format!("{} {}", self.to_typename(ty), name))
                    .collect::<Vec<String>>()
                    .join(", ");

                // Add a comment marking the extern declaration to track it in the generated code
                format!(
                    "// extern {} {} ({})\n",
                    self.to_typename(return_ty),
                    name,
                    params_str
                )
            }
            TToplevelStmt::EnumDecl { name, fields } => {
                let fields_str = fields.join(", ");
                format!("typedef enum {} {{\n {} \n}} {}; ", name, fields_str, name)
            }
            TToplevelStmt::StructDecl { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(name, ty)| format!("{} {}", self.to_typename(ty), name))
                    .collect::<Vec<String>>()
                    .join(";\n");
                format!(
                    "typedef struct {} {{\n {} \n}} {}; ",
                    name, fields_str, name
                )
            }

            TToplevelStmt::Stmt(stmt) => self.gen_stmt(&stmt.target),
            _ => unimplemented!("tstmt"),
        }
    }
}
