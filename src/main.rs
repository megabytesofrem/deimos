use deimos::codegen::Compiler;
use deimos::semant::typechk::{TypechkError, Typeck};
use deimos::syntax::errors::SyntaxError;
use deimos::syntax::parse::Parser;

fn main() {
    println!("Deimos compiler v0.0.0.1");
    println!("----------------------------------------------------------------");
    println!("This compiler is a stage1 compiler, only used for bootstrapping.");
    println!("----------------------------------------------------------------");

    let src = std::fs::read_to_string("syntax_tests/test.ds").expect("Failed to read file");
    drive(&src);
}

fn drive(src: &str) {
    let mut syntax_errors: Vec<SyntaxError> = Vec::new();
    let mut tc_errors: Vec<TypechkError> = Vec::new();

    let ast = Parser::parse(src);
    if ast.is_err() {
        syntax_errors.push(ast.clone().err().unwrap());
    }

    let typed_ast = Typeck::check(ast.unwrap());
    if typed_ast.is_err() {
        tc_errors.push(typed_ast.clone().err().unwrap());
    }

    if !syntax_errors.is_empty() {
        for error in syntax_errors {
            println!("SyntaxError: {}", error);
        }
    }

    if !tc_errors.is_empty() {
        for error in tc_errors {
            println!("TypeckError: {}", error);
        }
    }

    if let Ok(ast) = typed_ast {
        let compiler = Compiler::compile(&ast);

        println!("Successfully compiled!");
        println!("Output: ");
        println!("----------------------------------------------------------------");
        println!("{}", compiler.code);
    }
}