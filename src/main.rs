use std::error::Error;

use deimos::codegen::transpile::Transpiler;
use deimos::parser::Parser;
use deimos::semant::typechk::Typeck;

fn main() {
    println!("Deimos compiler v0.0.0.2");
    println!("================================================================");
    println!("This compiler is a stage1 compiler, only used for bootstrapping.");
    println!("    The generated code may be atrocious as a result of this.    ");
    println!("================================================================");

    let src = std::fs::read_to_string("test/io.dms").expect("Failed to read file");

    drive(&src).unwrap_or_else(|e| {
        eprintln!("{}", e);
    });
}

fn print_errors(errors: Vec<impl Error>) {
    for e in errors {
        eprintln!("{}", e);
    }
}

fn drive(src: &str) -> anyhow::Result<()> {
    let ast = Parser::parse(src).map_err(|e| {
        print_errors(e);
        anyhow::anyhow!("Parsing failed")
    })?;

    let typed_ast = Typeck::check(ast).map_err(|e| {
        print_errors(e);
        anyhow::anyhow!("Type checking failed")
    })?;

    // Compile the typed AST to C code
    let compiler = Transpiler::compile(&typed_ast);
    println!("{}", compiler);

    Ok(())
}
