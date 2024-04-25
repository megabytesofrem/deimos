use std::error::Error;

use deimos::codegen::transpile::Transpiler;
use deimos::parser::Parser;
use deimos::semant::typechk::Typeck;

fn main() {
    println!("Deimos compiler v0.0.0.2");
    println!("================================================================");
    println!("This compiler is a stage1 compiler, only used for bootstrapping.");
    println!("================================================================");

    let src = std::fs::read_to_string("test/ifelse.dms").expect("Failed to read file");
    drive(&src).unwrap_or_else(|e| {
        eprintln!("{}", e);
    });
}

fn print_errors(errors: Vec<impl Error>) {
    errors.iter().for_each(|e| eprintln!("{}", e));
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

    println!("{:#?}", typed_ast);

    // Compile the typed AST to C code
    let compiler = Transpiler::compile(&typed_ast);
    println!("{}", compiler);

    Ok(())
}
