use std::error::Error;

use clap::Parser as Clap;

use deimos::middle::module_info::ModuleBuilder;
// use deimos::backend::transpile::Transpiler;
use deimos::middle::typecheck::Typecheck;
use deimos::parser::Parser;

#[derive(clap::Parser, Debug)]
struct Args {
    #[arg(short, long)]
    file: String,
}

fn main() {
    println!("Deimos compiler v0.0.0.2");
    println!("================================================================");
    println!("This compiler is a stage1 compiler, only used for bootstrapping.");
    println!("================================================================");

    let args = Args::parse();

    let src = std::fs::read_to_string(args.file.clone()).expect("Failed to read file");
    drive(&args.file, &src).unwrap_or_else(|e| {
        eprintln!("{}", e);
    });
}

fn print_errors(errors: Vec<impl Error>) {
    errors.iter().for_each(|e| eprintln!("E: {}", e));
}

fn drive<'a>(filename: &'a str, src: &'a str) -> anyhow::Result<()> {
    // There are multiple stages in the compiler
    //
    // 0.   Lexical analysis
    // 1.   Parsing into an AST
    // 2.   Type checking the AST
    // 3.   Collecting module information and building a ModuleInfo data structure ← we are here
    // 4.   Transpiling the AST, along with collected module information, into C code
    // 4.5. Providing a basic standard library
    // 5.   Emitting the C code to a file
    // 6.   Invoking gcc or clang on the C code

    let ast = Parser::parse(src).map_err(|e| {
        print_errors(e);
        anyhow::anyhow!("Parsing failed")
    })?;

    let typed_ast = Typecheck::check(ast).map_err(|e| {
        print_errors(e);
        anyhow::anyhow!("Type checking failed")
    })?;

    //println!("{:#?}", typed_ast);

    let mut module_builder = ModuleBuilder::new();
    let module_info = module_builder.build_module(filename, typed_ast);

    println!("{:#?}", module_builder.get_mangled_info());

    //let c_header = module_builder.build_header(typed_ast);

    //println!("{:}", c_header);

    //println!("{:#?}", typed_ast);

    // Compile the typed AST to C code
    //let compiler = Transpiler::compile(&typed_ast);
    //println!("{}", compiler);

    Ok(())
}
