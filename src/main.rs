use std::error::Error;

use clap::Parser as Clap;

use deimos::sema::resolver::Resolver;
use deimos::sema::typecheck::Typechecker;
use deimos::syntax::parser::Parser;

#[derive(clap::Parser, Debug)]
struct Args {
    #[arg(short, long)]
    file: String,
}
fn main() {
    println!("Deimos compiler v0.0.0.3");
    println!("================================================================");
    println!("This compiler is a stage1 compiler, only used for bootstrapping.");
    println!("================================================================");

    let args = Args::parse();

    let src = std::fs::read_to_string(args.file.clone()).expect("Failed to read file");
    drive(&args.file, &src).unwrap_or_else(|e| {
        eprintln!("{}", e);
    });
}

fn print_errors(errors: &[impl std::fmt::Display]) {
    errors.iter().for_each(|e| eprintln!("E: {}", e));
}

fn drive<'a>(filename: &'a str, src: &'a str) -> anyhow::Result<()> {
    let ast = Parser::parse(src).map_err(|e| {
        print_errors(&e);
        anyhow::anyhow!("Parsing failed")
    })?;

    let resolver = Resolver::new("main");

    let mut typecheck = Typechecker::new(resolver);
    if let Err(errors) = typecheck.check(&ast) {
        print_errors(&errors);
        return Err(anyhow::anyhow!("Typechecking failed"));
    }

    Ok(())
}
