use std::process::ExitCode;
use std::result;
use std::fs;

mod parser;
mod lexer;
mod compiler;

use compiler::Compiler;
use lexer::Lexer;
use parser::parse_statement_toplevel;

type Result<T> = result::Result<T, ()>;

fn start() -> Result<()> {
    let file_path = "main.txt";

    let source_code = fs::read_to_string(file_path).map_err(|e| {
        eprintln!("ERROR: could not open file `{file_path}`: {e}");
    })?;

    let mut lexer = Lexer::new(&source_code, file_path).peekable();

    let mut ast = Vec::new();
    while let Some(stmt) = parse_statement_toplevel(&mut lexer)? {
        ast.push(stmt);
    }

    let mut compiler = Compiler::new();
    println!("{}", compiler.compile_ast(ast)?);

    Ok(())
}

fn main() -> ExitCode {
    match start() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
