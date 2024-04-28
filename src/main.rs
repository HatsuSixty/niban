use std::process::ExitCode;
use std::result;
use std::fs;

mod lexer;

use lexer::Lexer;

type Result<T> = result::Result<T, ()>;

fn start() -> Result<()> {
    let file_path = "main.txt";

    let source_code = fs::read_to_string(file_path).map_err(|e| {
        eprintln!("ERROR: could not open file `{file_path}`: {e}");
    })?;

    let mut lexer = Lexer::new(&source_code, file_path);
    while let Some(token) = lexer.next_token()? {
        println!("{token}");
    }

    Ok(())
}

fn main() -> ExitCode {
    match start() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
