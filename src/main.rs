use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::{Command, ExitCode};
use std::result;

mod compiler;
mod lexer;
mod parser;
mod qbe_generator;

use compiler::Compiler;
use lexer::Lexer;
use parser::parse_statement_toplevel;

use crate::qbe_generator::QbeCompiler;

type Result<T> = result::Result<T, ()>;

static RUNTIME_ASM: &str = include_str!("runtime_linux.asm");

fn usage(program: &str, error: bool) {
    let usage = format!(
        r#"Usage: {program} [OPTIONS] <input_file>
    -o,   --output  <output_file>    Use <output_file> as the output file path
    -h,   --help                     Prints this help and exits
    -lib, --library                  Don't create an entry point"#
    );

    if error {
        eprintln!("{usage}");
    } else {
        println!("{usage}");
    }
}

fn compile_file(file_path: &str, library: bool) -> Result<String> {
    let source_code = fs::read_to_string(&file_path).map_err(|e| {
        eprintln!("ERROR: could not open file `{file_path}`: {e}");
    })?;

    let mut lexer = Lexer::new(&source_code, &file_path).peekable();

    let mut ast = Vec::new();
    while let Some(stmt) = parse_statement_toplevel(&mut lexer)? {
        ast.push(stmt);
    }

    let mut compiler = Compiler::new();
    let mut generator = QbeCompiler::new();
    Ok(generator.compile(compiler.compile_ast(ast)?, library))
}

fn print_command_quoted(command: &[&str]) {
    print!("[CMD] ");

    for (i, arg) in command.iter().enumerate() {
        if arg.contains(" ") {
            print!("\"{arg}\"");
        } else {
            print!("{arg}");
        }

        if i != command.len() - 1 {
            print!(" ");
        }
    }
    println!();
}

fn run_command(command: &[&str]) -> Result<()> {
    print_command_quoted(command);

    let status = Command::new(command[0])
        .args(&command[1..])
        .status()
        .map_err(|e| {
            eprintln!("ERROR: failed to get status for command: {e}");
        })?;

    if !status.success() {
        eprintln!("ERROR: command failed to execute");
        return Err(());
    }

    Ok(())
}

fn start() -> Result<()> {
    let mut args = std::env::args();

    let program = if let Some(prog) = args.next() {
        prog
    } else {
        "niban".to_string()
    };

    let mut file_path = None;
    let mut output_file_path = None;
    let mut library = false;

    loop {
        let arg = if let Some(arg) = args.next() {
            arg
        } else {
            break;
        };

        match arg.as_str() {
            "-h" | "--help" => {
                usage(&program, false);
                return Ok(());
            }
            "-o" | "--output" => {
                let out = if let Some(arg) = args.next() {
                    arg
                } else {
                    usage(&program, true);
                    eprintln!("ERROR: no output file was provided");
                    return Err(());
                };

                output_file_path = Some(out);
            }
            "-lib" | "--library" => library = true,
            _ => file_path = Some(arg.clone()),
        }
    }

    let file_path = if let Some(fpath) = file_path {
        fpath
    } else {
        usage(&program, true);
        eprintln!("ERROR: no input file was provided");
        return Err(());
    };

    let output_file_path = if let Some(o) = output_file_path {
        o
    } else {
        if let Some(out_path) = Path::new(&file_path).file_stem() {
            let out_path = out_path.to_str().unwrap().to_string();
            if out_path == file_path {
                format!("{out_path}.out")
            } else {
                out_path
            }
        } else {
            format!("{file_path}.out")
        }
    };

    let qbe_code = compile_file(&file_path, library)?;

    let object_file_path = format!("{output_file_path}.o");
    {
        let asm_file_path = format!("{output_file_path}.asm");
        let output_file_path = format!("{output_file_path}.ssa");

        let mut output = fs::File::create(&output_file_path).map_err(|e| {
            eprintln!("ERROR: could not create file `{output_file_path}`: {e}");
        })?;
        write!(output, "{qbe_code}").map_err(|e| {
            eprintln!("ERROR: could not write into file `{output_file_path}`: {e}");
        })?;

        run_command(&["qbe", &output_file_path, "-o", &asm_file_path])?;
        run_command(&["as", "-o", &object_file_path, &asm_file_path])?;
    }

    let runtime_file_path = format!("{output_file_path}_runtime.o");
    {
        let output_file_path = format!("{output_file_path}_runtime.asm");

        let mut output = fs::File::create(&output_file_path).map_err(|e| {
            eprintln!("ERROR: could not create file `{output_file_path}`: {e}");
        })?;
        write!(output, "{RUNTIME_ASM}").map_err(|e| {
            eprintln!("ERROR: could not write into file `{output_file_path}`: {e}");
        })?;

        run_command(&["fasm", &output_file_path, &runtime_file_path])?;
    }

    if library {
        let output_file_path = format!("{output_file_path}.a");
        run_command(&["ar", "rcs", &output_file_path, &runtime_file_path, &object_file_path])?;
    } else {
        run_command(&["ld", "-o", &output_file_path, &runtime_file_path, &object_file_path])?;
    }

    Ok(())
}

fn main() -> ExitCode {
    match start() {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
