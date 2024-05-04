use std::fmt::Write;

use crate::parser::{Expression, Statement};

#[derive(Default)]
struct Scope {
    procs: Vec<String>,
}

pub struct Compiler {
    qbe_code: String,
    strings: Vec<String>,
    scope: Vec<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut scope = Vec::new();
        scope.push(Scope::default());

        Self {
            qbe_code: String::new(),
            strings: Vec::new(),
            scope,
        }
    }

    fn string(&mut self, string: String) -> String {
        let s = format!("str_{}", self.strings.len());
        self.strings.push(string);
        return s;
    }

    fn compile_statement(&mut self, statement: Statement) -> super::Result<()> {
        match statement {
            Statement::ProcDecl {
                loc: _,
                name,
                statements,
            } => {
                self.scope.last_mut().unwrap().procs.push(name.clone());

                let _ = writeln!(self.qbe_code, "function ${name}() {{");
                let _ = writeln!(self.qbe_code, "@start");

                for st in statements {
                    self.compile_statement(st)?;
                }

                let _ = writeln!(self.qbe_code, "ret");
                let _ = writeln!(self.qbe_code, "}}");
            }
            Statement::ProcCall {
                loc,
                name,
                expressions,
            } => match name.as_str() {
                "print" => {
                    if expressions.len() != 1 {
                        eprintln!(
                            "{loc}: ERROR: incorrect amount of arguments for function `{name}`"
                        );
                        return Err(());
                    }

                    let string = match &expressions[0] {
                        Expression::String(string) => string,
                        _ => {
                            eprintln!("{loc}: ERROR: expressions are not allowed as parameters procedure parameters (for now)");
                            return Err(());
                        }
                    };

                    let string = self.string(string.clone());
                    let _ = writeln!(
                        self.qbe_code,
                        "call $printf(l $print_fmt, ..., l ${string})",
                    );
                }
                _ => {
                    if !self.scope.last().unwrap().procs.contains(&name) {
                        eprintln!("{loc}: ERROR: undefined function `{name}`");
                        return Err(());
                    }
                    let _ = writeln!(self.qbe_code, "call ${name}()");
                }
            },
        }
        Ok(())
    }

    pub fn compile_ast(&mut self, ast: Vec<Statement>) -> super::Result<String> {
        for st in ast {
            self.compile_statement(st)?;
        }

        let _ = writeln!(self.qbe_code, "export function w $_start() {{");
        let _ = writeln!(self.qbe_code, "@start");
        let _ = writeln!(self.qbe_code, "call $main()");
        let _ = writeln!(self.qbe_code, "%exit_code =w copy 0");
        let _ = writeln!(self.qbe_code, "call $exit(w %exit_code)");
        let _ = writeln!(self.qbe_code, "ret 0");
        let _ = writeln!(self.qbe_code, "}}");

        for (i, string) in self.strings.iter().enumerate() {
            let _ = writeln!(self.qbe_code, "data $str_{i} = {{ b \"{string}\", b 0 }}");
        }
        let _ = writeln!(self.qbe_code, "data $print_fmt = {{ b \"%s\\n\", b 0 }}");

        Ok(self.qbe_code.clone())
    }
}
