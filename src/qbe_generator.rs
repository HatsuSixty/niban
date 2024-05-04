use std::fmt::Write;

use crate::compiler::Ir;
use crate::compiler::Proc;

pub struct QbeCompiler {
    stack: usize,
    code: String,
    strings: Vec<String>,
}

impl QbeCompiler {
    pub fn new() -> Self {
        Self {
            stack: 0,
            code: String::new(),
            strings: Vec::new(),
        }
    }

    fn string(&mut self, string: String) -> String {
        let i = self.strings.len();
        self.strings.push(string);
        format!("str_{i}")
    }

    fn compile_ir_to_qbe(&mut self, ir: Vec<Ir>) {
        for inst in ir {
            match inst {
                Ir::Proc(Proc { name, instructions }) => {
                    if name == "main" {
                        let _ = write!(self.code, "export ");
                    }
                    let _ = writeln!(self.code, "function ${name}() {{");
                    let _ = writeln!(self.code, "@start");

                    self.compile_ir_to_qbe(instructions);

                    let _ = writeln!(self.code, "ret");
                    let _ = writeln!(self.code, "}}");
                }
                Ir::PushInt(i) => {
                    let _ = writeln!(self.code, "%s{s} =l copy {i}", s = self.stack);
                    self.stack += 1;
                }
                Ir::PushString(s) => {
                    let string = self.string(s.clone());
                    let _ = writeln!(self.code, "%s{stack} =l copy ${string}", stack = self.stack);
                    self.stack += 1;
                }
                Ir::ProcCall(proc) => {
                    let _ = writeln!(self.code, "call ${proc}()");
                }
                Ir::PrintInt => {
                    let value_index = self.stack;

                    let _ = writeln!(
                        self.code,
                        "%s{s} =l copy %s{v}",
                        s = self.stack,
                        v = self.stack - 1
                    );
                    self.stack -= 1;

                    let _ = writeln!(
                        self.code,
                        "call $printf(l $printf_int_fmt, ..., l %s{value_index})"
                    );
                }
                Ir::PrintString => {
                    let value_index = self.stack;

                    let _ = writeln!(
                        self.code,
                        "%s{s} =l copy %s{v}",
                        s = self.stack,
                        v = self.stack - 1
                    );
                    self.stack -= 1;

                    let _ = writeln!(
                        self.code,
                        "call $printf(l $printf_string_fmt, ..., l %s{value_index})"
                    );
                }
                Ir::Plus => {
                    let a = self.stack;
                    let _ = writeln!(self.code, "%s{s} =l copy %s{v}", s = self.stack, v = self.stack - 1);
                    self.stack -= 1;

                    let b = self.stack;
                    let _ = writeln!(self.code, "%s{s} =l copy %s{v}", s = self.stack, v = self.stack - 1);
                    self.stack -= 1;

                    let _ = writeln!(self.code, "%s{s} =l add %s{a}, %s{b}", s = self.stack);
                    self.stack += 1;
                }
                Ir::Minus => todo!(),
                Ir::Mult => todo!(),
                Ir::Div => todo!(),
                Ir::Mod => todo!(),
            }
        }
    }

    pub fn compile(&mut self, ir: Vec<Ir>) -> String {
        self.compile_ir_to_qbe(ir);

        let _ = writeln!(self.code, "export function $_start() {{");
        let _ = writeln!(self.code, "@start");
        let _ = writeln!(self.code, "call $main()");
        let _ = writeln!(self.code, "call $exit(w 0)");
        let _ = writeln!(self.code, "ret");
        let _ = writeln!(self.code, "}}");

        let _ = writeln!(self.code, "data $printf_string_fmt = {{ b \"%s\\n\", b 0 }}");
        let _ = writeln!(self.code, "data $printf_int_fmt = {{ b \"%lld\\n\", b 0 }}");

        for (i, s) in self.strings.iter().enumerate() {
            let _ = writeln!(self.code, "data $str_{i} = {{ b \"{s}\", b 0 }}");
        }

        self.code.clone()
    }
}
