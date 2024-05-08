use std::fmt::Write;

use crate::compiler::Ir;
use crate::compiler::Proc;

pub struct QbeCompiler {
    stack: usize,
    register_stack: usize,
    code: String,
    strings: Vec<String>,

    variables: Vec<String>,
    global_variables: Vec<String>,
}

impl QbeCompiler {
    pub fn new() -> Self {
        Self {
            stack: 0,
            register_stack: 0,
            code: String::new(),
            strings: Vec::new(),
            variables: Vec::new(),
            global_variables: Vec::new(),
        }
    }

    fn string(&mut self, string: String) -> String {
        let i = self.strings.len();
        self.strings.push(string);
        format!("str_{i}")
    }

    fn pop(&mut self) -> usize {
        self.stack -= 1;
        let _ = writeln!(self.code, "%r{r} =l copy %s{s}", r = self.register_stack, s = self.stack);
        self.register_stack += 1;
        return self.register_stack - 1;
    }

    fn reset_registers(&mut self) {
        self.register_stack = 0;
    }

    fn push(&mut self) -> usize {
        self.stack += 1;
        self.stack - 1
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

                    self.variables.clear();
                    let _ = writeln!(self.code, "ret");
                    let _ = writeln!(self.code, "}}");
                }
                Ir::PushInt(i) => {
                    let index = self.push();
                    let _ = writeln!(self.code, "%s{index} =l copy {i}");
                }
                Ir::PushString(s) => {
                    let index = self.push();
                    let string = self.string(s.clone());
                    let _ = writeln!(self.code, "%s{index} =l copy ${string}");
                }
                Ir::ProcCall(proc) => {
                    let _ = writeln!(self.code, "call ${proc}()");
                }
                Ir::PrintInt => {
                    let a = self.pop();
                    let _ = writeln!(self.code, "call $print_unsigned(l %r{a})");
                    self.reset_registers();
                }
                Ir::PrintString => {
                    let a = self.pop();
                    let _ = writeln!(self.code, "call $print_string(l %r{a})");
                    self.reset_registers();
                }
                Ir::Plus => {
                    let a = self.pop();
                    let b = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l add %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Minus => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l sub %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Mult => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l mul %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Div => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l div %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Mod => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l rem %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::GetVar(name) => {
                    let r = self.push();
                    if self.variables.contains(&name) {
                        let _ = writeln!(self.code, "%s{r} =l loadl %niban_variable_{name}");
                    } else if self.global_variables.contains(&name) {
                        let _ = writeln!(self.code, "%s{r} =l loadl $niban_variable_{name}");
                    } else {
                        unreachable!();
                    }
                }
                Ir::SetVar(name) => {
                    let a = self.pop();
                    if self.variables.contains(&name) {
                        let _ = writeln!(self.code, "storel %r{a}, %niban_variable_{name}");
                    } else if self.global_variables.contains(&name) {
                        let _ = writeln!(self.code, "storel %r{a}, $niban_variable_{name}");
                    } else {
                        unreachable!();
                    }
                }
                Ir::GlobalVar(name, _) => {
                    self.global_variables.push(name);
                }
                Ir::LocalVar(name, _) => {
                    self.variables.push(name.clone());
                    let _ = writeln!(self.code, "%niban_variable_{name} =l alloc4 8");
                }
            }
        }
    }

    pub fn compile(&mut self, ir: Vec<Ir>, library: bool) -> String {
        self.compile_ir_to_qbe(ir);

        if !library {
            let _ = writeln!(self.code, "export function $_start() {{");
            let _ = writeln!(self.code, "@start");
            let _ = writeln!(self.code, "call $main()");
            let _ = writeln!(self.code, "call $exit(w 0)");
            let _ = writeln!(self.code, "ret");
            let _ = writeln!(self.code, "}}");
        }

        for (i, s) in self.strings.iter().enumerate() {
            let _ = writeln!(self.code, "data $str_{i} = {{ b \"{s}\", b 0 }}");
        }

        for var in self.global_variables.iter() {
            let _ = writeln!(self.code, "data $niban_variable_{var} = {{ z 8 }}");
        }

        self.code.clone()
    }
}
