use std::collections::HashMap;
use std::fmt::Write;

use crate::compiler::Datatype;
use crate::compiler::Ir;
use crate::compiler::Proc;
use crate::compiler::Value;

fn datatype_as_load(datatype: Datatype) -> &'static str {
    match datatype {
        Datatype::I8 => "loadsb",
        Datatype::I16 => "loadsh",
        Datatype::I32 => "loadsw",
        Datatype::I64 | Datatype::String | Datatype::Pointer(_) => "loadl",
        Datatype::None => unreachable!(),
    }
}

fn datatype_as_store(datatype: Datatype) -> &'static str {
    match datatype {
        Datatype::I8 => "storeb",
        Datatype::I16 => "storeh",
        Datatype::I32 => "storew",
        Datatype::I64 | Datatype::String | Datatype::Pointer(_) => "storel",
        Datatype::None => unreachable!(),
    }
}

#[derive(Default)]
pub struct QbeCompiler {
    stack: usize,
    register_stack: usize,
    jumpifnot_count: usize,
    code: String,

    global_variables: HashMap<String, (Datatype, Value)>,
    local_variables: HashMap<String, Datatype>,
    strings: Vec<String>,
}

impl QbeCompiler {
    pub fn new() -> Self {
        Self::default()
    }

    fn string(&mut self, string: String) -> String {
        let i = self.strings.len();
        self.strings.push(string);
        format!("str_{i}")
    }

    fn pop(&mut self) -> usize {
        self.stack -= 1;
        let _ = writeln!(
            self.code,
            "%r{r} =l copy %s{s}",
            r = self.register_stack,
            s = self.stack
        );
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
            let _ = writeln!(self.code, "# Ir::{inst:?}");
            match inst {
                Ir::Proc {
                    proc: Proc { name, instructions },
                    export,
                } => {
                    if export {
                        let _ = write!(self.code, "export ");
                    }
                    let _ = writeln!(self.code, "function ${name}() {{");
                    let _ = writeln!(self.code, "@start");

                    self.local_variables.clear();
                    self.compile_ir_to_qbe(instructions.to_vec());

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
                Ir::Lt => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l csltl %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Gt => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l csgtl %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Le => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l cslel %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Ge => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l csgel %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l ceql %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::Nq => {
                    let b = self.pop();
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l cnel %r{a}, %r{b}");
                    self.reset_registers();
                }
                Ir::GetVar(name) => {
                    let r = self.push();

                    let is_global;
                    let datatype = if let Some(v) = self.global_variables.get(&name) {
                        is_global = true;
                        &v.0
                    } else if let Some(v) = self.local_variables.get(&name) {
                        is_global = false;
                        v
                    } else {
                        unreachable!();
                    };

                    let load = datatype_as_load(datatype.clone());
                    if is_global {
                        let _ = writeln!(self.code, "%s{r} =l {load} $niban_variable_{name}");
                    } else {
                        let _ = writeln!(self.code, "%s{r} =l {load} %niban_variable_{name}");
                    }
                }
                Ir::SetVar(name) => {
                    let a = self.pop();

                    let is_global;
                    let datatype = if let Some(v) = self.global_variables.get(&name) {
                        is_global = true;
                        &v.0
                    } else if let Some(v) = self.local_variables.get(&name) {
                        is_global = false;
                        v
                    } else {
                        unreachable!();
                    };

                    let store = datatype_as_store(datatype.clone());
                    if is_global {
                        let _ = writeln!(self.code, "{store} %r{a}, $niban_variable_{name}");
                    } else {
                        let _ = writeln!(self.code, "{store} %r{a}, %niban_variable_{name}");
                    }

                    self.reset_registers();
                }
                Ir::GlobalVar {
                    name,
                    datatype,
                    initial_value,
                } => {
                    self.global_variables
                        .insert(name.to_string(), (datatype, initial_value.clone()));
                }
                Ir::LocalVar(name, datatype) => {
                    let _ = writeln!(
                        self.code,
                        "%niban_variable_{name} =l alloc4 {size}",
                        size = datatype.get_size()
                    );
                    self.local_variables.insert(name.to_string(), datatype);
                }
                Ir::JumpIfNot(label_id) => {
                    let a = self.pop();
                    let r = self.push();
                    let _ = writeln!(self.code, "%s{r} =l ceql %r{a}, 0");
                    self.reset_registers();

                    let i = self.jumpifnot_count;
                    self.jumpifnot_count += 1;

                    let cond = self.pop();
                    let _ = writeln!(self.code, "jnz %r{cond}, @if{i}, @endif{i}");
                    let _ = writeln!(self.code, "@if{i}");
                    let _ = writeln!(self.code, "jmp @label{label_id}");
                    let _ = writeln!(self.code, "@endif{i}");
                    self.reset_registers();
                }
                Ir::Jump(label_id) => {
                    let _ = writeln!(self.code, "jmp @label{label_id}");
                }
                Ir::Label(id) => {
                    let _ = writeln!(self.code, "@label{id}");
                }
                Ir::AddrOf(var_name) => {
                    let r = self.push();

                    if self.global_variables.contains_key(&var_name) {
                        let _ = writeln!(self.code, "%s{r} =l copy $niban_variable_{var_name}");
                    } else if self.local_variables.contains_key(&var_name) {
                        let _ = writeln!(self.code, "%s{r} =l copy %niban_variable_{var_name}");
                    } else {
                        unreachable!();
                    }
                }
                Ir::Read(datatype) => {
                    let a = self.pop();
                    let r = self.push();

                    let load = datatype_as_load(datatype);
                    let _ = writeln!(self.code, "%s{r} =l {load} %r{a}");

                    self.reset_registers();
                }
                Ir::Write(datatype) => {
                    let addr = self.pop();
                    let value = self.pop();

                    let store = datatype_as_store(datatype);
                    let _ = writeln!(self.code, "{store} %r{value}, %r{addr}");
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

        let mut global_strings = Vec::new();
        for (var, value) in &self.global_variables {
            let _ = write!(self.code, "data $niban_variable_{var} = {{ ");
            match value {
                (datatype, Value::Integer(i)) => {
                    let bytes = i.to_le_bytes();
                    let len = datatype.get_size();

                    for i in 0..len {
                        let _ = write!(self.code, "b {}", bytes[i]);
                        if i != len - 1 {
                            let _ = write!(self.code, ", ");
                        }
                    }
                }
                (datatype, Value::String(string)) => {
                    assert!(datatype == &Datatype::String);

                    let _ = write!(
                        self.code,
                        "l $str_{}",
                        self.strings.len() + global_strings.len()
                    );
                    global_strings.push(string);
                }
            }
            let _ = writeln!(self.code, " }}");
        }

        for s in global_strings.iter() {
            self.strings.push(s.to_string());
        }

        for (i, s) in self.strings.iter().enumerate() {
            let _ = writeln!(self.code, "data $str_{i} = {{ b \"{s}\", b 0 }}");
        }

        self.code.clone()
    }
}
