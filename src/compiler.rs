use std::collections::HashMap;

use crate::parser::{Expression, ExpressionKind, Operator, Statement, StatementKind};

#[derive(Debug, Clone)]
pub struct Proc {
    instructions: Vec<Ir>,
}

enum Datatype {
    Integer,
    String,
    None,
}

#[derive(Debug, Clone)]
pub enum Ir {
    Proc(Proc),
    PushInt(i64),
    PushString(String),
    ProcCall(String),
    PrintInt,
    PrintString,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

impl Ir {
    fn datatype(&self) -> Datatype {
        match self {
            Self::Proc(_) => Datatype::None,
            Self::PushInt(_) => Datatype::Integer,
            Self::PushString(_) => Datatype::String,
            Self::ProcCall(_) => Datatype::None,
            Self::PrintInt => Datatype::None,
            Self::PrintString => Datatype::None,
            Self::Plus => Datatype::Integer,
            Self::Minus => Datatype::Integer,
            Self::Mult => Datatype::Integer,
            Self::Div => Datatype::Integer,
            Self::Mod => Datatype::Integer,
        }
    }
}

#[derive(Default)]
struct Scope {
    procs: HashMap<String, Proc>,
}

pub struct Compiler {
    scope: Vec<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut scope = Vec::new();
        scope.push(Scope::default());

        Self { scope }
    }

    fn compile_expression_impl(
        &mut self,
        expression: Expression,
        level: usize,
    ) -> super::Result<Vec<Ir>> {
        let mut ir = Vec::new();

        match expression {
            Expression { expression, loc } => match expression {
                ExpressionKind::Binary { kind, left, right } => {
                    for inst in self.compile_expression_impl(*left, level + 1)? {
                        ir.push(inst);
                    }

                    for inst in self.compile_expression_impl(*right, level + 1)? {
                        ir.push(inst);
                    }

                    ir.push(match kind {
                        Operator::Plus => Ir::Plus,
                        Operator::Minus => Ir::Minus,
                        Operator::Div => Ir::Div,
                        Operator::Mult => Ir::Mult,
                        Operator::Mod => Ir::Mod,
                    });
                }
                ExpressionKind::Integer(i) => {
                    ir.push(Ir::PushInt(i));
                }
                ExpressionKind::String(string) => {
                    if level != level {
                        eprintln!("{loc}: ERROR: strings are not allowed in binary expressions");
                        return Err(());
                    }
                    ir.push(Ir::PushString(string));
                }
            },
        }

        Ok(ir)
    }

    fn compile_expression(&mut self, expression: Expression) -> super::Result<Vec<Ir>> {
        self.compile_expression_impl(expression, 0)
    }

    fn compile_statement(&mut self, statement: Statement) -> super::Result<Vec<Ir>> {
        let mut ir = Vec::new();

        match statement {
            Statement { statement, loc } => match statement {
                StatementKind::ProcDecl { name, statements } => {
                    let mut instructions = Vec::new();
                    for statement in statements {
                        for inst in self.compile_statement(statement)? {
                            instructions.push(inst);
                        }
                    }

                    let proc = Proc { instructions };
                    self.scope
                        .last_mut()
                        .unwrap()
                        .procs
                        .insert(name, proc.clone());

                    ir.push(Ir::Proc(proc));
                }
                StatementKind::ProcCall { name, expressions } => {
                    for expression in &expressions {
                        for inst in self.compile_expression(expression.clone())? {
                            ir.push(inst);
                        }
                    }

                    match name.as_str() {
                        "print" => {
                            if expressions.len() != 1 {
                                eprintln!("{loc}: ERROR: incorrect amount of arguments for procedure `{name}`");
                                return Err(());
                            }

                            match ir.last().unwrap().datatype() {
                                Datatype::Integer => ir.push(Ir::PrintInt),
                                Datatype::String => ir.push(Ir::PrintString),
                                Datatype::None => {
                                    let loc = &expressions[0].loc;
                                    eprintln!("{loc}: ERROR: procedure `{name}` expects `Integer` or `String`, but got `None`");
                                    return Err(());
                                }
                            }
                        }
                        _ => {
                            if !self.scope.last().unwrap().procs.contains_key(&name) {
                                eprintln!("{loc}: ERROR: unknown procedure `{name}`");
                                return Err(());
                            }

                            if expressions.len() != 0 {
                                eprintln!("{loc}: ERROR: incorrect amount of arguments for procedure `{name}`");
                                return Err(());
                            }

                            ir.push(Ir::ProcCall(name));
                        }
                    }
                }
            },
        }

        Ok(ir)
    }

    pub fn compile_ast(&mut self, ast: Vec<Statement>) -> super::Result<Vec<Ir>> {
        let mut ir = Vec::new();

        for st in ast {
            for inst in self.compile_statement(st)? {
                ir.push(inst);
            }
        }

        Ok(ir)
    }
}
