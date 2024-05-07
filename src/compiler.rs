use std::collections::HashMap;

use crate::parser::{Expression, ExpressionKind, Operator, Statement, StatementKind};

#[derive(Debug, Clone)]
pub struct Proc {
    pub name: String,
    pub instructions: Vec<Ir>,
}

#[derive(Debug, PartialEq)]
enum Datatype {
    Integer,
    String,
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
    ) -> super::Result<(Vec<Ir>, Datatype)> {
        let mut ir = Vec::new();

        let Expression { expression, loc } = expression;
        let datatype;

        match expression {
            ExpressionKind::Binary { kind, left, right } => {
                let (left_ir, left_datatype) = self.compile_expression_impl(*left, level + 1)?;
                for inst in left_ir {
                    ir.push(inst);
                }

                let (right_ir, right_datatype) = self.compile_expression_impl(*right, level + 1)?;
                for inst in right_ir {
                    ir.push(inst);
                }

                ir.push(match kind {
                    Operator::Plus => Ir::Plus,
                    Operator::Minus => Ir::Minus,
                    Operator::Div => Ir::Div,
                    Operator::Mult => Ir::Mult,
                    Operator::Mod => Ir::Mod,
                });

                datatype =
                    if left_datatype == Datatype::String || right_datatype == Datatype::String {
                        Datatype::String
                    } else {
                        left_datatype
                    };
            }
            ExpressionKind::Integer(i) => {
                ir.push(Ir::PushInt(i));
                datatype = Datatype::Integer;
            }
            ExpressionKind::String(string) => {
                if level != 0 {
                    eprintln!("{loc}: ERROR: strings are not allowed in binary expressions");
                    return Err(());
                }
                ir.push(Ir::PushString(string));
                datatype = Datatype::String;
            }
        }

        Ok((ir, datatype))
    }

    fn compile_expression(&mut self, expression: Expression) -> super::Result<(Vec<Ir>, Datatype)> {
        self.compile_expression_impl(expression, 0)
    }

    fn compile_statement(&mut self, statement: Statement) -> super::Result<Vec<Ir>> {
        let mut ir = Vec::new();

        let Statement { statement, loc } = statement;

        match statement {
            StatementKind::ProcDecl { name, statements } => {
                let mut instructions = Vec::new();
                for statement in statements {
                    for inst in self.compile_statement(statement)? {
                        instructions.push(inst);
                    }
                }

                let proc = Proc {
                    name: name.clone(),
                    instructions,
                };
                self.scope
                    .last_mut()
                    .unwrap()
                    .procs
                    .insert(name, proc.clone());

                ir.push(Ir::Proc(proc));
            }
            StatementKind::ProcCall { name, expressions } => {
                let mut datatypes = Vec::new();

                for expression in &expressions {
                    let (expr_ir, datatype) = self.compile_expression(expression.clone())?;
                    datatypes.push(datatype);
                    for inst in expr_ir {
                        ir.push(inst);
                    }
                }

                match name.as_str() {
                    "print" => {
                        if expressions.len() != 1 {
                            eprintln!("{loc}: ERROR: incorrect amount of arguments for procedure `{name}`");
                            return Err(());
                        }

                        match &datatypes[0] {
                            Datatype::Integer => ir.push(Ir::PrintInt),
                            Datatype::String => ir.push(Ir::PrintString),
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
            StatementKind::Let { name: _name, expression } => {
                println!("{expression:?}");
                todo!();
            }
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
