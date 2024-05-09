use std::collections::HashMap;

use crate::lexer::Location;
use crate::parser::{Expression, ExpressionKind, Operator, Statement, StatementKind};

#[derive(Debug, Clone)]
pub struct Proc {
    pub name: String,
    pub instructions: Vec<Ir>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub datatype: Datatype,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Datatype {
    Integer,
    String,
}

impl Datatype {
    fn from_string(string: String) -> Option<Self> {
        match string.as_str() {
            "Integer" => Some(Self::Integer),
            "String" => Some(Self::String),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Ir {
    Proc(Proc),
    PushInt(i64),
    PushString(String),

    GlobalVar {
        name: String,
        datatype: Datatype,
        initial_value: Value,
    },
    LocalVar(String, Datatype),
    GetVar(String),
    SetVar(String),

    ProcCall(String),

    PrintInt,
    PrintString,

    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

#[derive(Default, Debug)]
struct Scope {
    procs: HashMap<String, Proc>,
    variables: HashMap<String, Variable>,
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

    fn find_variable(&self, loc: Location, name: String) -> super::Result<Variable> {
        for scope in self.scope.iter().rev() {
            if let Some(variable) = scope.variables.get(&name) {
                return Ok(variable.clone());
            }
        }
        eprintln!("{loc}: ERROR: unknown variable `{name}`");
        Err(())
    }

    fn find_procedure(&self, loc: Location, name: String) -> super::Result<Proc> {
        for scope in self.scope.iter().rev() {
            if let Some(variable) = scope.procs.get(&name) {
                return Ok(variable.clone());
            }
        }
        eprintln!("{loc}: ERROR: unknown procedure `{name}`");
        Err(())
    }

    fn name_redefinition(&self, loc: Location, name: String) -> super::Result<()> {
        let builtin_procs = &["print"];

        for scope in self.scope.iter().rev() {
            if scope.procs.contains_key(&name)
                || scope.variables.contains_key(&name)
                || builtin_procs.contains(&name.as_str())
            {
                eprintln!("{loc}: ERROR: redefinition of name `{name}`");
                return Err(());
            }
        }
        Ok(())
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
            ExpressionKind::Statement(statement) => {
                for inst in self.compile_statement(statement.clone())? {
                    ir.push(inst);
                }
                match statement.statement {
                    StatementKind::GetVar { name } => {
                        let var = self.find_variable(loc, name)?;
                        datatype = var.datatype;
                    }
                    _ => {
                        eprintln!("{loc}: ERROR: unexpected statement in expression");
                        return Err(());
                    }
                }
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
                self.scope.push(Scope::default());

                let mut instructions = Vec::new();
                for statement in statements {
                    for inst in self.compile_statement(statement)? {
                        instructions.push(inst);
                    }
                }

                self.scope.pop().unwrap();

                self.name_redefinition(loc, name.clone())?;

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
                        self.find_procedure(loc.clone(), name.clone())?;

                        if expressions.len() != 0 {
                            let loc = &expressions.last().unwrap().loc;
                            eprintln!("{loc}: ERROR: incorrect amount of arguments for procedure `{name}`");
                            return Err(());
                        }

                        ir.push(Ir::ProcCall(name));
                    }
                }
            }
            StatementKind::Let {
                name,
                expression,
                datatype,
            } => {
                let datatype = if let Some(datatype) = Datatype::from_string(datatype.clone()) {
                    datatype
                } else {
                    eprintln!("{loc}: ERROR: unknown type `{datatype}`");
                    return Err(());
                };

                let (expr_ir, expr_datatype) = self.compile_expression(*expression.clone())?;

                if self.scope.len() == 1 {
                    // Global variable
                    let Expression {
                        expression: expr,
                        loc: expr_loc,
                    } = *expression;

                    let value;

                    match expr {
                        ExpressionKind::Integer(i) => value = Value::Integer(i),
                        ExpressionKind::String(s) => value = Value::String(s),
                        _ => {
                            eprintln!(
                                "{expr_loc}: ERROR: expression cannot be executed at compile time"
                            );
                            return Err(());
                        }
                    }

                    ir.push(Ir::GlobalVar {
                        name: name.clone(),
                        datatype: datatype.clone(),
                        initial_value: value,
                    });
                } else {
                    // Local variable
                    ir.push(Ir::LocalVar(name.clone(), datatype.clone()));

                    for inst in expr_ir {
                        ir.push(inst);
                    }

                    ir.push(Ir::SetVar(name.clone()));
                }

                if expr_datatype != datatype {
                    eprintln!("{loc}: ERROR: mismatched types: expression has type `{expr_datatype:?}` and variable has type `{datatype:?}`");
                    return Err(());
                }

                self.name_redefinition(loc, name.clone())?;

                let var = Variable {
                    name: name.clone(),
                    datatype,
                };
                self.scope.last_mut().unwrap().variables.insert(name, var);
            }
            StatementKind::GetVar { name } => {
                self.find_variable(loc, name.clone())?;
                ir.push(Ir::GetVar(name));
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
