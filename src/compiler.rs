use std::collections::HashMap;

use crate::lexer::Location;
use crate::parser::{Expression, ExpressionKind, Operator, Statement, StatementKind, UnaryOperator};

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
    I8,
    I16,
    I32,
    I64,
    String,
    Pointer(Box<Datatype>),
    None,
}

impl Datatype {
    pub fn get_size(&self) -> usize {
        match self {
            Datatype::I8 => 1,
            Datatype::I16 => 2,
            Datatype::I32 => 4,
            Datatype::I64 => 8,
            Datatype::String => 8, // 64 bit pointer
            Datatype::Pointer(_) => 8,
            Datatype::None => 0,
        }
    }

    fn mismatches(&self, other: &Datatype) -> bool {
        match (self, other) {
            (
                Datatype::I8 | Datatype::I16 | Datatype::I32 | Datatype::I64,
                Datatype::I8 | Datatype::I16 | Datatype::I32 | Datatype::I64,
            ) => false,
            (a, b) => a != b,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    String(String),
}

impl Value {
    fn as_int(&self, loc: Location) -> super::Result<i64> {
        match self {
            Self::Integer(i) => Ok(*i),
            e => {
                eprintln!("{loc}: ERROR: expected integer but got {e:?}");
                return Err(());
            }
        }
    }

    fn datatype(&self) -> Datatype {
        match self {
            Self::Integer(_) => Datatype::I32,
            Self::String(_) => Datatype::String,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ir {
    Proc {
        proc: Proc,
        export: bool,
    },
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

    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Nq,
    Not,

    JumpIfNot(usize),
    Jump(usize),
    Label(usize),

    AddrOf(String),
    Read(Datatype),
    Write(Datatype),
}

#[derive(Default, Debug)]
struct Scope {
    procs: HashMap<String, Proc>,
    variables: HashMap<String, Variable>,
}

pub struct Compiler {
    scope: Vec<Scope>,
    label_count: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut scope = Vec::new();
        scope.push(Scope::default());

        Self {
            scope,
            label_count: 0,
        }
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

    fn compile_expression(&mut self, expression: Expression) -> super::Result<(Vec<Ir>, Datatype)> {
        let mut ir = Vec::new();

        let Expression { expression, loc: _ } = expression;
        let datatype;

        match expression {
            ExpressionKind::Binary { kind, left, right } => {
                let (left_ir, left_datatype) = self.compile_expression(*left.clone())?;
                for inst in left_ir {
                    ir.push(inst);
                }

                let (right_ir, right_datatype) = self.compile_expression(*right.clone())?;
                for inst in right_ir {
                    ir.push(inst);
                }

                if left_datatype == Datatype::String {
                    eprintln!(
                        "{}: ERROR: strings are not allowed in binary expressions",
                        left.loc
                    );
                    return Err(());
                }

                if right_datatype == Datatype::String {
                    eprintln!(
                        "{}: ERROR: strings are not allowed in binary expressions",
                        right.loc
                    );
                    return Err(());
                }

                ir.push(match kind {
                    Operator::Plus => Ir::Plus,
                    Operator::Minus => Ir::Minus,
                    Operator::Div => Ir::Div,
                    Operator::Mult => Ir::Mult,
                    Operator::Mod => Ir::Mod,
                    Operator::Lt => Ir::Lt,
                    Operator::Gt => Ir::Gt,
                    Operator::Le => Ir::Le,
                    Operator::Ge => Ir::Ge,
                    Operator::Eq => Ir::Eq,
                    Operator::Nq => Ir::Nq,
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
                datatype = Datatype::I32;
            }
            ExpressionKind::String(string) => {
                ir.push(Ir::PushString(string));
                datatype = Datatype::String;
            }
            ExpressionKind::Statement(statement) => {
                let (stmt_ir, stmt_datatype) = self.compile_statement(statement.clone())?;
                for inst in stmt_ir {
                    ir.push(inst);
                }
                datatype = stmt_datatype;
            }
            ExpressionKind::Unary { kind, right } => {
                let (expr_ir, expr_datatype) = self.compile_expression(*right)?;
                for inst in expr_ir {
                    ir.push(inst);
                }

                match kind {
                    UnaryOperator::Not => ir.push(Ir::Not),
                }

                datatype = expr_datatype;
            }
        }

        Ok((ir, datatype))
    }

    fn compile_block(&mut self, block: Vec<Statement>) -> super::Result<Vec<Ir>> {
        self.scope.push(Scope::default());

        let mut instructions = Vec::new();
        for statement in block {
            let (stmt_ir, _) = self.compile_statement(statement)?;
            for inst in stmt_ir {
                instructions.push(inst);
            }
        }

        self.scope.pop().unwrap();

        Ok(instructions)
    }

    fn compile_statement(&mut self, statement: Statement) -> super::Result<(Vec<Ir>, Datatype)> {
        let mut ir = Vec::new();

        let mut statement_datatype = Datatype::None;
        let Statement { statement, loc } = statement;

        match statement {
            StatementKind::ProcDecl {
                name,
                statements,
                export,
            } => {
                let instructions = self.compile_block(statements)?;

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

                ir.push(Ir::Proc { proc, export });
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
                            Datatype::I8
                            | Datatype::I16
                            | Datatype::I32
                            | Datatype::I64
                            | Datatype::Pointer(_) => ir.push(Ir::PrintInt),
                            Datatype::String => ir.push(Ir::PrintString),
                            Datatype::None => {
                                eprintln!("{loc}: ERROR: mismatched types: expression has type `None` and procedure `{name}` expects `I8`, `I16`, `I32`, `I64` or `Pointer`");
                                return Err(());
                            }
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
                let expr_loc = expression.loc.clone();

                if self.scope.len() == 1 {
                    // Global variable
                    let value = compile_time_evaluate(*expression)?;
                    if value.datatype().mismatches(&datatype) {
                        eprintln!("{expr_loc}: ERROR: mismatched types: expression has type `{expr_datatype:?}` and variable has type `{datatype:?}`", expr_datatype = value.datatype());
                        return Err(());
                    }

                    ir.push(Ir::GlobalVar {
                        name: name.clone(),
                        datatype: datatype.clone(),
                        initial_value: value,
                    });
                } else {
                    // Local variable
                    let (expr_ir, expr_datatype) = self.compile_expression(*expression)?;
                    if expr_datatype.mismatches(&datatype) {
                        eprintln!("{expr_loc}: ERROR: mismatched types: expression has type `{expr_datatype:?}` and variable has type `{datatype:?}`");
                        return Err(());
                    }

                    ir.push(Ir::LocalVar(name.clone(), datatype.clone()));

                    for inst in expr_ir {
                        ir.push(inst);
                    }
                    ir.push(Ir::SetVar(name.clone()));
                }

                self.name_redefinition(loc, name.clone())?;

                let var = Variable {
                    name: name.clone(),
                    datatype,
                };
                self.scope.last_mut().unwrap().variables.insert(name, var);
            }
            StatementKind::GetVar { name } => {
                let variable = self.find_variable(loc, name.clone())?;
                ir.push(Ir::GetVar(name));

                statement_datatype = variable.datatype;
            }
            StatementKind::SetVar { name, expression } => {
                let (expr_ir, expr_datatype) = self.compile_expression(*expression.clone())?;

                let var = self.find_variable(loc.clone(), name.clone())?;
                if var.datatype.mismatches(&expr_datatype) {
                    eprintln!("{loc}: ERROR: mismatched types: expression has type `{expr_datatype:?}` and variable has type `{datatype:?}`", loc = expression.loc, datatype = var.datatype);
                    return Err(());
                }

                for inst in expr_ir {
                    ir.push(inst);
                }

                ir.push(Ir::SetVar(name.clone()));
            }
            StatementKind::If {
                condition,
                then,
                elsee,
            } => {
                let (expr_ir, _) = self.compile_expression(*condition)?;
                for inst in expr_ir {
                    ir.push(inst);
                }

                let jumpifnot_index = ir.len();
                ir.push(Ir::JumpIfNot(0));

                for inst in self.compile_block(then)? {
                    ir.push(inst);
                }

                let mut skip_else_jmp = None;
                if elsee.is_some() {
                    skip_else_jmp = Some(ir.len());
                    ir.push(Ir::Jump(0));
                }

                ir[jumpifnot_index] = Ir::JumpIfNot(self.label_count);
                ir.push(Ir::Label(self.label_count));
                self.label_count += 1;

                if let Some(skip_else_jmp) = skip_else_jmp {
                    for inst in self.compile_block(elsee.unwrap())? {
                        ir.push(inst);
                    }

                    ir[skip_else_jmp] = Ir::Jump(self.label_count);
                    ir.push(Ir::Label(self.label_count));
                    self.label_count += 1;
                }
            }
            StatementKind::While { condition, body } => {
                let while_label_id = self.label_count;
                self.label_count += 1;
                ir.push(Ir::Label(while_label_id));

                let (expr_ir, _) = self.compile_expression(*condition)?;
                for inst in expr_ir {
                    ir.push(inst);
                }

                let jumpifnot_index = ir.len();
                ir.push(Ir::JumpIfNot(0));

                for inst in self.compile_block(body)? {
                    ir.push(inst);
                }

                ir.push(Ir::Jump(while_label_id));

                ir[jumpifnot_index] = Ir::JumpIfNot(self.label_count);
                ir.push(Ir::Label(self.label_count));
                self.label_count += 1;
            }
            StatementKind::AddrOf { name } => {
                let variable = self.find_variable(loc, name.clone())?;
                ir.push(Ir::AddrOf(name));

                statement_datatype = Datatype::Pointer(Box::new(variable.datatype));
            }
            StatementKind::Dereference { name } => {
                let variable = self.find_variable(loc.clone(), name.clone())?;
                let ptr_to_datatype = if let Datatype::Pointer(datatype) = variable.datatype {
                    *datatype
                } else {
                    eprintln!("{loc}: ERROR: trying to dereference non-pointer variable");
                    return Err(());
                };

                ir.push(Ir::GetVar(name.clone()));
                ir.push(Ir::Read(ptr_to_datatype.clone()));

                statement_datatype = ptr_to_datatype;
            }
            StatementKind::WriteIntoAddr { name, expression } => {
                let addr_datatype = if let Datatype::Pointer(datatype) =
                    self.find_variable(loc.clone(), name.clone())?.datatype
                {
                    *datatype
                } else {
                    eprintln!("{loc}: ERROR: write address must be a pointer");
                    return Err(());
                };

                let (expr_ir, expr_datatype) = self.compile_expression(*expression)?;
                if expr_datatype.mismatches(&addr_datatype) {
                    eprintln!("{loc}: ERROR: mismatched types: expression has type `{expr_datatype:?}` and memory location has type `{addr_datatype:?}`");
                    return Err(());
                }
                for inst in expr_ir {
                    ir.push(inst);
                }

                ir.push(Ir::GetVar(name.clone()));
                ir.push(Ir::Write(addr_datatype));
            }
        }

        Ok((ir, statement_datatype))
    }

    pub fn compile_ast(&mut self, ast: Vec<Statement>) -> super::Result<Vec<Ir>> {
        let mut ir = Vec::new();

        for st in ast {
            let (stmt_ir, _) = self.compile_statement(st)?;
            for inst in stmt_ir {
                ir.push(inst);
            }
        }

        Ok(ir)
    }
}

fn compile_time_evaluate(expression: Expression) -> super::Result<Value> {
    let Expression { expression, loc } = expression;

    match expression {
        ExpressionKind::Binary { kind, left, right } => {
            let left = compile_time_evaluate(*left)?;
            let right = compile_time_evaluate(*right)?;

            match kind {
                Operator::Div => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer(left / right))
                }
                Operator::Minus => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer(left - right))
                }
                Operator::Mod => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer(left % right))
                }
                Operator::Mult => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer(left * right))
                }
                Operator::Plus => match (left, right) {
                    (Value::String(s), Value::Integer(i)) => Ok(Value::String(format!("{s}{i}"))),
                    (Value::Integer(i), Value::String(s)) => Ok(Value::String(format!("{i}{s}"))),
                    (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{a}{b}"))),
                    (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
                },
                Operator::Lt => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left < right) as i64))
                }
                Operator::Gt => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left > right) as i64))
                }
                Operator::Le => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left <= right) as i64))
                }
                Operator::Ge => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left >= right) as i64))
                }
                Operator::Eq => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left == right) as i64))
                }
                Operator::Nq => {
                    let left = left.as_int(loc.clone())?;
                    let right = right.as_int(loc)?;
                    Ok(Value::Integer((left != right) as i64))
                }
            }
        }
        ExpressionKind::Integer(i) => Ok(Value::Integer(i)),
        ExpressionKind::Statement(_) => {
            eprintln!("{loc}: ERROR: statements are not allowed in compile time expressions");
            Err(())
        }
        ExpressionKind::String(s) => Ok(Value::String(s)),
        ExpressionKind::Unary { kind, right } => {
            let value = compile_time_evaluate(*right)?.as_int(loc.clone())?;
            match kind {
                UnaryOperator::Not => Ok(Value::Integer(!value)),
            }
        }
    }
}
