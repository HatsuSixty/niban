use crate::{
    compiler::Datatype,
    lexer::{Keyword, Lexer, Location, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum Operator {
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
}

impl Operator {
    fn from_token(token: Token) -> Option<Operator> {
        match token.token {
            TokenKind::Plus => Some(Self::Plus),
            TokenKind::Minus => Some(Self::Minus),
            TokenKind::Mult => Some(Self::Mult),
            TokenKind::Div => Some(Self::Div),
            TokenKind::Mod => Some(Self::Mod),
            TokenKind::Lt => Some(Self::Lt),
            TokenKind::Gt => Some(Self::Gt),
            TokenKind::GreaterEqual => Some(Self::Ge),
            TokenKind::LessEqual => Some(Self::Le),
            TokenKind::DoubleEqual => Some(Self::Eq),
            TokenKind::NotEqual => Some(Self::Nq),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Dereference,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Binary {
        kind: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Unary {
        kind: UnaryOperator,
        right: Box<Expression>,
    },
    String(String),
    Integer(i64),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression: ExpressionKind,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    ProcDecl {
        name: String,
        statements: Vec<Statement>,
        export: bool,
    },
    ProcCall {
        name: String,
        expressions: Vec<Expression>,
    },
    Let {
        name: String,
        datatype: Datatype,
        expression: Box<Expression>,
    },
    GetVar {
        name: String,
    },
    SetVar {
        name: String,
        expression: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then: Vec<Statement>,
        elsee: Option<Vec<Statement>>,
    },
    While {
        condition: Box<Expression>,
        body: Vec<Statement>,
    },
    AddrOf {
        name: String,
    },
    WriteIntoAddr {
        address: Box<Expression>,
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub loc: Location,
    pub statement: StatementKind,
}

fn expect_token(inn: &str, lexer: &mut Lexer, token: TokenKind) -> super::Result<Token> {
    let t = match lexer.peek()? {
        Some(x) => x,
        None => {
            eprintln!(
                "{loc}: ERROR: expected token `{token:?}` {inn} but got nothing",
                loc = lexer.get_loc()
            );
            return Err(());
        }
    };

    if !t.token.eq(&token) {
        eprintln!(
            "{loc}: ERROR: expected token `{token:?}` {inn} but got `{got:?}`",
            loc = t.loc,
            got = t.token
        );
        return Err(());
    }

    lexer.next()?;
    Ok(t)
}

fn parse_block(lexer: &mut Lexer) -> super::Result<Vec<Statement>> {
    let open_brace = expect_token("at the beginning of a block", lexer, TokenKind::OpenBrace)?;
    let mut statements = Vec::new();

    loop {
        let token = if let Some(x) = lexer.peek()? {
            x
        } else {
            eprintln!(
                "{}: ERROR: reached end of file while parsing block",
                open_brace.loc
            );
            return Err(());
        };

        if token.token.eq(&TokenKind::CloseBrace) {
            break;
        }

        let statement = if let Some(s) = parse_statement(lexer)? {
            s
        } else {
            eprintln!(
                "{}: ERROR: reached end of file while parsing statement",
                token.loc
            );
            return Err(());
        };
        expect_token("at the end of a statement", lexer, TokenKind::Semicolon)?;

        statements.push(statement);
    }
    lexer.next()?;

    Ok(statements)
}

fn parse_proc(loc: Location, lexer: &mut Lexer, export: bool) -> super::Result<Statement> {
    let name = expect_token("in procedure definition", lexer, TokenKind::Word("".into()))?;

    let _open_paren = expect_token("in procedure definition", lexer, TokenKind::OpenParen)?;
    expect_token("in procedure definition", lexer, TokenKind::CloseParen)?;

    let block = parse_block(lexer)?;

    Ok(Statement {
        statement: StatementKind::ProcDecl {
            name: match name.token {
                TokenKind::Word(n) => n.into(),
                _ => unreachable!(),
            },
            statements: block,
            export,
        },
        loc,
    })
}

fn parse_procparams(lexer: &mut Lexer) -> super::Result<Vec<Expression>> {
    let open_paren = expect_token("in procedure parameters", lexer, TokenKind::OpenParen)?;

    let mut expressions = Vec::new();

    let mut loc;
    loop {
        let token = if let Some(tok) = lexer.peek()? {
            tok
        } else {
            eprintln!(
                "{}: ERROR: reached end of file while parsing procedure parameters",
                open_paren.loc
            );
            return Err(());
        };
        loc = token.loc;

        if token.token.eq(&TokenKind::CloseParen) {
            break;
        }

        let expr = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest())?;

        if let Some(tok) = lexer.peek()? {
            match tok.token {
                TokenKind::Comma => {
                    lexer.next()?;
                }
                TokenKind::CloseParen => {}
                _ => {
                    eprintln!(
                        "{loc}: ERROR: expected token `Comma` in procedure parameters but got `{tok:?}`",
                        loc = tok.loc, tok = tok.token
                    );
                    return Err(());
                }
            }
        }

        expressions.push(expr);
    }
    lexer.next()?;

    Ok(expressions)
}

fn parse_proccall(name: Token, lexer: &mut Lexer) -> super::Result<Statement> {
    let parameters = parse_procparams(lexer)?;

    Ok(Statement {
        statement: StatementKind::ProcCall {
            name: match name.token {
                TokenKind::Word(s) => s,
                _ => unreachable!(),
            },
            expressions: parameters,
        },
        loc: name.loc,
    })
}

fn parse_datatype(lexer: &mut Lexer) -> super::Result<Datatype> {
    let token = expect_token("in datatype", lexer, TokenKind::Word("".into()))?;
    let name = if let TokenKind::Word(text) = token.token {
        text
    } else {
        unreachable!();
    };

    Ok(match name.as_str() {
        "I8" => Datatype::I8,
        "I16" => Datatype::I16,
        "I32" => Datatype::I32,
        "I64" => Datatype::I64,
        "String" => Datatype::String,
        "Pointer" => {
            expect_token("in datatype", lexer, TokenKind::OpenParen)?;
            let inner_type = parse_datatype(lexer)?;
            expect_token("in datatype", lexer, TokenKind::CloseParen)?;

            Datatype::Pointer(Box::new(inner_type))
        }
        _ => {
            eprintln!("{loc}: ERROR: unknown datatype `{name}`", loc = token.loc);
            return Err(());
        }
    })
}

fn parse_let(loc: Location, lexer: &mut Lexer) -> super::Result<Statement> {
    let name = expect_token("in variable definition", lexer, TokenKind::Word("".into()))?;

    expect_token("in variable definition", lexer, TokenKind::Colon)?;

    let datatype = parse_datatype(lexer)?;

    expect_token("in variable definition", lexer, TokenKind::Equal)?;

    let expr = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest())?;

    Ok(Statement {
        loc,
        statement: StatementKind::Let {
            name: match name.token {
                TokenKind::Word(name) => name,
                _ => unreachable!(),
            },
            datatype,
            expression: Box::new(expr),
        },
    })
}

fn parse_if(loc: Location, lexer: &mut Lexer) -> super::Result<Statement> {
    let condition = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest())?;

    let then = parse_block(lexer)?;
    let mut elsee = None;

    if let Some(token) = lexer.peek()? {
        if token.token.eq(&TokenKind::Keyword(Keyword::Else)) {
            lexer.next()?;

            match lexer.peek()? {
                Some(Token {
                    token: TokenKind::Keyword(Keyword::If),
                    ..
                }) => {
                    let iff = lexer.next()?.unwrap();
                    elsee = Some(vec![parse_if(iff.loc, lexer)?]);
                }
                _ => elsee = Some(parse_block(lexer)?),
            }
        }
    }

    Ok(Statement {
        loc,
        statement: StatementKind::If {
            condition: Box::new(condition),
            then,
            elsee,
        },
    })
}

fn parse_while(loc: Location, lexer: &mut Lexer) -> super::Result<Statement> {
    let condition = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest())?;
    let body = parse_block(lexer)?;

    Ok(Statement {
        loc,
        statement: StatementKind::While {
            condition: Box::new(condition),
            body,
        },
    })
}

fn parse_addrof(loc: Location, lexer: &mut Lexer) -> super::Result<Statement> {
    let name = expect_token("in addrof statement", lexer, TokenKind::Word("".into()))?;
    Ok(Statement {
        statement: StatementKind::AddrOf {
            name: if let TokenKind::Word(name) = name.token {
                name
            } else {
                unreachable!();
            },
        },
        loc,
    })
}

fn parse_statement(lexer: &mut Lexer) -> super::Result<Option<Statement>> {
    let token = if let Some(tok) = lexer.next()? {
        tok
    } else {
        return Ok(None);
    };

    let statement;

    match token.token {
        TokenKind::Keyword(ref keyword) => match keyword {
            Keyword::Proc | Keyword::Export => {
                eprintln!(
                    "{}: ERROR: procedure definitions are only allowed as toplevel statements",
                    token.loc
                );
                return Err(());
                // statement = parse_proc(token.loc, lexer)?;
            }
            Keyword::Let => statement = parse_let(token.loc, lexer)?,
            Keyword::If => statement = parse_if(token.loc, lexer)?,
            Keyword::While => statement = parse_while(token.loc, lexer)?,
            Keyword::Else => {
                eprintln!("{}: ERROR: unexpected token `{:?}`", token.loc, token.token);
                return Err(());
            }
        },
        TokenKind::Word(_) => {
            if let Some(ptk) = lexer.peek()? {
                match ptk.token {
                    TokenKind::OpenParen => statement = parse_proccall(token, lexer)?,
                    TokenKind::Equal => {
                        let name = if let TokenKind::Word(name) = token.token {
                            name
                        } else {
                            unreachable!();
                        };

                        lexer.next()?;

                        let expr =
                            parse_expression(lexer.get_loc(), lexer, OperatorPrecedence::lowest())?;

                        statement = Statement {
                            statement: StatementKind::SetVar {
                                name,
                                expression: Box::new(expr),
                            },
                            loc: token.loc,
                        };
                    }
                    _ => {
                        let name = if let TokenKind::Word(name) = token.token {
                            name
                        } else {
                            unreachable!();
                        };

                        statement = Statement {
                            statement: StatementKind::GetVar { name },
                            loc: token.loc,
                        }
                    }
                }
            } else {
                eprintln!(
                    "{loc}: ERROR: reached end of file while handling `{word:?}`",
                    loc = token.loc,
                    word = token.token,
                );
                return Err(());
            }
        }
        TokenKind::Ampersand => statement = parse_addrof(token.loc, lexer)?,
        TokenKind::Gt => {
            let addr = parse_expression(token.loc.clone(), lexer, OperatorPrecedence::lowest())?;
            expect_token("in memory write expression", lexer, TokenKind::LeftArrow)?;
            let value = parse_expression(token.loc.clone(), lexer, OperatorPrecedence::lowest())?;

            statement = Statement {
                statement: StatementKind::WriteIntoAddr {
                    address: Box::new(addr),
                    value: Box::new(value),
                },
                loc: token.loc,
            }
        }
        _ => {
            eprintln!("{}: ERROR: unexpected token `{:?}`", token.loc, token.token);
            return Err(());
        }
    }

    Ok(Some(statement))
}

pub fn parse_statement_toplevel(lexer: &mut Lexer) -> super::Result<Option<Statement>> {
    let token = if let Some(tok) = lexer.next()? {
        tok
    } else {
        return Ok(None);
    };

    match token.token {
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::Proc => Ok(Some(parse_proc(token.loc, lexer, false)?)),
            Keyword::Export => {
                expect_token(
                    "in procedure definition",
                    lexer,
                    TokenKind::Keyword(Keyword::Proc),
                )?;

                Ok(Some(parse_proc(token.loc, lexer, true)?))
            }
            Keyword::Let => Ok(Some(parse_let(token.loc, lexer)?)),
            Keyword::If | Keyword::Else => {
                eprintln!(
                    "{}: ERROR: if/else statements are not allowed as toplevel statements",
                    token.loc,
                );
                Err(())
            }
            Keyword::While => {
                eprintln!(
                    "{}: ERROR: while statements are not allowed as toplevel statements",
                    token.loc,
                );
                Err(())
            }
        },
        _ => {
            eprintln!(
                "{}: ERROR: no toplevel statement starts with token `{:?}`",
                token.loc, token.token,
            );
            Err(())
        }
    }
}

#[derive(PartialEq)]
enum OperatorPrecedence {
    Primary,
    Multiplicative,
    Additive,
    ComparisonLtGt,
    ComparisonEqNq,
}

impl OperatorPrecedence {
    fn lowest() -> Self {
        Self::ComparisonEqNq
    }

    fn higher(&self) -> Self {
        match self {
            Self::Primary => Self::Primary,
            Self::Multiplicative => Self::Primary,
            Self::Additive => Self::Multiplicative,
            Self::ComparisonLtGt => Self::Additive,
            Self::ComparisonEqNq => Self::ComparisonLtGt,
        }
    }

    fn matches_precedence(&self, operator: Operator) -> bool {
        match (self, operator) {
            (Self::Primary, _) => false,
            (Self::Multiplicative, Operator::Div | Operator::Mult | Operator::Mod) => true,
            (Self::Additive, Operator::Plus | Operator::Minus) => true,
            (Self::ComparisonLtGt, Operator::Lt | Operator::Gt | Operator::Le | Operator::Ge) => {
                true
            }
            (Self::ComparisonEqNq, Operator::Eq | Operator::Nq) => true,
            _ => false,
        }
    }
}

fn parse_expression(
    loc: Location,
    lexer: &mut Lexer,
    precedence: OperatorPrecedence,
) -> super::Result<Expression> {
    if precedence == OperatorPrecedence::Primary {
        return parse_primary_expression(loc, lexer);
    }

    let mut left = parse_expression(loc.clone(), lexer, precedence.higher())?;

    loop {
        let token = if let Some(tok) = lexer.peek()? {
            tok
        } else {
            break;
        };

        let operator = match Operator::from_token(token.clone()) {
            Some(op) => {
                if !precedence.matches_precedence(op.clone()) {
                    break;
                }
                op
            }
            None => break,
        };
        lexer.next()?;

        let right = parse_expression(loc.clone(), lexer, precedence.higher())?;

        left = Expression {
            expression: ExpressionKind::Binary {
                kind: operator,
                left: Box::new(left.clone()),
                right: Box::new(right),
            },
            loc: loc.clone(),
        };
    }

    Ok(left)
}

fn parse_primary_expression(loc: Location, lexer: &mut Lexer) -> super::Result<Expression> {
    let token = if let Some(tok) = lexer.peek()? {
        tok
    } else {
        eprintln!("{loc}: ERROR: reached end of file while parsing expression");
        return Err(());
    };

    match token.token {
        TokenKind::String(string) => {
            lexer.next()?;

            Ok(Expression {
                expression: ExpressionKind::String(string),
                loc: token.loc,
            })
        }
        TokenKind::Integer(integer) => {
            lexer.next()?;

            Ok(Expression {
                expression: ExpressionKind::Integer(integer),
                loc: token.loc,
            })
        }
        TokenKind::Exclamation => {
            lexer.next()?;

            Ok(Expression {
                expression: ExpressionKind::Unary {
                    kind: UnaryOperator::Not,
                    right: Box::new(parse_expression(
                        token.loc.clone(),
                        lexer,
                        (OperatorPrecedence::Primary).higher(),
                    )?),
                },
                loc: token.loc,
            })
        }
        TokenKind::Mult => {
            lexer.next()?;

            Ok(Expression {
                expression: ExpressionKind::Unary {
                    kind: UnaryOperator::Dereference,
                    right: Box::new(parse_expression(
                        token.loc.clone(),
                        lexer,
                        (OperatorPrecedence::Primary).higher(),
                    )?),
                },
                loc: token.loc,
            })
        }
        TokenKind::OpenParen => {
            lexer.next()?;

            let value = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest());
            expect_token("in expression", lexer, TokenKind::CloseParen)?;
            value
        }
        _ => {
            let statement = if let Some(stmt) = parse_statement(lexer)? {
                stmt
            } else {
                eprintln!(
                    "{loc}: ERROR: reached end of file while handling `{word:?}`",
                    loc = token.loc.clone(),
                    word = token.token
                );
                return Err(());
            };
            Ok(Expression {
                expression: ExpressionKind::Statement(statement),
                loc: token.loc,
            })
        }
    }
}
