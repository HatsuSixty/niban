use crate::lexer::{Keyword, Lexer, /*LexerError, */Location, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

impl Operator {
    fn from_token(token: Token) -> Option<Operator> {
        match token.token {
            TokenKind::Plus => Some(Self::Plus),
            TokenKind::Minus => Some(Self::Minus),
            TokenKind::Mult => Some(Self::Mult),
            TokenKind::Div => Some(Self::Div),
            TokenKind::Mod => Some(Self::Mod),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Binary {
        kind: Operator,
        left: Box<Expression>,
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
        datatype: String,
        expression: Box<Expression>,
    },
    GetVar {
        name: String,
    },
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub loc: Location,
    pub statement: StatementKind,
}

fn peek_token<'a>(lexer: &mut Lexer<'a>) -> super::Result<Option<Token>> {
    lexer.peek().clone().map_err(|e| eprintln!("{e}"))
}

fn next_token(lexer: &mut Lexer) -> super::Result<Option<Token>> {
    lexer.next().clone().map_err(|e| eprintln!("{e}"))
}

fn expect_token(
    inn: &str,
    loc: Location,
    lexer: &mut Lexer,
    token: TokenKind,
) -> super::Result<Token> {
    let t = match peek_token(lexer)? {
        Some(x) => x,
        None => {
            eprintln!("{loc}: ERROR: expected token `{token:?}` {inn} but got nothing");
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

    lexer.next();
    Ok(t)
}

pub fn parse_block(loc: Location, lexer: &mut Lexer) -> super::Result<Vec<Statement>> {
    let open_brace = expect_token(
        "at the beginning of a block",
        loc,
        lexer,
        TokenKind::OpenBrace,
    )?;
    let mut statements = Vec::new();

    loop {
        let token = if let Some(x) = peek_token(lexer)? {
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
        expect_token(
            "at the end of a statement",
            token.loc,
            lexer,
            TokenKind::Semicolon,
        )?;

        statements.push(statement);
    }
    lexer.next();

    Ok(statements)
}

pub fn parse_proc(loc: Location, lexer: &mut Lexer, export: bool) -> super::Result<Statement> {
    let name = expect_token(
        "in procedure definition",
        loc.clone(),
        lexer,
        TokenKind::Word("".into()),
    )?;

    let _open_paren = expect_token(
        "in procedure definition",
        loc.clone(),
        lexer,
        TokenKind::OpenParen,
    )?;
    let close_paren = expect_token(
        "in procedure definition",
        loc.clone(),
        lexer,
        TokenKind::CloseParen,
    )?;

    let block = parse_block(close_paren.loc, lexer)?;

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

pub fn parse_procparams(
    loc: Location,
    lexer: &mut Lexer,
) -> super::Result<Vec<Expression>> {
    let open_paren = expect_token(
        "in procedure parameters",
        loc.clone(),
        lexer,
        TokenKind::OpenParen,
    )?;

    let mut expressions = Vec::new();

    let mut loc;
    loop {
        let token = if let Some(tok) = peek_token(lexer)? {
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

        if let Some(tok) = peek_token(lexer)? {
            match tok.token {
                TokenKind::Comma => {
                    lexer.next();
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
    lexer.next();

    Ok(expressions)
}

pub fn parse_proccall(name: Token, lexer: &mut Lexer) -> super::Result<Statement> {
    let parameters = parse_procparams(name.loc.clone(), lexer)?;

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

pub fn parse_let(loc: Location, lexer: &mut Lexer) -> super::Result<Statement> {
    let name = expect_token(
        "in variable definition",
        loc.clone(),
        lexer,
        TokenKind::Word("".into()),
    )?;

    expect_token(
        "in variable definition",
        loc.clone(),
        lexer,
        TokenKind::Colon,
    )?;

    let datatype = expect_token(
        "in variable definition",
        loc.clone(),
        lexer,
        TokenKind::Word("".into()),
    )?;

    expect_token(
        "in variable definition",
        loc.clone(),
        lexer,
        TokenKind::Equal,
    )?;

    let expr = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest())?;

    Ok(Statement {
        loc,
        statement: StatementKind::Let {
            name: match name.token {
                TokenKind::Word(name) => name,
                _ => unreachable!(),
            },
            datatype: match datatype.token {
                TokenKind::Word(tp) => tp,
                _ => unreachable!(),
            },
            expression: Box::new(expr),
        },
    })
}

pub fn parse_statement(lexer: &mut Lexer) -> super::Result<Option<Statement>> {
    let token = if let Some(tok) = next_token(lexer)? {
        tok
    } else {
        return Ok(None);
    };

    let statement;

    match token.token {
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::Proc | Keyword::Export => {
                eprintln!(
                    "{}: ERROR: procedure definitions are only allowed as toplevel statements",
                    token.loc
                );
                return Err(());
                // statement = parse_proc(token.loc, lexer)?;
            }
            Keyword::Let => statement = parse_let(token.loc, lexer)?,
        },
        TokenKind::Word(_) => {
            if let Some(ptk) = peek_token(lexer)? {
                if ptk.token.eq(&TokenKind::OpenParen) {
                    statement = parse_proccall(token, lexer)?
                } else {
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
            } else {
                eprintln!(
                    "{loc}: ERROR: reached end of file while handling `{word:?}`",
                    loc = token.loc,
                    word = token.token,
                );
                return Err(());
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
    let token = if let Some(tok) = next_token(lexer)? {
        tok
    } else {
        return Ok(None);
    };

    match token.token {
        TokenKind::Keyword(keyword) => match keyword {
            Keyword::Proc => Ok(Some(parse_proc(token.loc, lexer, false)?)),
            Keyword::Export => {
                expect_token("in procedure definition", token.loc.clone(), lexer, TokenKind::Keyword(Keyword::Proc))?;

                Ok(Some(parse_proc(token.loc, lexer, true)?))
            }
            Keyword::Let => Ok(Some(parse_let(token.loc, lexer)?)),
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
}

impl OperatorPrecedence {
    fn lowest() -> Self {
        Self::Additive
    }

    fn higher(&self) -> Self {
        match self {
            Self::Primary => Self::Primary,
            Self::Multiplicative => Self::Primary,
            Self::Additive => Self::Multiplicative,
        }
    }

    fn matches_precedence(&self, operator: Operator) -> bool {
        match (self, operator) {
            (Self::Primary, _) => false,
            (Self::Multiplicative, Operator::Div | Operator::Mult | Operator::Mod) => true,
            (Self::Additive, Operator::Plus | Operator::Minus) => true,
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
        let token = if let Some(tok) = peek_token(lexer)? {
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
        lexer.next();

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

pub fn parse_primary_expression(
    loc: Location,
    lexer: &mut Lexer,
) -> super::Result<Expression> {
    let token = if let Some(tok) = peek_token(lexer)? {
        tok
    } else {
        eprintln!("{loc}: ERROR: reached end of file while parsing expression");
        return Err(());
    };

    match token.token {
        TokenKind::String(string) => {
            lexer.next();

            Ok(Expression {
                expression: ExpressionKind::String(string),
                loc: token.loc,
            })
        }
        TokenKind::Integer(integer) => {
            lexer.next();

            Ok(Expression {
                expression: ExpressionKind::Integer(integer),
                loc: token.loc,
            })
        }
        TokenKind::OpenParen => {
            lexer.next();

            let value = parse_expression(loc.clone(), lexer, OperatorPrecedence::lowest());
            expect_token("in expression", token.loc, lexer, TokenKind::CloseParen)?;
            value
        }
        TokenKind::Word(_) => {
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
        _ => {
            eprintln!("{}: ERROR: unexpected token `{:?}`", token.loc, token.token);
            Err(())
        }
    }
}
