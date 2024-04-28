use crate::lexer::{Keyword, LexerError, Location, Token, TokenKind};
use crate::lexer_type;

use std::iter::Peekable;

#[derive(Debug)]
pub enum ExpressionKind {
    String,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub left: Option<Box<Expression>>,
    pub right: Option<Box<Expression>>,
    pub string: Option<String>,
}

#[derive(Debug)]
pub enum Statement {
    ProcDecl {
        name: String,
        statements: Vec<Statement>,
    },
    ProcCall {
        name: String,
        expressions: Vec<Expression>,
    },
}

fn peek_token<'a>(lexer: &mut lexer_type!()) -> super::Result<Option<Token<'a>>> {
    if let Some(x) = lexer.peek() {
        match x {
            Ok(token) => return Ok(Some(token.clone())),
            Err(err) => match err {
                LexerError::Eof(_) => return Ok(None),
                e => {
                    eprintln!("{e}");
                    return Err(());
                }
            },
        }
    }
    Ok(None)
}

fn expect_token<'a>(
    inn: &str,
    loc: Location<'a>,
    lexer: &mut lexer_type!(),
    token: TokenKind,
) -> super::Result<Token<'a>> {
    let t = match peek_token(lexer)? {
        Some(x) => x,
        None => {
            eprintln!("{loc}: ERROR: expected token `{token:?}` {inn} but got nothing");
            return Err(());
        }
    };

    if t.kind != token {
        eprintln!(
            "{loc}: ERROR: expected token `{token:?}` in {inn} but got `{got:?}`",
            loc = t.loc,
            got = t.kind
        );
        return Err(());
    }

    lexer.next();
    Ok(t)
}

pub fn parse_block<'a>(
    loc: Location<'a>,
    lexer: &mut lexer_type!(),
) -> super::Result<Vec<Statement>> {
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

        if token.kind == TokenKind::CloseBrace {
            break;
        }

        let statement = if let Some(s) = parse_statement(lexer)? {
            s
        } else {
            eprintln!(
                "{}: ERROR: reached end of file while parsing statemend",
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

pub fn parse_proc<'a>(lexer: &mut lexer_type!()) -> super::Result<Statement> {
    let proc = lexer.next().unwrap().unwrap();

    let name = expect_token(
        "in procedure definition",
        proc.loc.clone(),
        lexer,
        TokenKind::Word,
    )?;
    let _open_paren = expect_token(
        "in procedure definition",
        proc.loc.clone(),
        lexer,
        TokenKind::OpenParen,
    )?;
    let close_paren = expect_token(
        "in procedure definition",
        proc.loc,
        lexer,
        TokenKind::CloseParen,
    )?;

    let block = parse_block(close_paren.loc, lexer)?;

    Ok(Statement::ProcDecl {
        name: name.string,
        statements: block,
    })
}

pub fn parse_proccall<'a>(lexer: &mut lexer_type!()) -> super::Result<Statement> {
    let name = lexer.next().unwrap().unwrap();

    let open_paren = expect_token(
        "in procedure call",
        name.loc.clone(),
        lexer,
        TokenKind::OpenParen,
    )?;

    let string = if let Some(s) = peek_token(lexer)? {
        s
    } else {
        eprintln!(
            "{}: ERROR: reached end of file while parsing procedure call parameters",
            open_paren.loc
        );
        return Err(());
    };
    if string.kind != TokenKind::String {
        eprintln!("{}: ERROR: for now, only procedure calls that take one string as parameter are allowed", string.loc);
        return Err(());
    }
    lexer.next();

    let _close_paren = expect_token("in procedure call", name.loc, lexer, TokenKind::CloseParen)?;

    let mut parameters = Vec::new();
    parameters.push(Expression {
        kind: ExpressionKind::String,
        left: None,
        right: None,
        string: Some(string.string),
    });

    Ok(Statement::ProcCall {
        name: name.string,
        expressions: parameters,
    })
}

pub fn parse_statement<'a>(lexer: &mut lexer_type!()) -> super::Result<Option<Statement>> {
    let token = if let Some(tok) = peek_token(lexer)? {
        tok
    } else {
        return Ok(None);
    };

    let statement;

    match token.kind {
        TokenKind::Keyword => match token.keyword.unwrap() {
            Keyword::Proc => statement = parse_proc(lexer)?,
        },
        TokenKind::Word => statement = parse_proccall(lexer)?,
        _ => {
            eprintln!("{}: ERROR: unexpected token `{}`", token.loc, token.string,);
            return Err(());
        }
    }

    Ok(Some(statement))
}

pub fn parse_statement_toplevel<'a>(lexer: &mut lexer_type!()) -> super::Result<Option<Statement>> {
    let token = if let Some(tok) = peek_token(lexer)? {
        tok
    } else {
        return Ok(None);
    };

    match token.kind {
        TokenKind::Keyword => match token.keyword.unwrap() {
            Keyword::Proc => Ok(Some(parse_proc(lexer)?)),
        },
        _ => {
            eprintln!(
                "{}: ERROR: no toplevel statement starts with token `{}`",
                token.loc, token.string,
            );
            Err(())
        }
    }
}
