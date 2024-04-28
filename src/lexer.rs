use std::fmt;

use super::Result;

fn is_special_character(c: char) -> bool {
    Token::from_char(c, Default::default()).is_some() || c.is_whitespace()
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Proc,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proc => write!(f, "proc"),
        }
    }
}

impl Keyword {
    fn from_string(text: &str) -> Option<Self> {
        match text {
            "proc" => Some(Self::Proc),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    Keyword,
    String,
    Word,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Comma,
}

#[derive(Debug, Default, Clone)]
pub struct Location<'a> {
    pub file_path: &'a str,
    pub line: u32,
    pub col: u32,
}

impl<'a> fmt::Display for Location<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.line, self.col)
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub keyword: Option<Keyword>,
    pub string: String,
    pub loc: Location<'a>,
}

impl<'a> Token<'a> {
    fn from_char(c: char, loc: Location<'a>) -> Option<Self> {
        let kind;
        match c {
            '(' => kind = Some(TokenKind::OpenParen),
            ')' => kind = Some(TokenKind::CloseParen),
            '{' => kind = Some(TokenKind::OpenBrace),
            '}' => kind = Some(TokenKind::CloseBrace),
            ';' => kind = Some(TokenKind::Semicolon),
            ',' => kind = Some(TokenKind::Comma),
            _ => kind = None,
        }

        if kind.is_none() {
            return None;
        }

        Some(Self {
            kind: kind.unwrap(),
            string: format!("{c}"),
            keyword: None,
            loc,
        })
    }

    fn from_keyword(keyword: Keyword, loc: Location<'a>) -> Self {
        Self {
            kind: TokenKind::Keyword,
            keyword: Some(keyword.clone()),
            string: format!("{keyword}"),
            loc,
        }
    }

    fn from_word(word: String, loc: Location<'a>) -> Self {
        Self {
            kind: TokenKind::Word,
            keyword: None,
            string: word,
            loc,
        }
    }

    fn from_string(string: String, loc: Location<'a>) -> Self {
        Self {
            kind: TokenKind::String,
            keyword: None,
            string,
            loc,
        }
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}: ", self.loc, self.kind)?;
        match self.kind {
            TokenKind::Keyword => write!(
                f,
                "{}",
                self.keyword.clone().expect("This should never fail")
            ),
            _ => write!(f, "\"{}\"", self.string),
        }
    }
}

pub struct Lexer<'a> {
    source_code: &'a str,
    loc: Location<'a>,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str, file_loc: &'a str) -> Lexer<'a> {
        Lexer {
            source_code,
            loc: Location {
                file_path: file_loc,
                line: 1,
                col: 1,
            },
            cursor: 0,
        }
    }

    fn cursor(&self) -> char {
        self.source_code.chars().nth(self.cursor).unwrap()
    }

    fn eof(&self) -> bool {
        self.source_code.chars().nth(self.cursor).is_none()
    }

    fn advance_loc(&mut self, c: char) {
        self.loc.col += 1;
        if c == '\n' {
            self.loc.col = 1;
            self.loc.line += 1;
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>> {
        if self.eof() {
            return Ok(None);
        }

        while self.cursor().is_whitespace() {
            self.advance_loc(self.cursor());

            self.cursor += 1;
            if self.eof() {
                return Ok(None);
            }
        }

        if let Some(token) = Token::from_char(self.cursor(), self.loc.clone()) {
            self.advance_loc(self.cursor());
            self.cursor += 1;
            return Ok(Some(token));
        }

        let string_loc = self.loc.clone();
        if self.cursor() == '"' {
            self.cursor += 1;
            if self.eof() {
                eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                return Err(());
            }
            self.advance_loc(self.cursor());

            let mut string = String::new();
            while self.cursor() != '"' {
                string.push(self.cursor());

                self.cursor += 1;
                if self.eof() {
                    eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                    return Err(());
                }
                self.advance_loc(self.cursor());
            }

            self.cursor += 1;
            if self.eof() {
                eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                return Err(());
            }
            self.advance_loc(self.cursor());

            return Ok(Some(Token::from_string(string, string_loc)));
        }

        let mut current_token_text = String::new();
        while !is_special_character(self.cursor()) {
            current_token_text.push(self.cursor());
            self.advance_loc(self.cursor());

            self.cursor += 1;
            if self.eof() {
                break;
            }
        }

        let loc = Location {
            file_path: self.loc.file_path,
            line: self.loc.line,
            col: self.loc.col - current_token_text.len() as u32,
        };

        if let Some(keyword) = Keyword::from_string(&current_token_text.as_str()) {
            return Ok(Some(Token::from_keyword(keyword, loc)));
        }

        Ok(Some(Token::from_word(current_token_text, loc)))
    }
}
