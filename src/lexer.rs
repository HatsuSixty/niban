use std::fmt;

#[macro_export]
macro_rules! lexer_type {
    () => {
        Peekable<impl Iterator<Item=Result<Token, LexerError>>>
    }
}

fn is_special_character(c: char) -> bool {
    Token::from_char(c, Location::new("")).is_some() || c.is_whitespace()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Proc,
    Let,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proc => write!(f, "proc"),
            Self::Let => write!(f, "let"),
        }
    }
}

impl Keyword {
    fn from_string(text: &str) -> Option<Self> {
        match text {
            "proc" => Some(Self::Proc),
            "let" => Some(Self::Let),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    String(String),
    Word(String),
    Integer(i64),

    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,

    Colon,
    Semicolon,
    Comma,
    Equal,

    Plus,
    Minus,
    Div,
    Mult,
    Mod,
}

impl TokenKind {
    pub fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(_), Self::String(_)) => true,
            (Self::Word(_), Self::Word(_)) => true,
            (Self::Integer(_), Self::Integer(_)) => true,
            _ => self == other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file_path: String,
    pub line: u32,
    pub col: u32,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.line, self.col)
    }
}

impl Location {
    fn new(file_path: &str) -> Self {
        Self {
            file_path: file_path.to_string(),
            line: 1,
            col: 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token: TokenKind,
    pub loc: Location,
}

impl Token {
    fn from_char(c: char, loc: Location) -> Option<Self> {
        let kind;
        match c {
            '(' => kind = Some(TokenKind::OpenParen),
            ')' => kind = Some(TokenKind::CloseParen),
            '{' => kind = Some(TokenKind::OpenBrace),
            '}' => kind = Some(TokenKind::CloseBrace),
            ';' => kind = Some(TokenKind::Semicolon),
            ',' => kind = Some(TokenKind::Comma),
            '+' => kind = Some(TokenKind::Plus),
            '-' => kind = Some(TokenKind::Minus),
            '/' => kind = Some(TokenKind::Div),
            '*' => kind = Some(TokenKind::Mult),
            '%' => kind = Some(TokenKind::Mod),
            '=' => kind = Some(TokenKind::Equal),
            ':' => kind = Some(TokenKind::Colon),
            _ => kind = None,
        }

        if kind.is_none() {
            return None;
        }

        Some(Self {
            token: kind.unwrap(),
            loc,
        })
    }

    fn from_keyword(keyword: Keyword, loc: Location) -> Self {
        Self {
            token: TokenKind::Keyword(keyword),
            loc,
        }
    }

    fn from_word(word: String, loc: Location) -> Self {
        Self {
            token: TokenKind::Word(word),
            loc,
        }
    }

    fn from_integer(integer: i64, loc: Location) -> Self {
        Self {
            token: TokenKind::Integer(integer),
            loc,
        }
    }

    fn from_string(string: String, loc: Location) -> Self {
        Self {
            token: TokenKind::String(string),
            loc,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {:?}: ", self.loc, self.token)?;
        match &self.token {
            TokenKind::Keyword(keyword) => write!(f, "{}", keyword),
            TokenKind::String(string) => write!(f, "\"{}\"", string),
            TokenKind::Word(word) => write!(f, "{}", word),
            _ => write!(f, "{:?}", self.token),
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    Eof(Location),
    StringEof(Location),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof(loc) => write!(f, "{loc}: ERROR: reached end of file"),
            Self::StringEof(loc) => {
                write!(f, "{loc}: ERROR: reached end of file while parsing string")
            }
        }
    }
}

pub struct Lexer<'a> {
    source_code: &'a str,
    loc: Location,
    cursor: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str, file_loc: &'a str) -> Lexer<'a> {
        Lexer {
            source_code,
            loc: Location::new(file_loc),
            cursor: 0,
        }
    }

    fn cursor(&self) -> char {
        self.source_code.chars().nth(self.cursor).unwrap()
    }

    fn eof(&self) -> bool {
        self.source_code.chars().nth(self.cursor).is_none()
    }

    fn advance_cursor(&mut self) {
        self.loc.col += 1;
        if self.cursor() == '\n' {
            self.loc.col = 1;
            self.loc.line += 1;
        }

        self.cursor += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if self.eof() {
            return Err(LexerError::Eof(self.loc.clone()));
        }

        while self.cursor().is_whitespace() {
            self.advance_cursor();
            if self.eof() {
                return Err(LexerError::Eof(self.loc.clone()));
            }
        }

        if let Some(token) = Token::from_char(self.cursor(), self.loc.clone()) {
            self.advance_cursor();
            return Ok(token);
        }

        let string_loc = self.loc.clone();
        if self.cursor() == '"' {
            self.advance_cursor();
            if self.eof() {
                return Err(LexerError::StringEof(string_loc));
            }

            let mut string = String::new();
            while self.cursor() != '"' {
                string.push(self.cursor());

                self.advance_cursor();
                if self.eof() {
                    return Err(LexerError::StringEof(string_loc));
                }
            }

            self.advance_cursor();
            if self.eof() {
                return Err(LexerError::StringEof(string_loc));
            }

            return Ok(Token::from_string(string, string_loc));
        }

        let mut current_token_text = String::new();
        while !is_special_character(self.cursor()) {
            current_token_text.push(self.cursor());

            self.advance_cursor();
            if self.eof() {
                break;
            }
        }

        let loc = Location {
            file_path: self.loc.file_path.clone(),
            line: self.loc.line,
            col: self.loc.col - current_token_text.len() as u32,
        };

        if let Some(keyword) = Keyword::from_string(&current_token_text.as_str()) {
            return Ok(Token::from_keyword(keyword, loc));
        }

        if let Ok(num) = current_token_text.parse::<i64>() {
            Ok(Token::from_integer(num, loc))
        } else {
            Ok(Token::from_word(current_token_text, loc))
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.next_token() {
            Ok(token) => Some(Ok(token)),
            Err(err) => match err {
                LexerError::Eof(_) => None,
                _ => Some(Err(err)),
            },
        }
    }
}
