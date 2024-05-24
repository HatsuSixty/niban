use std::fmt;

fn is_special_character(c: char) -> bool {
    Token::from_symbols(&[c], Location::new("")).is_some() || c.is_whitespace()
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Proc,
    Let,
    Export,
    If,
    Else,
    While,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Proc => write!(f, "proc"),
            Self::Let => write!(f, "let"),
            Self::Export => write!(f, "export"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::While => write!(f, "while"),
        }
    }
}

impl Keyword {
    fn from_string(text: &str) -> Option<Self> {
        match text {
            "proc" => Some(Self::Proc),
            "let" => Some(Self::Let),
            "export" => Some(Self::Export),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "while" => Some(Self::While),
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

    Lt,
    Gt,
    DoubleEqual,
    LessEqual,
    GreaterEqual,
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
    // Returns token and consumed characters
    fn from_symbols(chars: &[char], loc: Location) -> Option<(Self, usize)> {
        let kind;
        let mut used_chars = 1;
        match chars[0] {
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
            '=' => {
                if chars.len() > 1 {
                    if chars[1] == '=' {
                        kind = Some(TokenKind::DoubleEqual);
                        used_chars = 2;
                    } else {
                        kind = Some(TokenKind::Equal);
                    }
                } else {
                    kind = Some(TokenKind::Equal);
                }
            }
            ':' => kind = Some(TokenKind::Colon),
            '<' => {
                if chars.len() > 1 {
                    if chars[1] == '=' {
                        kind = Some(TokenKind::LessEqual);
                        used_chars = 2;
                    } else {
                        kind = Some(TokenKind::Lt);
                    }
                } else {
                    kind = Some(TokenKind::Lt);
                }
            }
            '>' => {
                if chars.len() > 1 {
                    if chars[1] == '=' {
                        kind = Some(TokenKind::GreaterEqual);
                        used_chars = 2;
                    } else {
                        kind = Some(TokenKind::Gt);
                    }
                } else {
                    kind = Some(TokenKind::Gt);
                }
            }
            _ => kind = None,
        }

        if kind.is_none() {
            return None;
        }

        Some((Self {
            token: kind.unwrap(),
            loc,
        }, used_chars))
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

pub struct Lexer<'a> {
    source_code: &'a str,
    loc: Location,
    cursor: usize,
    peek: Option<super::Result<Option<Token>>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source_code: &'a str, file_loc: &'a str) -> Lexer<'a> {
        Lexer {
            source_code,
            loc: Location::new(file_loc),
            cursor: 0,
            peek: None,
        }
    }

    pub fn peek(&mut self) -> super::Result<Option<Token>> {
        if let Some(p) = &self.peek {
            p.clone()
        } else {
            panic!("You called peek before next");
        }
    }

    pub fn next(&mut self) -> super::Result<Option<Token>> {
        if self.peek.is_some() {
            let p = self.peek.clone();
            self.peek = Some(self.next_token());
            p.unwrap()
        } else {
            let ret = self.next_token();
            self.peek = Some(self.next_token());
            ret
        }
    }

    pub fn get_loc(&mut self) -> Location {
        if self.eof() {
            return self.loc.clone();
        }

        while self.cursor().is_whitespace() {
            self.advance_cursor();
            if self.eof() {
                break;
            }
        }
        self.loc.clone()
    }

    fn peek_cursor(&self) -> Option<char> {
        self.source_code.chars().nth(self.cursor + 1)
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

    fn next_token(&mut self) -> super::Result<Option<Token>> {
        if self.eof() {
            return Ok(None);
        }

        while self.cursor().is_whitespace() {
            self.advance_cursor();
            if self.eof() {
                return Ok(None);
            }
        }


        {
            let chars = if let Some(c) = self.peek_cursor() {
                vec![self.cursor(), c]
            } else {
                vec![self.cursor()]
            };
            if let Some((token, used_chars)) = Token::from_symbols(chars.as_slice(), self.loc.clone()) {
                for _ in 0..used_chars {
                    self.advance_cursor();
                }
                return Ok(Some(token));
            }
        }

        let string_loc = self.loc.clone();
        if self.cursor() == '"' {
            self.advance_cursor();
            if self.eof() {
                eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                return Err(());
            }

            let mut string = String::new();
            while self.cursor() != '"' {
                string.push(self.cursor());

                self.advance_cursor();
                if self.eof() {
                    eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                    return Err(());
                }
            }

            self.advance_cursor();
            if self.eof() {
                eprintln!("{string_loc}: ERROR: reached end of file while parsing string");
                return Err(());
            }

            return Ok(Some(Token::from_string(string, string_loc)));
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
            return Ok(Some(Token::from_keyword(keyword, loc)));
        }

        if let Ok(num) = current_token_text.parse::<i64>() {
            Ok(Some(Token::from_integer(num, loc)))
        } else {
            Ok(Some(Token::from_word(current_token_text, loc)))
        }
    }
}
