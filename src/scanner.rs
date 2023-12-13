use std::ffi::{c_char, CStr, CString};
use std::ptr;

use once_cell::sync::Lazy;

#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) start: *const c_char,
    pub(crate) length: usize,
    pub(crate) line: usize,
}

impl Token {
    pub(crate) fn zero() -> Self {
        Self {
            r#type: TokenType::Error,
            start: ptr::null_mut(),
            length: 0,
            line: 0,
        }
    }

    pub(crate) fn synthetic(text: &'static CStr) -> Self {
        let ty = match text.to_str().unwrap() {
            "super" => TokenType::Super,
            "this" => TokenType::This,
            _ => unreachable!(),
        };

        Self {
            r#type: ty,
            line: 0,
            start: text.as_ptr(),
            length: text.to_bytes().len(),
        }
    }

    pub(crate) fn text(&self) -> &str {
        let bytes = unsafe { std::slice::from_raw_parts(self.start as *const u8, self.length) };
        std::str::from_utf8(bytes).expect("utf-8 source")
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One- or two-character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Synthetic tokens
    Error,
    Eof,
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn keyword_or_identifier(start: *const c_char, current: *const c_char) -> TokenType {
    // The chars tracked by the scanner _are not_ a null-terminated string. Be careful with lengths
    // and comparisons!

    // Safety: Both pointers point into the same C string. The current ponter was derived from the
    // start pointer.
    let n: usize = (unsafe { current.offset_from(start) })
        .try_into()
        .expect("identifier is short");

    macro_rules! kwd_if {
        ($kwd:expr, $ty:expr) => {
            if str_equal($kwd, start, n) {
                return $ty;
            }
        };
    }

    kwd_if!("and", TokenType::And);
    kwd_if!("class", TokenType::Class);
    kwd_if!("else", TokenType::Else);
    kwd_if!("false", TokenType::False);
    kwd_if!("for", TokenType::For);
    kwd_if!("fun", TokenType::Fun);
    kwd_if!("if", TokenType::If);
    kwd_if!("nil", TokenType::Nil);
    kwd_if!("or", TokenType::Or);
    kwd_if!("print", TokenType::Print);
    kwd_if!("return", TokenType::Return);
    kwd_if!("super", TokenType::Super);
    kwd_if!("this", TokenType::This);
    kwd_if!("true", TokenType::True);
    kwd_if!("var", TokenType::Var);
    kwd_if!("while", TokenType::While);

    TokenType::Identifier
}

fn str_equal(a: &str, b: *const c_char, n: usize) -> bool {
    if a.len() != n {
        return false;
    }

    let a = a.as_ptr();

    for i in 0..n {
        let aa = (unsafe { *a.add(i) }) as c_char;
        let bb = unsafe { *b.add(i) };
        if aa != bb {
            return false;
        }
    }

    true
}

#[repr(C)]
pub struct Scanner {
    start: *const c_char,
    current: *const c_char,
    line: usize,
}

impl Scanner {
    pub fn new(source: *const c_char) -> Self {
        Self {
            start: source,
            current: source,
            line: 1,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_ignored();

        self.start = self.current;

        if self.at_eof() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if is_alpha(c) {
            return self.scan_identifier();
        }
        if is_digit(c) {
            return self.scan_number();
        }

        match c {
            // One character, no lookahead
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),

            // One or two characters, 1 lookahead
            '!' => {
                if self.match_('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }

            '"' => self.scan_string(),

            _ => {
                static UNEXPECTED_CHARACTER: Lazy<CString> =
                    Lazy::new(|| CString::new("Unexpected character.").unwrap());
                self.make_error(&UNEXPECTED_CHARACTER)
            }
        }
    }

    fn skip_ignored(&mut self) {
        loop {
            match self.peek() {
                // Whitespace
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }

                // Comments
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.at_eof() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }

                _ => return,
            }
        }
    }

    fn scan_identifier(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }

        let ty = keyword_or_identifier(self.start, self.current);
        self.make_token(ty)
    }

    fn scan_number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        // Only consume the dot if there are also more digits afterward. Otherwise, it indicates a
        // method call rather than a decimal number.
        if self.peek() == '.' && is_digit(self.peek_next()) {
            self.advance(); // `.`

            while is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn scan_string(&mut self) -> Token {
        while self.peek() != '"' && !self.at_eof() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_eof() {
            static UNTERMINATED_STRING: Lazy<CString> =
                Lazy::new(|| CString::new("Unterminated string.").unwrap());

            return self.make_error(&UNTERMINATED_STRING);
        }

        self.advance(); // peek `"`
        self.make_token(TokenType::String)
    }

    fn at_eof(&self) -> bool {
        self.peek() == '\0'
    }

    fn peek(&self) -> char {
        // Safety: This points into the source text, which is a null-terminated C string that
        // lives for at least as long as the scanner.
        (unsafe { *self.current }) as u8 as char
    }

    fn peek_next(&self) -> char {
        if self.at_eof() {
            return '\0';
        }

        // Safety: This points into the source text, which is a null-terminated C string that
        // lives for at least as long as the scanner.
        (unsafe { *self.current.add(1) }) as u8 as char
    }

    fn advance(&mut self) -> char {
        // Safety: This points into the source text, which is a null-terminated C string that
        // lives for at least as long as the scanner.
        self.current = unsafe { self.current.add(1) };

        (unsafe { *self.current.sub(1) }) as u8 as char
    }

    fn match_(&mut self, expected: char) -> bool {
        if self.at_eof() {
            return false;
        }

        if self.peek() != expected {
            return false;
        }

        self.advance();
        true
    }

    fn make_token(&self, ty: TokenType) -> Token {
        let length = (self.current as usize) - (self.start as usize);

        Token {
            r#type: ty,
            start: self.start,

            length,
            line: self.line,
        }
    }

    fn make_error(&self, msg: &CStr) -> Token {
        Token {
            r#type: TokenType::Error,
            start: msg.as_ptr() as *const c_char,
            length: msg.to_bytes().len(),
            line: self.line,
        }
    }
}
