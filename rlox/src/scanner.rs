use std::ffi::{c_char, c_int, CStr, CString};

use once_cell::sync::Lazy;

#[derive(Debug)]
#[repr(C)]
pub struct Token {
    r#type: TokenType,
    start: *const c_char,
    length: c_int,
    line: c_int,
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[repr(C)]
#[derive(Debug)]
pub enum TokenType {
    // Single-character tokens
    TokenLeftParen,
    TokenRightParen,
    TokenLeftBrace,
    TokenRightBrace,
    TokenComma,
    TokenDot,
    TokenMinus,
    TokenPlus,
    TokenSemicolon,
    TokenSlash,
    TokenStar,

    // One- or two-character tokens
    TokenBang,
    TokenBangEqual,
    TokenEqual,
    TokenEqualEqual,
    TokenGreater,
    TokenGreaterEqual,
    TokenLess,
    TokenLessEqual,

    // Literals
    TokenIdentifier,
    TokenString,
    TokenNumber,

    // Keywords
    TokenAnd,
    TokenClass,
    TokenElse,
    TokenFalse,
    TokenFor,
    TokenFun,
    TokenIf,
    TokenNil,
    TokenOr,
    TokenPrint,
    TokenReturn,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenWhile,

    // Synthetic tokens
    TokenError,
    TokenEof,
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

    kwd_if!("and", TokenType::TokenAnd);
    kwd_if!("class", TokenType::TokenClass);
    kwd_if!("else", TokenType::TokenElse);
    kwd_if!("false", TokenType::TokenFalse);
    kwd_if!("for", TokenType::TokenFor);
    kwd_if!("fun", TokenType::TokenFun);
    kwd_if!("if", TokenType::TokenIf);
    kwd_if!("nil", TokenType::TokenNil);
    kwd_if!("or", TokenType::TokenOr);
    kwd_if!("print", TokenType::TokenPrint);
    kwd_if!("return", TokenType::TokenReturn);
    kwd_if!("super", TokenType::TokenSuper);
    kwd_if!("this", TokenType::TokenThis);
    kwd_if!("true", TokenType::TokenTrue);
    kwd_if!("var", TokenType::TokenVar);
    kwd_if!("while", TokenType::TokenWhile);

    TokenType::TokenIdentifier
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
    line: c_int,
}

impl Scanner {
    pub fn init(&mut self, source: *const c_char) {
        self.start = source;
        self.current = source;
        self.line = 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_ignored();

        self.start = self.current;

        if self.at_eof() {
            return self.make_token(TokenType::TokenEof);
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
            '(' => self.make_token(TokenType::TokenLeftParen),
            ')' => self.make_token(TokenType::TokenRightParen),
            '{' => self.make_token(TokenType::TokenLeftBrace),
            '}' => self.make_token(TokenType::TokenRightBrace),
            ';' => self.make_token(TokenType::TokenSemicolon),
            ',' => self.make_token(TokenType::TokenComma),
            '.' => self.make_token(TokenType::TokenDot),
            '-' => self.make_token(TokenType::TokenMinus),
            '+' => self.make_token(TokenType::TokenPlus),
            '/' => self.make_token(TokenType::TokenSlash),
            '*' => self.make_token(TokenType::TokenStar),

            // One or two characters, 1 lookahead
            '!' => {
                if self.match_('=') {
                    self.make_token(TokenType::TokenBangEqual)
                } else {
                    self.make_token(TokenType::TokenBang)
                }
            }
            '=' => {
                if self.match_('=') {
                    self.make_token(TokenType::TokenEqualEqual)
                } else {
                    self.make_token(TokenType::TokenEqual)
                }
            }
            '<' => {
                if self.match_('=') {
                    self.make_token(TokenType::TokenLessEqual)
                } else {
                    self.make_token(TokenType::TokenLess)
                }
            }
            '>' => {
                if self.match_('=') {
                    self.make_token(TokenType::TokenGreaterEqual)
                } else {
                    self.make_token(TokenType::TokenGreater)
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

        self.make_token(TokenType::TokenNumber)
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
        self.make_token(TokenType::TokenString)
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

            length: length.try_into().expect("token length fits in i32"),
            line: self.line,
        }
    }

    fn make_error(&self, msg: &CStr) -> Token {
        Token {
            r#type: TokenType::TokenError,
            start: msg.as_ptr() as *const c_char,
            length: msg
                .to_bytes()
                .len()
                .try_into()
                .expect("short error message"),
            line: self.line,
        }
    }
}
