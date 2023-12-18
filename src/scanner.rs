#[derive(Debug, Copy, Clone, Default)]
#[repr(C)]
pub struct Token<'a> {
    pub(crate) r#type: TokenType,
    pub(crate) text: &'a str,
    pub(crate) line: usize,
}

impl Token<'_> {
    pub(crate) fn synthetic(text: &'static str) -> Self {
        let ty = match text {
            "super" => TokenType::Super,
            "this" => TokenType::This,
            _ => unreachable!(),
        };

        Self {
            r#type: ty,
            line: 0,
            text,
        }
    }

    pub(crate) fn text(&self) -> &str {
        self.text
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, Hash)]
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
    #[default]
    Error,
    Eof,
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn keyword_or_identifier(text: &str) -> TokenType {
    macro_rules! kwd_if {
        ($kwd:expr, $ty:expr) => {
            if text == $kwd {
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

#[repr(C)]
pub struct Scanner<'source> {
    source: &'source str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn next_token(&mut self) -> Token<'source> {
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

            _ => self.make_error("Unexpected character."),
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

    fn scan_identifier(&mut self) -> Token<'source> {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];

        let ty = keyword_or_identifier(text);
        self.make_token(ty)
    }

    fn scan_number(&mut self) -> Token<'source> {
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

    fn scan_string(&mut self) -> Token<'source> {
        while self.peek() != '"' && !self.at_eof() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_eof() {
            return self.make_error("Unterminated string.");
        }

        self.advance(); // peek `"`
        self.make_token(TokenType::String)
    }

    fn at_eof(&self) -> bool {
        self.peek() == '\0'
    }

    fn peek(&self) -> char {
        self.source[self.current..]
            .chars()
            .next()
            .unwrap_or_default()
    }

    fn peek_next(&self) -> char {
        if self.at_eof() {
            return '\0';
        }

        let mut chars = self.source[self.current..].chars();
        chars.next().unwrap(); // peek 0
        chars.next().unwrap_or_default() // peek 1
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += c.len_utf8();
        c
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

    fn make_token(&self, ty: TokenType) -> Token<'source> {
        let text = &self.source[self.start..self.current];

        Token {
            r#type: ty,
            text,
            line: self.line,
        }
    }

    fn make_error<'msg>(&self, msg: &'msg str) -> Token<'msg> {
        Token {
            r#type: TokenType::Error,
            text: msg,
            line: self.line,
        }
    }
}
