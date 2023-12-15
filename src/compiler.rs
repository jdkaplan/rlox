use std::collections::HashMap;
use std::ptr;

use once_cell::sync::Lazy;

use crate::alloc::Gc;
use crate::chunk::{Chunk, Opcode};
use crate::object::{Obj, ObjFunction, ObjString};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::vm::Vm;
use crate::U8_COUNT;

#[repr(C)]
pub struct Parser<'source> {
    current: Token<'source>,
    previous: Token<'source>,

    scanner: *mut Scanner<'source>,
    compiler: *mut Compiler<'source>,
    klass: *mut ClassCompiler,

    had_error: bool,
    panicking: bool,

    vm: *mut Vm,
}

#[repr(C)]
pub struct Compiler<'source> {
    pub(crate) enclosing: *mut Compiler<'source>,
    pub(crate) function: *mut ObjFunction,
    mode: FunctionMode,

    locals: Vec<Local<'source>>,

    // The maximum upvalue index is tracked by function, so it's not easy to make this a Vec,
    // even though it seems like it should be one.
    upvalues: [Upvalue; U8_COUNT],

    scope_depth: usize,
}

impl<'enclosing> Compiler<'enclosing> {
    fn new(
        enclosing: *mut Compiler<'enclosing>,
        function: *mut ObjFunction,
        mode: FunctionMode,
    ) -> Self {
        let mut compiler = Self {
            enclosing,
            function,
            mode,

            locals: Vec::with_capacity(U8_COUNT),
            upvalues: [Upvalue::default(); U8_COUNT],

            scope_depth: 0,
        };
        // Reserve stack slot zero for the currently executing function.
        compiler.reserve_self_slot();
        compiler
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum FunctionMode {
    Function,
    Initializer,
    Method,
    Script,
}

#[repr(C)]
pub struct ClassCompiler {
    enclosing: *mut ClassCompiler,
    has_superclass: bool,
}

#[derive(Debug, Copy, Clone, Default)]
#[repr(C)]
pub struct Local<'source> {
    name: Token<'source>,
    depth: Option<usize>,
    is_captured: bool,
}

#[derive(Debug, Copy, Clone, Default)]
#[repr(C)]
pub struct Upvalue {
    index: u8,
    is_local: bool,
}

pub(crate) fn compile(vm: *mut Vm, source: &str) -> *mut ObjFunction {
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut scanner, vm);

    parser.start_compiler(FunctionMode::Script);

    while !parser.match_(TokenType::Eof) {
        parser.declaration();
    }

    let fun = parser.end_compiler();
    if parser.had_error {
        ptr::null_mut()
    } else {
        fun
    }
}

impl<'source> Parser<'source> {
    pub(crate) fn new(scanner: &mut Scanner<'source>, vm: *mut Vm) -> Self {
        let mut parser = Self {
            // Will be overwritten by the immediate call to advance.
            current: Token::default(),
            previous: Token::default(),

            compiler: ptr::null_mut(),
            klass: ptr::null_mut(),

            had_error: false,
            panicking: false,

            scanner,
            vm,
        };
        parser.advance();
        parser
    }

    pub(crate) fn start_compiler(&mut self, mode: FunctionMode) {
        let gc = Gc::new(self.compiler, self.vm);
        let function = ObjFunction::new(gc);

        let compiler = Compiler::new(self.compiler, function, mode);

        self.compiler = Box::into_raw(Box::new(compiler));
    }

    pub(crate) fn end_compiler(&mut self) -> *mut ObjFunction {
        self.emit_return();

        let compiler = unsafe { Box::from_raw(self.compiler) };
        let function = unsafe { compiler.function.as_mut().unwrap() };

        #[cfg(feature = "print_code")]
        if !self.had_error {
            self.current_chunk().disassemble(&function.name());
        }

        self.compiler = compiler.enclosing;
        function
    }

    pub(crate) fn start_panic(&mut self, msg: &str) {
        // Avoid emitting errors while panicking. This flag will be cleared by error
        // recovery.
        if self.panicking {
            return;
        }
        self.panicking = true;

        eprintln!("{}", msg);
        self.had_error = true;
    }

    pub(crate) fn format_error(token: &Token, msg: &str) -> String {
        let line = token.line;
        let loc = match token.r#type {
            TokenType::Eof => Some(String::from("end")),
            TokenType::Error => None, // No location info for synthetic token
            _ => Some(format!("'{}'", token.text())),
        };

        if let Some(loc) = loc {
            format!("[line {}] Error at {}: {}", line, loc, msg)
        } else {
            format!("[line {}] Error: {}", line, msg)
        }
    }

    pub(crate) fn error(&mut self, msg: &str) {
        let err = Self::format_error(&self.previous, msg);
        self.start_panic(&err);
    }

    pub(crate) fn error_at_current(&mut self, msg: &str) {
        let err = Self::format_error(&self.current, msg);
        self.start_panic(&err);
    }

    pub(crate) fn advance(&mut self) {
        self.previous = self.current;
        loop {
            self.current = unsafe { &mut *self.scanner }.next_token();

            if self.current.r#type != TokenType::Error {
                break;
            }

            let text = String::from(self.current.text());
            self.error_at_current(&text);
        }
    }

    pub(crate) fn check(&self, ty: TokenType) -> bool {
        self.current.r#type == ty
    }

    pub(crate) fn match_(&mut self, ty: TokenType) -> bool {
        if !self.check(ty) {
            return false;
        }
        self.advance();
        true
    }

    pub(crate) fn consume(&mut self, ty: TokenType, msg: &str) {
        if self.current.r#type == ty {
            self.advance();
            return;
        }
        self.error_at_current(msg);
    }

    pub(crate) fn synchronize(&mut self) {
        self.panicking = false;

        while self.current.r#type != TokenType::Eof {
            if self.previous.r#type == TokenType::Semicolon {
                return;
            }

            match self.current.r#type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,

                _ => self.advance(),
            }
        }
    }
}

impl Parser<'_> {
    pub(crate) fn current_chunk(&mut self) -> &mut Chunk {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        let function = unsafe { compiler.function.as_mut().unwrap() };
        &mut function.chunk
    }

    pub(crate) fn emit_byte(&mut self, byte: impl Into<u8>) {
        let gc = Gc::new(self.compiler, self.vm);

        let line = self.previous.line;
        self.current_chunk().write_byte(gc, byte.into(), line)
    }

    pub(crate) fn emit_bytes(&mut self, b1: impl Into<u8>, b2: impl Into<u8>) {
        let gc = Gc::new(self.compiler, self.vm);

        let line = self.previous.line;
        let chunk = self.current_chunk();
        chunk.write_byte(gc, b1.into(), line);
        chunk.write_byte(gc, b2.into(), line);
    }

    pub(crate) fn emit_loop(&mut self, start: usize) {
        self.emit_byte(Opcode::Loop);

        let offset = self.current_chunk().code.len() - start + 2;
        if offset > u16::MAX.into() {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    pub(crate) fn emit_jump(&mut self, op: Opcode) -> usize {
        self.emit_byte(op);
        self.emit_bytes(0xff, 0xff);
        self.current_chunk().code.len() - 2
    }

    pub(crate) fn patch_jump(&mut self, offset: usize) {
        // Walk two extra bytes past the offset itself.
        let chunk = self.current_chunk();
        let jump = chunk.code.len() - offset - 2;

        if jump > u16::MAX.into() {
            self.error("Too much code to jump over.");
            return;
        }

        chunk.code.set(offset, ((jump >> 8) & 0xff) as u8);
        chunk.code.set(offset + 1, (jump & 0xff) as u8);
    }

    pub(crate) fn emit_return(&mut self) {
        if unsafe { self.compiler.as_mut().unwrap() }.mode == FunctionMode::Initializer {
            self.emit_bytes(Opcode::GetLocal, 0);
        } else {
            self.emit_byte(Opcode::Nil);
        }
        self.emit_byte(Opcode::Return);
    }

    pub(crate) fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(Opcode::Constant, constant);
    }

    pub(crate) fn make_constant(&mut self, value: Value) -> u8 {
        let gc = Gc::new(self.compiler, self.vm);

        if self.current_chunk().constants.len() > u8::MAX.into() {
            self.error("Too many constants in one chunk.");
            return 0;
        }

        self.current_chunk().add_constant(gc, value)
    }
}

impl<'source> Parser<'source> {
    pub(crate) fn scope_begin(&mut self) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        compiler.scope_depth += 1;
    }

    pub(crate) fn scope_end(&mut self) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        compiler.scope_depth -= 1;

        // TODO: OP_POP_N/OP_CLOSE_UPVALUE_N is a good optimization here.
        while let Some(local) = compiler.locals.last() {
            let Some(depth) = local.depth else {
                break;
            };
            if depth <= compiler.scope_depth {
                break;
            }

            if local.is_captured {
                self.emit_byte(Opcode::CloseUpvalue);
            } else {
                self.emit_byte(Opcode::Pop);
            }
            compiler.locals.pop();
        }
    }

    pub(crate) fn add_local<'token: 'source>(&mut self, name: Token<'token>) {
        if unsafe { self.compiler.as_mut().unwrap() }.locals.len() == U8_COUNT {
            self.error("Too many local variables in function.");
            return;
        }

        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        compiler.locals.push(Local {
            name,
            depth: None, // declared: reserved without value
            is_captured: false,
        });
    }
}

impl Compiler<'_> {
    fn reserve_self_slot(&mut self) {
        let name = if self.mode != FunctionMode::Function {
            Token::synthetic("this")
        } else {
            Token::default()
        };

        let local = Local {
            name,
            depth: Some(0),
            is_captured: false,
        };

        self.locals.push(local);
    }

    pub(crate) fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            // Global variables don't have their initialization tracked like this.
            return;
        }

        let local = self.locals.last_mut().unwrap();
        local.depth = Some(self.scope_depth);
    }
}

impl Parser<'_> {
    pub(crate) fn identifier_constant(&mut self, name: *const Token) -> u8 {
        let gc = Gc::new(self.compiler, self.vm);

        let name = unsafe { name.as_ref().unwrap() };

        let s = ObjString::from_str(gc, name.text);
        self.make_constant(Value::obj(s as *mut Obj))
    }

    pub(crate) fn resolve_local(&mut self, compiler: &Compiler, name: &Token) -> Option<usize> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.

        for (i, local) in compiler.locals.iter().enumerate().rev() {
            if identifiers_equal(name, &local.name) {
                if local.depth.is_none() {
                    self.error("Can't read local variable in its own initializer.");
                }
                return Some(i);
            }
        }

        None
    }

    pub(crate) fn add_upvalue(
        &mut self,
        compiler: &mut Compiler,
        index: u8,
        is_local: bool,
    ) -> Option<usize> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.
        let function = unsafe { compiler.function.as_mut().unwrap() };
        let upvalue_count = function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues[i];
            if upvalue.index == index && upvalue.is_local == is_local {
                return Some(i);
            }
        }

        if function.upvalue_count == U8_COUNT {
            self.error("Too many closure variables in function.");
            return None;
        }

        let upvalue = &mut compiler.upvalues[function.upvalue_count];
        upvalue.index = index;
        upvalue.is_local = is_local;

        function.upvalue_count += 1;
        Some(function.upvalue_count - 1) // The _previous_ count is the new item's index
    }

    pub(crate) fn resolve_upvalue(
        &mut self,
        compiler: &mut Compiler,
        name: &Token,
    ) -> Option<usize> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.

        let Some(enclosing) = (unsafe { compiler.enclosing.as_mut() }) else {
            return None;
        };

        if let Some(local) = self.resolve_local(enclosing, name) {
            enclosing.locals.get_mut(local).unwrap().is_captured = true;
            return self.add_upvalue(compiler, local as u8, true);
        }

        if let Some(upvalue) = self.resolve_upvalue(enclosing, name) {
            return self.add_upvalue(compiler, upvalue as u8, false);
        }

        None
    }

    pub(crate) fn declare_variable(&mut self) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        if compiler.scope_depth == 0 {
            // Globals are late-bound and don't have a location on the stack.
            return;
        }

        let name = self.previous;
        for local in compiler.locals.iter().rev() {
            if local.depth < Some(compiler.scope_depth) {
                break;
            }

            if identifiers_equal(&name, &local.name) {
                self.error("Already a variable with this name in this scope.");
            }
        }
        self.add_local(name);
    }

    pub(crate) fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(TokenType::Identifier, msg);

        self.declare_variable();

        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        if compiler.scope_depth > 0 {
            return 0;
        }

        self.identifier_constant(&self.previous)
    }

    pub(crate) fn define_variable(&mut self, global: u8) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        if compiler.scope_depth > 0 {
            compiler.mark_initialized();
            // No runtime code to execute! The value is already in the stack slot.
            return;
        }
        self.emit_bytes(Opcode::DefineGlobal, global);
    }
}

// Grammar

static RULES: Lazy<Rules> = Lazy::new(make_rules);

pub(crate) type ParseFn = fn(parser: &mut Parser, can_assign: bool);

pub(crate) struct Rules(HashMap<TokenType, ParseRule>);

pub(crate) struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    pub(crate) fn new(
        prefix: Option<ParseFn>,
        infix: Option<ParseFn>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
    Unused,
}

impl Precedence {
    pub(crate) fn plus_one(self) -> Self {
        use Precedence as P;
        match self {
            P::None => P::Assignment,
            P::Assignment => P::Or,
            P::Or => P::And,
            P::And => P::Equality,
            P::Equality => P::Comparison,
            P::Comparison => P::Term,
            P::Term => P::Factor,
            P::Factor => P::Unary,
            P::Unary => P::Call,
            P::Call => P::Primary,
            P::Primary => P::Unused,
            P::Unused => P::Unused,
        }
    }
}

impl Parser<'_> {
    pub(crate) fn argument_list(&mut self) -> u8 {
        let mut argc: u32 = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if argc == u8::MAX.into() {
                    self.error("Can't have more than 255 arguments.");
                }

                argc += 1;

                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        argc as u8
    }

    pub(crate) fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let slot: u8;
        let op_get: Opcode;
        let op_set: Opcode;

        let compiler = unsafe { self.compiler.as_mut().unwrap() };

        if let Some(arg) = self.resolve_local(compiler, name) {
            slot = arg as u8;
            op_get = Opcode::GetLocal;
            op_set = Opcode::SetLocal;
        } else if let Some(arg) = self.resolve_upvalue(compiler, name) {
            slot = arg as u8;
            op_get = Opcode::GetUpvalue;
            op_set = Opcode::SetUpvalue;
        } else {
            slot = self.identifier_constant(name);
            op_get = Opcode::GetGlobal;
            op_set = Opcode::SetGlobal;
        }

        if can_assign && self.match_(TokenType::Equal) {
            self.expression();
            self.emit_bytes(op_set, slot)
        } else {
            self.emit_bytes(op_get, slot)
        }
    }
}

pub(crate) fn and_(parser: &mut Parser, _can_assign: bool) {
    // [ a ]

    // jump_false_peek end
    let end_jump = parser.emit_jump(Opcode::JumpIfFalse);
    // pop a
    parser.emit_byte(Opcode::Pop);
    // <b>
    parser.parse_precedence(Precedence::And);
    // end:
    parser.patch_jump(end_jump);

    // [ a and b ]
}

pub(crate) fn or_(parser: &mut Parser, _can_assign: bool) {
    // TODO: Implement symmetrically with and_.

    // [ a ]

    // jump_false_peek else
    let else_jump = parser.emit_jump(Opcode::JumpIfFalse);
    // jump end
    let end_jump = parser.emit_jump(Opcode::Jump);
    // else:
    parser.patch_jump(else_jump);
    // pop a
    parser.emit_byte(Opcode::Pop);
    // <b>
    parser.parse_precedence(Precedence::Or);
    // end:
    parser.patch_jump(end_jump);

    // [ a or b ]
}

pub(crate) fn binary(parser: &mut Parser, _can_assign: bool) {
    let op = parser.previous.r#type;
    let rule = RULES.get(op);
    parser.parse_precedence(rule.precedence.plus_one());

    match op {
        TokenType::Plus => parser.emit_byte(Opcode::Add),
        TokenType::Minus => parser.emit_byte(Opcode::Sub),
        TokenType::Star => parser.emit_byte(Opcode::Mul),
        TokenType::Slash => parser.emit_byte(Opcode::Div),

        TokenType::BangEqual => parser.emit_bytes(Opcode::Equal, Opcode::Not),
        TokenType::EqualEqual => parser.emit_byte(Opcode::Equal),
        TokenType::Greater => parser.emit_byte(Opcode::Greater),
        TokenType::GreaterEqual => parser.emit_bytes(Opcode::Less, Opcode::Not),
        TokenType::Less => parser.emit_byte(Opcode::Less),
        TokenType::LessEqual => parser.emit_bytes(Opcode::Greater, Opcode::Not),

        _ => unreachable!(),
    }
}

pub(crate) fn call(parser: &mut Parser, _can_assign: bool) {
    let argc = parser.argument_list();
    parser.emit_bytes(Opcode::Call, argc);
}

pub(crate) fn dot(parser: &mut Parser, can_assign: bool) {
    parser.consume(TokenType::Identifier, "Expect property name after '.'.");
    let name = parser.identifier_constant(&parser.previous);

    if can_assign && parser.match_(TokenType::Equal) {
        parser.expression();
        parser.emit_bytes(Opcode::SetProperty, name);
    } else if parser.match_(TokenType::LeftParen) {
        let argc = parser.argument_list();
        parser.emit_bytes(Opcode::Invoke, name);
        parser.emit_byte(argc);
    } else {
        parser.emit_bytes(Opcode::GetProperty, name);
    }
}

pub(crate) fn literal(parser: &mut Parser, _can_assign: bool) {
    match parser.previous.r#type {
        TokenType::False => parser.emit_byte(Opcode::False),
        TokenType::True => parser.emit_byte(Opcode::True),
        TokenType::Nil => parser.emit_byte(Opcode::Nil),
        _ => unreachable!(),
    }
}

pub(crate) fn group(parser: &mut Parser, _can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::RightParen, "Expect ')' after expression.");
}

pub(crate) fn number(parser: &mut Parser, _can_assign: bool) {
    let f: f64 = parser.previous.text().parse().unwrap();
    parser.emit_constant(Value::number(f));
}

pub(crate) fn string(parser: &mut Parser, _can_assign: bool) {
    let gc = Gc::new(parser.compiler, parser.vm);

    let content = parser
        .previous
        .text
        .strip_prefix('"')
        .expect("string literal starts with double-quote")
        .strip_suffix('"')
        .expect("string literal ends with double-quote");

    // TODO: This is where handling escape sequences would go.

    let s = ObjString::from_str(gc, content);
    parser.emit_constant(Value::obj(s as *mut Obj));
}

pub(crate) fn variable(parser: &mut Parser, can_assign: bool) {
    let previous = parser.previous;
    parser.named_variable(&previous, can_assign)
}

pub(crate) fn super_(parser: &mut Parser, _can_assign: bool) {
    let klass = unsafe { parser.klass.as_mut() };
    if klass.is_none() {
        parser.error("Can't use 'super' outside of a class.");
    } else if !klass.unwrap().has_superclass {
        parser.error("Can't use 'super' in a class with no superclass.");
    }

    parser.consume(TokenType::Dot, "Expect '.' after 'super'.");
    parser.consume(TokenType::Identifier, "Expect superclass method name.");
    let name = parser.identifier_constant(&parser.previous);

    parser.named_variable(&Token::synthetic("this"), false);

    if parser.match_(TokenType::LeftParen) {
        let argc = parser.argument_list();
        parser.named_variable(&Token::synthetic("super"), false);
        parser.emit_bytes(Opcode::SuperInvoke, name);
        parser.emit_byte(argc);
    } else {
        parser.named_variable(&Token::synthetic("super"), false);
        parser.emit_bytes(Opcode::GetSuper, name);
    }
}

pub(crate) fn this_(parser: &mut Parser, _can_assign: bool) {
    if parser.klass.is_null() {
        parser.error("Can't use 'this' outside of a class.");
        return;
    };

    variable(parser, false);
}

pub(crate) fn unary(parser: &mut Parser, _can_assign: bool) {
    let op = parser.previous.r#type;

    parser.parse_precedence(Precedence::Unary);

    match op {
        TokenType::Bang => parser.emit_byte(Opcode::Not),
        TokenType::Minus => parser.emit_byte(Opcode::Neg),
        _ => unreachable!(),
    }
}

impl Parser<'_> {
    pub(crate) fn declaration(&mut self) {
        if self.match_(TokenType::Class) {
            self.decl_class();
        } else if self.match_(TokenType::Fun) {
            self.decl_fun();
        } else if self.match_(TokenType::Var) {
            self.decl_var();
        } else {
            self.statement();
        }

        if self.panicking {
            self.synchronize();
        }
    }

    pub(crate) fn statement(&mut self) {
        if self.match_(TokenType::If) {
            self.stmt_if();
        } else if self.match_(TokenType::For) {
            self.stmt_for();
        } else if self.match_(TokenType::Print) {
            self.stmt_print();
        } else if self.match_(TokenType::Return) {
            self.stmt_return();
        } else if self.match_(TokenType::While) {
            self.stmt_while();
        } else if self.match_(TokenType::LeftBrace) {
            self.scope_begin();
            self.block();
            self.scope_end();
        } else {
            self.stmt_expr();
        }
    }

    pub(crate) fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment)
    }

    pub(crate) fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        let Some(prefix) = RULES.get(self.previous.r#type).prefix else {
            self.error("Expect expression.");
            return;
        };

        let can_assign = prec <= Precedence::Assignment;
        prefix(self, can_assign);

        while prec <= RULES.get(self.current.r#type).precedence {
            self.advance();

            let infix = RULES.get(self.previous.r#type).infix.unwrap();
            infix(self, can_assign);
        }

        if can_assign && self.match_(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    pub(crate) fn decl_class(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.previous;

        let name_constant = self.identifier_constant(&class_name);
        self.declare_variable();

        self.emit_bytes(Opcode::Class, name_constant);
        self.define_variable(name_constant);

        let klass = ClassCompiler {
            enclosing: self.klass,
            has_superclass: false,
        };
        self.klass = Box::into_raw(Box::new(klass));

        if self.match_(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            variable(self, false);

            if identifiers_equal(&class_name, &self.previous) {
                self.error("A class can't inherit from itself.");
            }

            self.scope_begin();
            self.add_local(Token::synthetic("super"));
            self.define_variable(0);

            self.named_variable(&class_name, false);
            self.emit_byte(Opcode::Inherit);

            unsafe { self.klass.as_mut().unwrap() }.has_superclass = true;
        }

        self.named_variable(&class_name, false);

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.emit_byte(Opcode::Pop); // class

        let klass = unsafe { Box::from_raw(self.klass) };
        if klass.has_superclass {
            self.scope_end();
        }
        self.klass = klass.enclosing;
    }

    pub(crate) fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let constant = self.identifier_constant(&self.previous);

        let mode = if self.previous.text() == "init" {
            FunctionMode::Initializer
        } else {
            FunctionMode::Method
        };

        self.function(mode);

        self.emit_bytes(Opcode::Method, constant);
    }

    pub(crate) fn decl_fun(&mut self) {
        let global = self.parse_variable("Expect function name.");

        // Allow the function's name to be used within its body to support recursion.
        (unsafe { self.compiler.as_mut().unwrap() }).mark_initialized();

        self.function(FunctionMode::Function);

        self.define_variable(global);
    }

    pub(crate) fn decl_var(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(Opcode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    pub(crate) fn stmt_expr(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(Opcode::Pop);
    }

    pub(crate) fn stmt_if(&mut self) {
        // <cond>
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // jump_false_peek else
        let then_jump = self.emit_jump(Opcode::JumpIfFalse);
        // pop cond
        self.emit_byte(Opcode::Pop);
        // <conseq>
        self.statement();

        // TODO: Avoid the double-jump if no else branch.

        // jump out
        let else_jump = self.emit_jump(Opcode::Jump);

        // else:
        self.patch_jump(then_jump);
        // pop cond
        self.emit_byte(Opcode::Pop);

        // <alt>
        if self.match_(TokenType::Else) {
            self.statement();
        }
        // out:
        self.patch_jump(else_jump);
    }

    pub(crate) fn stmt_for(&mut self) {
        self.scope_begin();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");

        // <init>
        if self.match_(TokenType::Semicolon) {
            // No init
        } else if self.match_(TokenType::Var) {
            self.decl_var();
        } else {
            self.stmt_expr();
        }

        // start:
        let mut loop_start = self.current_chunk().code.len();

        let mut has_exit = false;
        let mut exit_jump = 0;

        // <cond>
        if !self.match_(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after for loop condition.");

            // jump_false_peek exit
            exit_jump = self.emit_jump(Opcode::JumpIfFalse);
            has_exit = true;

            // pop cond
            self.emit_byte(Opcode::Pop);
        }

        if !self.match_(TokenType::RightParen) {
            // jump body
            let body_jump = self.emit_jump(Opcode::Jump);

            // incr:
            let incr_start = self.current_chunk().code.len();
            // <next>
            self.expression();
            // pop next
            self.emit_byte(Opcode::Pop);

            self.consume(TokenType::RightParen, "Expect ')' after for loop clauses.");

            // jump start
            self.emit_loop(loop_start);
            loop_start = incr_start;

            // body:
            self.patch_jump(body_jump);
        }

        // <body>
        self.statement();
        // jump start
        self.emit_loop(loop_start);
        // exit:
        if has_exit {
            self.patch_jump(exit_jump);
            // pop cond
            self.emit_byte(Opcode::Pop);
        }

        self.scope_end();
    }

    pub(crate) fn stmt_print(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::Print);
    }

    pub(crate) fn stmt_return(&mut self) {
        let mode = unsafe { self.compiler.as_ref().unwrap() }.mode;
        if mode == FunctionMode::Script {
            self.error("Can't return from top-level code.");
        }

        if self.match_(TokenType::Semicolon) {
            self.emit_return();
        } else {
            if mode == FunctionMode::Initializer {
                self.error("Can't return a value from an initializer.");
                // Continue compiling the expression anyway to let the parser re-sync.
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit_byte(Opcode::Return);
        }
    }

    pub(crate) fn stmt_while(&mut self) {
        // start:
        let loop_start = self.current_chunk().code.len();
        // <cond>
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        // jump_false_peek exit
        let exit_jump = self.emit_jump(Opcode::JumpIfFalse);
        // pop cond
        self.emit_byte(Opcode::Pop);
        // <body>
        self.statement();
        // jump start
        self.emit_loop(loop_start);
        // exit:
        self.patch_jump(exit_jump);
        // pop cond
        self.emit_byte(Opcode::Pop);
    }

    pub(crate) fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    pub(crate) fn function(&mut self, mode: FunctionMode) {
        self.start_compiler(mode);

        let gc = Gc::new(self.compiler, self.vm);

        let compiler = unsafe { self.compiler.as_mut().unwrap() };

        let function = unsafe { compiler.function.as_mut().unwrap() };
        if mode != FunctionMode::Script {
            function.name = ObjString::from_str(gc, self.previous.text);
        }

        self.scope_begin();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                function.arity += 1;

                if function.arity > u8::MAX.into() {
                    self.error_at_current("Can't have more than 255 parameters.");
                }

                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);

                if !self.match_(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        // The self.scope_end() isn't necessary because all locals will get implicitly
        // popped when the VM pops the CallFrame.
        let function = self.end_compiler();
        let constant = self.make_constant(Value::obj(function as *mut Obj));
        self.emit_bytes(Opcode::Closure, constant);

        let function = unsafe { function.as_ref().unwrap() };
        for i in 0..function.upvalue_count {
            let upvalue = &compiler.upvalues[i];
            self.emit_byte(if upvalue.is_local { 1 } else { 0 });
            self.emit_byte(upvalue.index);
        }
    }
}

impl Rules {
    pub(crate) fn get(&self, ty: TokenType) -> &ParseRule {
        self.0.get(&ty).expect("parse rule")
    }
}

#[rustfmt::skip]
pub(crate) fn make_rules() -> Rules {
    let mut rules = HashMap::new();

    macro_rules! rule {
        ($token:expr, $prefix:expr, $infix:expr, $precedence:expr) => {
            rules.insert($token, ParseRule::new($prefix, $infix, $precedence))
        }
    }

    {
        rule!(TokenType::LeftParen,    Some(group),    Some(call),   Precedence::Call      );
        rule!(TokenType::RightParen,   None,           None,         Precedence::None      );
        rule!(TokenType::LeftBrace,    None,           None,         Precedence::None      );
        rule!(TokenType::RightBrace,   None,           None,         Precedence::None      );
        rule!(TokenType::Comma,        None,           None,         Precedence::None      );
        rule!(TokenType::Dot,          None,           Some(dot),    Precedence::Call      );
        rule!(TokenType::Minus,        Some(unary),    Some(binary), Precedence::Term      );
        rule!(TokenType::Plus,         None,           Some(binary), Precedence::Term      );
        rule!(TokenType::Semicolon,    None,           None,         Precedence::None      );
        rule!(TokenType::Slash,        None,           Some(binary), Precedence::Factor    );
        rule!(TokenType::Star,         None,           Some(binary), Precedence::Factor    );
        rule!(TokenType::Bang,         Some(unary),    None,         Precedence::None      );
        rule!(TokenType::BangEqual,    None,           Some(binary), Precedence::Equality  );
        rule!(TokenType::Equal,        None,           None,         Precedence::None      );
        rule!(TokenType::EqualEqual,   None,           Some(binary), Precedence::Equality  );
        rule!(TokenType::Greater,      None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::GreaterEqual, None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::Less,         None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::LessEqual,    None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::Identifier,   Some(variable), None,         Precedence::None      );
        rule!(TokenType::String,       Some(string),   None,         Precedence::None      );
        rule!(TokenType::Number,       Some(number),   None,         Precedence::None      );
        rule!(TokenType::And,          None,           Some(and_),   Precedence::And       );
        rule!(TokenType::Class,        None,           None,         Precedence::None      );
        rule!(TokenType::Else,         None,           None,         Precedence::None      );
        rule!(TokenType::False,        Some(literal),  None,         Precedence::None      );
        rule!(TokenType::For,          None,           None,         Precedence::None      );
        rule!(TokenType::Fun,          None,           None,         Precedence::None      );
        rule!(TokenType::If,           None,           None,         Precedence::None      );
        rule!(TokenType::Nil,          Some(literal),  None,         Precedence::None      );
        rule!(TokenType::Or,           None,           Some(or_),    Precedence::Or        );
        rule!(TokenType::Print,        None,           None,         Precedence::None      );
        rule!(TokenType::Return,       None,           None,         Precedence::None      );
        rule!(TokenType::Super,        Some(super_),   None,         Precedence::None      );
        rule!(TokenType::This,         Some(this_),    None,         Precedence::None      );
        rule!(TokenType::True,         Some(literal),  None,         Precedence::None      );
        rule!(TokenType::Var,          None,           None,         Precedence::None      );
        rule!(TokenType::While,        None,           None,         Precedence::None      );
        rule!(TokenType::Error,        None,           None,         Precedence::None      );
        rule!(TokenType::Eof,          None,           None,         Precedence::None      );
    }

    Rules(rules)
}

pub(crate) fn identifiers_equal(a: &Token, b: &Token) -> bool {
    a.text == b.text
}
