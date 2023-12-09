use std::collections::HashMap;
use std::ffi::{c_char, c_int, c_uint, CString};
use std::mem::MaybeUninit;
use std::ptr;

use once_cell::sync::Lazy;

use crate::chunk::Chunk;
use crate::object::{Obj, ObjFunction, ObjString};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::{Gc, Opcode, Vm, U8_COUNT};

static THIS_STR: Lazy<CString> = Lazy::new(|| CString::new("this").unwrap());
static SUPER_STR: Lazy<CString> = Lazy::new(|| CString::new("super").unwrap());

#[repr(C)]
pub struct Parser {
    current: Token,
    previous: Token,

    scanner: *mut Scanner,
    compiler: *mut Compiler,
    klass: *mut ClassCompiler,

    had_error: bool,
    panicking: bool,

    vm: *mut Vm,
}

#[repr(C)]
pub struct Compiler {
    pub(crate) enclosing: *mut Compiler,
    pub(crate) function: *mut ObjFunction,
    mode: FunctionMode,

    // TODO: This is a Vec
    locals: [Local; U8_COUNT],
    local_count: c_int,

    scope_depth: c_int,

    upvalues: [Upvalue; U8_COUNT],
}

/// cbindgen:rename-all=ScreamingSnakeCase
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C)]
pub enum FunctionMode {
    ModeFunction,
    ModeInitializer,
    ModeMethod,
    ModeScript,
}

#[repr(C)]
pub struct ClassCompiler {
    enclosing: *mut ClassCompiler,
    has_superclass: bool,
}

#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct Local {
    name: Token,
    depth: c_int,
    is_captured: bool,
}

#[derive(Debug, Copy, Clone)]
#[repr(C)]
pub struct Upvalue {
    index: u8,
    is_local: bool,
}

pub(crate) fn compile(vm: *mut Vm, source: *const c_char) -> *mut ObjFunction {
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(&mut scanner, vm);

    let mut c = MaybeUninit::uninit();
    parser.compiler_init(c.as_mut_ptr(), FunctionMode::ModeScript);

    while !parser.match_(TokenType::TokenEof) {
        parser.declaration();
    }

    let fun = parser.end_compiler();
    if parser.had_error {
        ptr::null_mut()
    } else {
        fun
    }
}

impl Parser {
    pub(crate) fn init(&mut self, scanner: *mut Scanner, vm: *mut Vm) {
        self.had_error = false;
        self.panicking = false;

        self.scanner = scanner;
        self.compiler = ptr::null_mut();
        self.klass = ptr::null_mut();
        self.vm = vm;
    }

    pub(crate) fn compiler_init(&mut self, compiler: *mut Compiler, mode: FunctionMode) {
        let gc = Gc::new(self.compiler, self.vm);

        let compiler_ref = unsafe { compiler.as_mut().unwrap() };

        compiler_ref.enclosing = self.compiler;
        compiler_ref.function = ptr::null_mut();
        compiler_ref.mode = mode;
        compiler_ref.local_count = 0;
        compiler_ref.scope_depth = 0;
        compiler_ref.function = ObjFunction::new(gc);

        self.compiler = compiler;

        // Reserve stack slot zero for the currently executing function.
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        let local = compiler
            .locals
            .get_mut(compiler.local_count as usize)
            .unwrap();
        compiler.local_count += 1;

        local.depth = 0;
        local.is_captured = false;
        if mode != FunctionMode::ModeFunction {
            // local.name = Token::synthetic(&THIS_STR);
            local.name.start = THIS_STR.as_ptr();
            local.name.length = 4;
        } else {
            static EMPTY_STR: Lazy<CString> = Lazy::new(|| CString::new("").unwrap());

            local.name.start = EMPTY_STR.as_ptr();
            local.name.length = 0;
        }
    }

    pub(crate) fn new(scanner: &mut Scanner, vm: *mut Vm) -> Self {
        let mut parser = Self {
            // Will be overwritten by the immediate call to advance.
            current: Token::zero(),
            previous: Token::zero(),

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

    pub(crate) fn end_compiler(&mut self) -> *mut ObjFunction {
        self.emit_return();
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
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
            TokenType::TokenEof => Some(String::from("end")),
            TokenType::TokenError => None, // No location info for synthetic token
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

            if self.current.r#type != TokenType::TokenError {
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

        while self.current.r#type != TokenType::TokenEof {
            if self.previous.r#type == TokenType::TokenSemicolon {
                return;
            }

            match self.current.r#type {
                TokenType::TokenClass
                | TokenType::TokenFun
                | TokenType::TokenVar
                | TokenType::TokenFor
                | TokenType::TokenIf
                | TokenType::TokenWhile
                | TokenType::TokenPrint
                | TokenType::TokenReturn => return,

                _ => self.advance(),
            }
        }
    }
}

impl Parser {
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

    pub(crate) fn emit_loop(&mut self, start: c_uint) {
        self.emit_byte(Opcode::OpLoop);

        let offset = self.current_chunk().code.len() - start + 2;
        if offset > u16::MAX.into() {
            self.error("Loop body too large.");
        }

        self.emit_byte(((offset >> 8) & 0xff) as u8);
        self.emit_byte((offset & 0xff) as u8);
    }

    pub(crate) fn emit_jump(&mut self, op: Opcode) -> c_uint {
        self.emit_byte(op);
        self.emit_bytes(0xff, 0xff);
        self.current_chunk().code.len() - 2
    }

    pub(crate) fn patch_jump(&mut self, offset: c_uint) {
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
        if unsafe { self.compiler.as_mut().unwrap() }.mode == FunctionMode::ModeInitializer {
            self.emit_bytes(Opcode::OpGetLocal, 0);
        } else {
            self.emit_byte(Opcode::OpNil);
        }
        self.emit_byte(Opcode::OpReturn);
    }

    pub(crate) fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(Opcode::OpConstant, constant);
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

impl Parser {
    pub(crate) fn scope_begin(&mut self) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        compiler.scope_depth += 1;
    }

    pub(crate) fn scope_end(&mut self) {
        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        compiler.scope_depth -= 1;

        // TODO: OP_POP_N/OP_CLOSE_UPVALUE_N is a good optimization here.
        while compiler.local_count > 0
            && compiler.locals[compiler.local_count as usize - 1].depth > compiler.scope_depth
        {
            if compiler.locals[compiler.local_count as usize - 1].is_captured {
                self.emit_byte(Opcode::OpCloseUpvalue);
            } else {
                self.emit_byte(Opcode::OpPop);
            }
            compiler.local_count -= 1;
        }
    }

    pub(crate) fn add_local(&mut self, name: Token) {
        if unsafe { self.compiler.as_mut().unwrap() }.local_count as usize == U8_COUNT {
            self.error("Too many local variables in function.");
            return;
        }

        let compiler = unsafe { self.compiler.as_mut().unwrap() };
        let local = compiler
            .locals
            .get_mut(compiler.local_count as usize)
            .unwrap();
        compiler.local_count += 1;

        local.name = name;
        local.depth = -1; // declared: reserved without value
        local.is_captured = false;
    }
}

impl Compiler {
    pub(crate) fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            // Global variables don't have their initialization tracked like this.
            return;
        }

        // TODO: This is a Vec
        let local_count = (self.local_count - 1) as usize;
        self.locals.get_mut(local_count).unwrap().depth = self.scope_depth;
        // defined: value available at a certain depth
    }
}

impl Parser {
    pub(crate) fn identifier_constant(&mut self, name: *const Token) -> u8 {
        let gc = Gc::new(self.compiler, self.vm);

        let name = unsafe { name.as_ref().unwrap() };

        let s = ObjString::from_borrowed(gc, name.start, name.length as usize);
        self.make_constant(Value::obj(s as *mut Obj))
    }

    pub(crate) fn resolve_local(&mut self, compiler: &Compiler, name: &Token) -> Option<c_int> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.

        for i in (0..compiler.local_count).rev() {
            let local = compiler.locals.get(i as usize).unwrap();
            if identifiers_equal(name, &local.name) {
                if local.depth == -1 {
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
    ) -> Option<c_int> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.
        let function = unsafe { compiler.function.as_mut().unwrap() };
        let upvalue_count = function.upvalue_count;

        for i in 0..upvalue_count {
            let upvalue = compiler.upvalues.get(i as usize).unwrap();
            if upvalue.index == index && upvalue.is_local == is_local {
                return Some(i);
            }
        }

        if function.upvalue_count as usize == U8_COUNT {
            self.error("Too many closure variables in function.");
            return None;
        }

        // TODO: This is a Vec
        let upvalue = compiler.upvalues.get_mut(upvalue_count as usize).unwrap();
        upvalue.is_local = is_local;
        upvalue.index = index;

        function.upvalue_count += 1;
        Some(function.upvalue_count - 1) // The _previous_ count is the new item's index
    }

    pub(crate) fn resolve_upvalue(
        &mut self,
        compiler: &mut Compiler,
        name: &Token,
    ) -> Option<c_int> {
        // Parser is only used to report errors.
        // Compiler is used to resolve locals.

        let Some(enclosing) = (unsafe { compiler.enclosing.as_mut() }) else {
            return None;
        };

        if let Some(local) = self.resolve_local(enclosing, name) {
            enclosing
                .locals
                .get_mut(local as usize)
                .unwrap()
                .is_captured = true;
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
        let local_count = compiler.local_count;
        for i in (0..local_count).rev() {
            let local = compiler.locals[i as usize];
            if local.depth != -1 && local.depth < compiler.scope_depth {
                break;
            }

            if identifiers_equal(&name, &local.name) {
                self.error("Already a variable with this name in this scope.");
            }
        }
        self.add_local(name);
    }

    pub(crate) fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(TokenType::TokenIdentifier, msg);

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
        self.emit_bytes(Opcode::OpDefineGlobal, global);
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

impl Parser {
    pub(crate) fn argument_list(&mut self) -> u8 {
        let mut argc: u32 = 0;
        if !self.check(TokenType::TokenRightParen) {
            loop {
                self.expression();
                if argc == u8::MAX.into() {
                    self.error("Can't have more than 255 arguments.");
                }

                argc += 1;

                if !self.match_(TokenType::TokenComma) {
                    break;
                }
            }
        }

        self.consume(TokenType::TokenRightParen, "Expect ')' after arguments.");
        argc as u8
    }

    pub(crate) fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let slot: u8;
        let op_get: Opcode;
        let op_set: Opcode;

        let compiler = unsafe { self.compiler.as_mut().unwrap() };

        if let Some(arg) = self.resolve_local(compiler, name) {
            slot = arg as u8;
            op_get = Opcode::OpGetLocal;
            op_set = Opcode::OpSetLocal;
        } else if let Some(arg) = self.resolve_upvalue(compiler, name) {
            slot = arg as u8;
            op_get = Opcode::OpGetUpvalue;
            op_set = Opcode::OpSetUpvalue;
        } else {
            slot = self.identifier_constant(name);
            op_get = Opcode::OpGetGlobal;
            op_set = Opcode::OpSetGlobal;
        }

        if can_assign && self.match_(TokenType::TokenEqual) {
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
    let end_jump = parser.emit_jump(Opcode::OpJumpIfFalse);
    // pop a
    parser.emit_byte(Opcode::OpPop);
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
    let else_jump = parser.emit_jump(Opcode::OpJumpIfFalse);
    // jump end
    let end_jump = parser.emit_jump(Opcode::OpJump);
    // else:
    parser.patch_jump(else_jump);
    // pop a
    parser.emit_byte(Opcode::OpPop);
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
        TokenType::TokenPlus => parser.emit_byte(Opcode::OpAdd),
        TokenType::TokenMinus => parser.emit_byte(Opcode::OpSub),
        TokenType::TokenStar => parser.emit_byte(Opcode::OpMul),
        TokenType::TokenSlash => parser.emit_byte(Opcode::OpDiv),

        TokenType::TokenBangEqual => parser.emit_bytes(Opcode::OpEqual, Opcode::OpNot),
        TokenType::TokenEqualEqual => parser.emit_byte(Opcode::OpEqual),
        TokenType::TokenGreater => parser.emit_byte(Opcode::OpGreater),
        TokenType::TokenGreaterEqual => parser.emit_bytes(Opcode::OpLess, Opcode::OpNot),
        TokenType::TokenLess => parser.emit_byte(Opcode::OpLess),
        TokenType::TokenLessEqual => parser.emit_bytes(Opcode::OpGreater, Opcode::OpNot),

        _ => unreachable!(),
    }
}

pub(crate) fn call(parser: &mut Parser, _can_assign: bool) {
    let argc = parser.argument_list();
    parser.emit_bytes(Opcode::OpCall, argc);
}

pub(crate) fn dot(parser: &mut Parser, can_assign: bool) {
    parser.consume(
        TokenType::TokenIdentifier,
        "Expect property name after '.'.",
    );
    let name = parser.identifier_constant(&parser.previous);

    if can_assign && parser.match_(TokenType::TokenEqual) {
        parser.expression();
        parser.emit_bytes(Opcode::OpSetProperty, name);
    } else if parser.match_(TokenType::TokenLeftParen) {
        let argc = parser.argument_list();
        parser.emit_bytes(Opcode::OpInvoke, name);
        parser.emit_byte(argc);
    } else {
        parser.emit_bytes(Opcode::OpGetProperty, name);
    }
}

pub(crate) fn literal(parser: &mut Parser, _can_assign: bool) {
    match parser.previous.r#type {
        TokenType::TokenFalse => parser.emit_byte(Opcode::OpFalse),
        TokenType::TokenTrue => parser.emit_byte(Opcode::OpTrue),
        TokenType::TokenNil => parser.emit_byte(Opcode::OpNil),
        _ => unreachable!(),
    }
}

pub(crate) fn group(parser: &mut Parser, _can_assign: bool) {
    parser.expression();
    parser.consume(TokenType::TokenRightParen, "Expect ')' after expression.");
}

pub(crate) fn number(parser: &mut Parser, _can_assign: bool) {
    let f: f64 = parser.previous.text().parse().unwrap();
    parser.emit_constant(Value::number(f));
}

pub(crate) fn string(parser: &mut Parser, _can_assign: bool) {
    let gc = Gc::new(parser.compiler, parser.vm);

    // Drop the leading quote '"'
    let start = unsafe { parser.previous.start.add(1) };
    // Drop the trailing quote '"' and one more because pointer math.
    let length = parser.previous.length - 2;

    let s = ObjString::from_borrowed(gc, start, length as usize);
    // TODO: This is where handling escape sequence would go.
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

    parser.consume(TokenType::TokenDot, "Expect '.' after 'super'.");
    parser.consume(TokenType::TokenIdentifier, "Expect superclass method name.");
    let name = parser.identifier_constant(&parser.previous);

    parser.named_variable(&Token::synthetic(&THIS_STR), false);

    if parser.match_(TokenType::TokenLeftParen) {
        let argc = parser.argument_list();
        parser.named_variable(&Token::synthetic(&SUPER_STR), false);
        parser.emit_bytes(Opcode::OpSuperInvoke, name);
        parser.emit_byte(argc);
    } else {
        parser.named_variable(&Token::synthetic(&SUPER_STR), false);
        parser.emit_bytes(Opcode::OpGetSuper, name);
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
        TokenType::TokenBang => parser.emit_byte(Opcode::OpNot),
        TokenType::TokenMinus => parser.emit_byte(Opcode::OpNeg),
        _ => unreachable!(),
    }
}

impl Parser {
    pub(crate) fn declaration(&mut self) {
        if self.match_(TokenType::TokenClass) {
            self.decl_class();
        } else if self.match_(TokenType::TokenFun) {
            self.decl_fun();
        } else if self.match_(TokenType::TokenVar) {
            self.decl_var();
        } else {
            self.statement();
        }

        if self.panicking {
            self.synchronize();
        }
    }

    pub(crate) fn statement(&mut self) {
        if self.match_(TokenType::TokenIf) {
            self.stmt_if();
        } else if self.match_(TokenType::TokenFor) {
            self.stmt_for();
        } else if self.match_(TokenType::TokenPrint) {
            self.stmt_print();
        } else if self.match_(TokenType::TokenReturn) {
            self.stmt_return();
        } else if self.match_(TokenType::TokenWhile) {
            self.stmt_while();
        } else if self.match_(TokenType::TokenLeftBrace) {
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

        if can_assign && self.match_(TokenType::TokenEqual) {
            self.error("Invalid assignment target.");
        }
    }

    pub(crate) fn decl_class(&mut self) {
        self.consume(TokenType::TokenIdentifier, "Expect class name.");
        let class_name = self.previous;

        let name_constant = self.identifier_constant(&class_name);
        self.declare_variable();

        self.emit_bytes(Opcode::OpClass, name_constant);
        self.define_variable(name_constant);

        let klass = ClassCompiler {
            enclosing: self.klass,
            has_superclass: false,
        };
        self.klass = Box::into_raw(Box::new(klass));

        if self.match_(TokenType::TokenLess) {
            self.consume(TokenType::TokenIdentifier, "Expect superclass name.");
            variable(self, false);

            if identifiers_equal(&class_name, &self.previous) {
                self.error("A class can't inherit from itself.");
            }

            self.scope_begin();
            self.add_local(Token::synthetic(&SUPER_STR));
            self.define_variable(0);

            self.named_variable(&class_name, false);
            self.emit_byte(Opcode::OpInherit);

            unsafe { self.klass.as_mut().unwrap() }.has_superclass = true;
        }

        self.named_variable(&class_name, false);

        self.consume(TokenType::TokenLeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::TokenRightBrace) && !self.check(TokenType::TokenEof) {
            self.method();
        }
        self.consume(TokenType::TokenRightBrace, "Expect '}' after class body.");
        self.emit_byte(Opcode::OpPop); // class

        let klass = unsafe { Box::from_raw(self.klass) };
        if klass.has_superclass {
            self.scope_end();
        }
        self.klass = klass.enclosing;
    }

    pub(crate) fn method(&mut self) {
        self.consume(TokenType::TokenIdentifier, "Expect method name.");
        let constant = self.identifier_constant(&self.previous);

        let mode = if self.previous.text() == "init" {
            FunctionMode::ModeInitializer
        } else {
            FunctionMode::ModeMethod
        };

        self.function(mode);

        self.emit_bytes(Opcode::OpMethod, constant);
    }

    pub(crate) fn decl_fun(&mut self) {
        let global = self.parse_variable("Expect function name.");

        // Allow the function's name to be used within its body to support recursion.
        (unsafe { self.compiler.as_mut().unwrap() }).mark_initialized();

        self.function(FunctionMode::ModeFunction);

        self.define_variable(global);
    }

    pub(crate) fn decl_var(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_(TokenType::TokenEqual) {
            self.expression();
        } else {
            self.emit_byte(Opcode::OpNil);
        }
        self.consume(
            TokenType::TokenSemicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    pub(crate) fn stmt_expr(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expect ';' after expression.");
        self.emit_byte(Opcode::OpPop);
    }

    pub(crate) fn stmt_if(&mut self) {
        // <cond>
        self.consume(TokenType::TokenLeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expect ')' after condition.");

        // jump_false_peek else
        let then_jump = self.emit_jump(Opcode::OpJumpIfFalse);
        // pop cond
        self.emit_byte(Opcode::OpPop);
        // <conseq>
        self.statement();

        // TODO: Avoid the double-jump if no else branch.

        // jump out
        let else_jump = self.emit_jump(Opcode::OpJump);

        // else:
        self.patch_jump(then_jump);
        // pop cond
        self.emit_byte(Opcode::OpPop);

        // <alt>
        if self.match_(TokenType::TokenElse) {
            self.statement();
        }
        // out:
        self.patch_jump(else_jump);
    }

    pub(crate) fn stmt_for(&mut self) {
        self.scope_begin();

        self.consume(TokenType::TokenLeftParen, "Expect '(' after 'for'.");

        // <init>
        if self.match_(TokenType::TokenSemicolon) {
            // No init
        } else if self.match_(TokenType::TokenVar) {
            self.decl_var();
        } else {
            self.stmt_expr();
        }

        // start:
        let mut loop_start = self.current_chunk().code.len();

        let mut has_exit = false;
        let mut exit_jump = 0;

        // <cond>
        if !self.match_(TokenType::TokenSemicolon) {
            self.expression();
            self.consume(
                TokenType::TokenSemicolon,
                "Expect ';' after for loop condition.",
            );

            // jump_false_peek exit
            exit_jump = self.emit_jump(Opcode::OpJumpIfFalse);
            has_exit = true;

            // pop cond
            self.emit_byte(Opcode::OpPop);
        }

        if !self.match_(TokenType::TokenRightParen) {
            // jump body
            let body_jump = self.emit_jump(Opcode::OpJump);

            // incr:
            let incr_start = self.current_chunk().code.len();
            // <next>
            self.expression();
            // pop next
            self.emit_byte(Opcode::OpPop);

            self.consume(
                TokenType::TokenRightParen,
                "Expect ')' after for loop clauses.",
            );

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
            self.emit_byte(Opcode::OpPop);
        }

        self.scope_end();
    }

    pub(crate) fn stmt_print(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expect ';' after value.");
        self.emit_byte(Opcode::OpPrint);
    }

    pub(crate) fn stmt_return(&mut self) {
        let mode = unsafe { self.compiler.as_ref().unwrap() }.mode;
        if mode == FunctionMode::ModeScript {
            self.error("Can't return from top-level code.");
        }

        if self.match_(TokenType::TokenSemicolon) {
            self.emit_return();
        } else {
            if mode == FunctionMode::ModeInitializer {
                self.error("Can't return a value from an initializer.");
                // Continue compiling the expression anyway to let the parser re-sync.
            }

            self.expression();
            self.consume(TokenType::TokenSemicolon, "Expect ';' after return value.");
            self.emit_byte(Opcode::OpReturn);
        }
    }

    pub(crate) fn stmt_while(&mut self) {
        // start:
        let loop_start = self.current_chunk().code.len();
        // <cond>
        self.consume(TokenType::TokenLeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expect ')' after condition.");

        // jump_false_peek exit
        let exit_jump = self.emit_jump(Opcode::OpJumpIfFalse);
        // pop cond
        self.emit_byte(Opcode::OpPop);
        // <body>
        self.statement();
        // jump start
        self.emit_loop(loop_start);
        // exit:
        self.patch_jump(exit_jump);
        // pop cond
        self.emit_byte(Opcode::OpPop);
    }

    pub(crate) fn block(&mut self) {
        while !self.check(TokenType::TokenRightBrace) && !self.check(TokenType::TokenEof) {
            self.declaration();
        }

        self.consume(TokenType::TokenRightBrace, "Expect '}' after block.");
    }

    pub(crate) fn function(&mut self, mode: FunctionMode) {
        let mut compiler = MaybeUninit::uninit();
        self.compiler_init(compiler.as_mut_ptr(), mode);

        let gc = Gc::new(self.compiler, self.vm);

        let compiler = unsafe { self.compiler.as_mut().unwrap() };

        let function = unsafe { compiler.function.as_mut().unwrap() };
        if mode != FunctionMode::ModeScript {
            function.name =
                ObjString::from_borrowed(gc, self.previous.start, self.previous.length as usize);
        }

        self.scope_begin();

        self.consume(TokenType::TokenLeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::TokenRightParen) {
            loop {
                function.arity += 1;

                if function.arity > u8::MAX.into() {
                    self.error_at_current("Can't have more than 255 parameters.");
                }

                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);

                if !self.match_(TokenType::TokenComma) {
                    break;
                }
            }
        }
        self.consume(TokenType::TokenRightParen, "Expect ')' after parameters.");
        self.consume(
            TokenType::TokenLeftBrace,
            "Expect '{' before function body.",
        );
        self.block();

        // The self.scope_end() isn't necessary because all locals will get implicitly
        // popped when the VM pops the CallFrame.
        let function = self.end_compiler();
        let constant = self.make_constant(Value::obj(function as *mut Obj));
        self.emit_bytes(Opcode::OpClosure, constant);

        let function = unsafe { function.as_ref().unwrap() };
        for i in 0..(function.upvalue_count as usize) {
            let upvalue = compiler.upvalues.get(i).unwrap();
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
        rule!(TokenType::TokenLeftParen,    Some(group),    Some(call),   Precedence::Call      );
        rule!(TokenType::TokenRightParen,   None,           None,         Precedence::None      );
        rule!(TokenType::TokenLeftBrace,    None,           None,         Precedence::None      );
        rule!(TokenType::TokenRightBrace,   None,           None,         Precedence::None      );
        rule!(TokenType::TokenComma,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenDot,          None,           Some(dot),    Precedence::Call      );
        rule!(TokenType::TokenMinus,        Some(unary),    Some(binary), Precedence::Term      );
        rule!(TokenType::TokenPlus,         None,           Some(binary), Precedence::Term      );
        rule!(TokenType::TokenSemicolon,    None,           None,         Precedence::None      );
        rule!(TokenType::TokenSlash,        None,           Some(binary), Precedence::Factor    );
        rule!(TokenType::TokenStar,         None,           Some(binary), Precedence::Factor    );
        rule!(TokenType::TokenBang,         Some(unary),    None,         Precedence::None      );
        rule!(TokenType::TokenBangEqual,    None,           Some(binary), Precedence::Equality  );
        rule!(TokenType::TokenEqual,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenEqualEqual,   None,           Some(binary), Precedence::Equality  );
        rule!(TokenType::TokenGreater,      None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::TokenGreaterEqual, None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::TokenLess,         None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::TokenLessEqual,    None,           Some(binary), Precedence::Comparison);
        rule!(TokenType::TokenIdentifier,   Some(variable), None,         Precedence::None      );
        rule!(TokenType::TokenString,       Some(string),   None,         Precedence::None      );
        rule!(TokenType::TokenNumber,       Some(number),   None,         Precedence::None      );
        rule!(TokenType::TokenAnd,          None,           Some(and_),   Precedence::And       );
        rule!(TokenType::TokenClass,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenElse,         None,           None,         Precedence::None      );
        rule!(TokenType::TokenFalse,        Some(literal),  None,         Precedence::None      );
        rule!(TokenType::TokenFor,          None,           None,         Precedence::None      );
        rule!(TokenType::TokenFun,          None,           None,         Precedence::None      );
        rule!(TokenType::TokenIf,           None,           None,         Precedence::None      );
        rule!(TokenType::TokenNil,          Some(literal),  None,         Precedence::None      );
        rule!(TokenType::TokenOr,           None,           Some(or_),    Precedence::Or        );
        rule!(TokenType::TokenPrint,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenReturn,       None,           None,         Precedence::None      );
        rule!(TokenType::TokenSuper,        Some(super_),   None,         Precedence::None      );
        rule!(TokenType::TokenThis,         Some(this_),    None,         Precedence::None      );
        rule!(TokenType::TokenTrue,         Some(literal),  None,         Precedence::None      );
        rule!(TokenType::TokenVar,          None,           None,         Precedence::None      );
        rule!(TokenType::TokenWhile,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenError,        None,           None,         Precedence::None      );
        rule!(TokenType::TokenEof,          None,           None,         Precedence::None      );
    }

    Rules(rules)
}

pub(crate) fn identifiers_equal(a: &Token, b: &Token) -> bool {
    if a.length != b.length {
        return false;
    }

    a.text() == b.text()
}
