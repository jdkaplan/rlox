#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static void error_at(Parser *parser, Token *token, const char *msg) {
  // Avoid emitting errors while panicking. This flag will be cleared by error
  // recovery.
  if (parser->panicking) {
    return;
  }
  parser->panicking = true;

  fprintf(stderr, "[line %d] error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end of file");
  } else if (token->type == TOKEN_ERROR) {
    // No location info
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", msg);
  parser->had_error = true;
}

static void error(Parser *parser, const char *msg) {
  error_at(parser, &parser->previous, msg);
}

static void error_at_current(Parser *parser, const char *msg) {
  error_at(parser, &parser->current, msg);
}

static void advance(Parser *parser) {
  parser->previous = parser->current;
  for (;;) {
    parser->current = scanner_next(parser->scanner);

    // dbg_token(parser->current);

    if (parser->current.type != TOKEN_ERROR)
      break;

    error_at_current(parser, parser->current.start);
  }
}

static void consume(Parser *parser, TokenType type, const char *msg) {
  if (parser->current.type == type) {
    advance(parser);
    return;
  }

  error_at_current(parser, msg);
}

static bool check(Parser *parser, TokenType type) {
  return parser->current.type == type;
}

static bool match(Parser *parser, TokenType type) {
  if (!check(parser, type)) {
    return false;
  }
  advance(parser);
  return true;
}

static Chunk *current_chunk(Parser *parser) {
  return &parser->compiler->function->chunk;
}

static void emit_byte(Parser *parser, uint8_t byte) {
  Gc gc = {parser->vm, parser->compiler};

  chunk_write(gc, current_chunk(parser), byte, parser->previous.line);
}

static void emit_bytes(Parser *parser, uint8_t b1, uint8_t b2) {
  Gc gc = {parser->vm, parser->compiler};

  Chunk *chunk = current_chunk(parser);
  chunk_write(gc, chunk, b1, parser->previous.line);
  chunk_write(gc, chunk, b2, parser->previous.line);
}

static void emit_loop(Parser *parser, unsigned int loop_start) {
  emit_byte(parser, OP_LOOP);

  unsigned int offset = VEC_LEN(current_chunk(parser)->code) - loop_start + 2;
  if (offset > UINT16_MAX) {
    error(parser, "loop body too large");
  }

  emit_byte(parser, (uint8_t)((offset >> 8) & 0xff));
  emit_byte(parser, (uint8_t)(offset & 0xff));
}

static unsigned int emit_jump(Parser *parser, Opcode op) {
  emit_byte(parser, op);
  emit_bytes(parser, 0xff, 0xff);
  return VEC_LEN(current_chunk(parser)->code) - 2;
}

static void patch_jump(Parser *parser, unsigned int offset) {
  // Walk two extra bytes past the offset itself.
  unsigned int jump = (VEC_LEN(current_chunk(parser)->code)) - offset - 2;

  if (jump > UINT16_MAX) {
    error(parser, "too much code to jump over");
  }

  VEC_SET(current_chunk(parser)->code, offset, (jump >> 8) & 0xff);
  VEC_SET(current_chunk(parser)->code, offset + 1, jump & 0xff);
}

static void emit_return(Parser *parser) {
  emit_byte(parser, OP_NIL);
  emit_byte(parser, OP_RETURN);
}

static uint8_t make_constant(Parser *parser, Value value) {
  Gc gc = {parser->vm, parser->compiler};

  return chunk_add_constant(gc, current_chunk(parser), value);
}

static void emit_constant(Parser *parser, Value value) {
  emit_bytes(parser, OP_CONSTANT, make_constant(parser, value));
}

static ObjFunction *end_compiler(Parser *parser) {
  emit_return(parser);
  ObjFunction *function = parser->compiler->function;

#ifdef DEBUG_PRINT_CODE
  if (!parser->had_error) {
    disassemble_chunk(current_chunk(parser), function->name != NULL
                                                 ? function->name->chars
                                                 : "<script>");
  }
#endif

  parser->compiler = parser->compiler->enclosing;
  return function;
}

static void compiler_init(Parser *parser, Compiler *compiler,
                          FunctionMode mode) {
  Gc gc = {parser->vm, parser->compiler};

  compiler->enclosing = parser->compiler;
  compiler->function = NULL;
  compiler->mode = mode;
  compiler->local_count = 0;
  compiler->scope_depth = 0;

  compiler->function = function_new(gc);

  parser->compiler = compiler;

  // Reserve stack slot zero for the currently executing function.
  Local *local = &parser->compiler->locals[parser->compiler->local_count++];
  local->depth = 0;
  local->is_captured = false;
  local->name.start = "";
  local->name.length = 0;
}

static void scope_begin(Parser *parser) {
  Compiler *compiler = parser->compiler;
  compiler->scope_depth++;
}

static void scope_end(Parser *parser) {
  Compiler *compiler = parser->compiler;
  compiler->scope_depth--;

  // TODO: OP_POP_N/OP_CLOSE_UPVALUE_N is a good optimization here.
  while (compiler->local_count > 0 &&
         compiler->locals[compiler->local_count - 1].depth >
             compiler->scope_depth) {
    if (compiler->locals[compiler->local_count - 1].is_captured) {
      emit_byte(parser, OP_CLOSE_UPVALUE);
    } else {
      emit_byte(parser, OP_POP);
    }
    compiler->local_count--;
  }
}

static void add_local(Parser *parser, Token name) {
  if (parser->compiler->local_count == UINT8_COUNT) {
    error(parser, "too many local variables in function");
    return;
  }

  Local *local = &parser->compiler->locals[parser->compiler->local_count++];
  local->name = name;
  local->depth = -1; // declared: reserved without value
  local->is_captured = false;
}

static void mark_initialized(Compiler *compiler) {
  if (compiler->scope_depth == 0) {
    // Global variables don't have their initialization tracked like this.
    return;
  }
  compiler->locals[compiler->local_count - 1].depth = compiler->scope_depth;
  // defined: value available at a certain depth
}

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(Parser *, bool);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

// Grammar

static void expression(Parser *parser);
static void parse_precedence(Parser *parser, Precedence precedence);
static void statement(Parser *parser);
static void declaration(Parser *parser);
static ParseRule *get_rule(TokenType type);

static void parse_precedence(Parser *parser, Precedence precedence) {
  advance(parser);
  ParseFn prefix = get_rule(parser->previous.type)->prefix;
  if (prefix == NULL) {
    error(parser, "expect expression");
    return;
  }

  bool can_assign = precedence <= PREC_ASSIGNMENT;
  prefix(parser, can_assign);

  while (precedence <= get_rule(parser->current.type)->precedence) {
    advance(parser);

    ParseFn infix = get_rule(parser->previous.type)->infix;
    infix(parser, can_assign);
  }

  if (can_assign && match(parser, TOKEN_EQUAL)) {
    error(parser, "invalid assignment target");
  }
}

static uint8_t identifier_constant(Parser *parser, Token *name) {
  Gc gc = {parser->vm, parser->compiler};

  ObjString *string = str_clone(gc, name->start, (size_t)(name->length));
  return make_constant(parser, V_OBJ(string));
}

static bool identifiers_equal(Token *a, Token *b) {
  if (a->length != b->length) {
    return false;
  }
  return memcmp(a->start, b->start, (size_t)(a->length)) == 0;
}

static int resolve_local(Parser *parser, Compiler *compiler, Token *name) {
  // Parser is only used to report errors.
  // Compiler is used to resolve locals.

  for (int i = compiler->local_count - 1; i >= 0; i--) {
    Local *local = &compiler->locals[i];
    if (identifiers_equal(name, &local->name)) {
      if (local->depth == -1) {
        error(parser, "can't read local variable in its own initializer");
      }
      return i;
    }
  }
  return -1;
}

static int add_upvalue(Parser *parser, Compiler *compiler, uint8_t index,
                       bool is_local) {
  // Parser is only used to report errors.
  // Compiler is used to resolve locals.

  int upvalue_count = compiler->function->upvalue_count;

  for (int i = 0; i < upvalue_count; i++) {
    Upvalue *upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->is_local == is_local) {
      return i;
    }
  }

  if (upvalue_count == UINT8_COUNT) {
    error(parser, "too many closure variables in function");
    return 0;
  }

  compiler->upvalues[upvalue_count].is_local = is_local;
  compiler->upvalues[upvalue_count].index = index;
  return compiler->function->upvalue_count++;
}

static int resolve_upvalue(Parser *parser, Compiler *compiler, Token *name) {
  // Parser is only used to report errors.
  // Compiler is used to resolve locals.

  if (compiler->enclosing == NULL) {
    return -1;
  }

  int local = resolve_local(parser, compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].is_captured = true;
    return add_upvalue(parser, compiler, (uint8_t)(local), true);
  }

  int upvalue = resolve_upvalue(parser, compiler->enclosing, name);
  if (upvalue != -1) {
    return add_upvalue(parser, compiler, (uint8_t)(upvalue), false);
  }

  return -1;
}

static void declare_variable(Parser *parser) {
  if (parser->compiler->scope_depth == 0) {
    // Globals are late-bound and don't have a location on the stack.
    return;
  }

  Token *name = &parser->previous;
  for (int i = parser->compiler->local_count - 1; i >= 0; i--) {
    Local *local = &parser->compiler->locals[i];
    if (local->depth != -1 && local->depth < parser->compiler->scope_depth) {
      break;
    }

    if (identifiers_equal(name, &local->name)) {
      error(parser, "variable already declared in this scope");
    }
  }
  add_local(parser, *name);
}

static uint8_t parse_variable(Parser *parser, const char *msg) {
  consume(parser, TOKEN_IDENTIFIER, msg);

  declare_variable(parser);
  if (parser->compiler->scope_depth > 0) {
    return 0;
  }

  return identifier_constant(parser, &parser->previous);
}

static void define_variable(Parser *parser, uint8_t global) {
  if (parser->compiler->scope_depth > 0) {
    mark_initialized(parser->compiler);
    // No runtime code to execute! The value is already in the stack slot.
    return;
  }
  emit_bytes(parser, OP_DEFINE_GLOBAL, global);
}

static uint8_t argument_list(Parser *parser) {
  uint8_t arg_count = 0;
  if (!check(parser, TOKEN_RIGHT_PAREN)) {
    do {
      expression(parser);
      if (arg_count == UINT8_MAX) {
        error(parser, "too many arguments");
      }
      arg_count++;
    } while (match(parser, TOKEN_COMMA));
  }
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after arguments");
  return arg_count;
}

static void and_(Parser *parser, bool UNUSED(can_assign)) {
  // [ a ]

  // jump_false_peek end
  unsigned int end_jump = emit_jump(parser, OP_JUMP_IF_FALSE);
  // pop a
  emit_byte(parser, OP_POP);
  // <b>
  parse_precedence(parser, PREC_AND);
  // end:
  patch_jump(parser, end_jump);

  // [ a and b ]
}

static void or_(Parser *parser, bool UNUSED(can_assign)) {
  // TODO: Implement symmetrically with and_.

  // [ a ]

  // jump_false_peek else
  unsigned int else_jump = emit_jump(parser, OP_JUMP_IF_FALSE);
  // jump end
  unsigned int end_jump = emit_jump(parser, OP_JUMP);
  // else:
  patch_jump(parser, else_jump);
  // pop a
  emit_byte(parser, OP_POP);
  // <b>
  parse_precedence(parser, PREC_OR);
  // end:
  patch_jump(parser, end_jump);

  // [ a or b ]
}

static void binary(Parser *parser, bool UNUSED(can_assign)) {
  TokenType op = parser->previous.type;
  ParseRule *rule = get_rule(op);
  parse_precedence(parser, rule->precedence + 1);

  switch (op) {
  case TOKEN_PLUS: {
    emit_byte(parser, OP_ADD);
    break;
  }
  case TOKEN_MINUS: {
    emit_byte(parser, OP_SUB);
    break;
  }
  case TOKEN_STAR: {
    emit_byte(parser, OP_MUL);
    break;
  }
  case TOKEN_SLASH: {
    emit_byte(parser, OP_DIV);
    break;
  }

  case TOKEN_BANG_EQUAL: {
    emit_bytes(parser, OP_EQUAL, OP_NOT);
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    emit_byte(parser, OP_EQUAL);
    break;
  }
  case TOKEN_GREATER: {
    emit_byte(parser, OP_GREATER);
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    emit_bytes(parser, OP_LESS, OP_NOT);
    break;
  }
  case TOKEN_LESS: {
    emit_byte(parser, OP_LESS);
    break;
  }
  case TOKEN_LESS_EQUAL: {
    emit_bytes(parser, OP_GREATER, OP_NOT);
    break;
  }

  default:
    return; // Unreachable
  }
}

static void call(Parser *parser, bool UNUSED(can_assign)) {
  uint8_t arg_count = argument_list(parser);
  emit_bytes(parser, OP_CALL, arg_count);
}

static void literal(Parser *parser, bool UNUSED(can_assign)) {
  switch (parser->previous.type) {
  case TOKEN_FALSE: {
    emit_byte(parser, OP_FALSE);
    break;
  }
  case TOKEN_TRUE: {
    emit_byte(parser, OP_TRUE);
    break;
  }
  case TOKEN_NIL: {
    emit_byte(parser, OP_NIL);
    break;
  }
  default:
    return; // Unreachable
  }
}

static void group(Parser *parser, bool UNUSED(can_assign)) {
  expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after expression");
}

static void number(Parser *parser, bool UNUSED(can_assign)) {
  double f = strtod(parser->previous.start, NULL);
  emit_constant(parser, V_NUMBER(f));
}

static void string(Parser *parser, bool UNUSED(can_assign)) {
  Gc gc = {parser->vm, parser->compiler};

  // Drop the leading quote '"'
  const char *start = parser->previous.start + 1;
  // Drop the trailing quote '"' and one more because pointer math.
  size_t length = (size_t)(parser->previous.length - 2);

  ObjString *str = str_clone(gc, start, length);
  // TODO: This is where handling escape sequence would go.
  emit_constant(parser, V_OBJ(str));
}

static void named_variable(Parser *parser, Token name, bool can_assign) {
  Opcode op_get, op_set;
  uint8_t slot;
  int arg = resolve_local(parser, parser->compiler, &name);
  if (arg != -1) {
    slot = (uint8_t)(arg);
    op_get = OP_GET_LOCAL;
    op_set = OP_SET_LOCAL;
  } else if ((arg = resolve_upvalue(parser, parser->compiler, &name)) != -1) {
    slot = (uint8_t)(arg);
    op_get = OP_GET_UPVALUE;
    op_set = OP_SET_UPVALUE;
  } else {
    slot = identifier_constant(parser, &name);
    op_get = OP_GET_GLOBAL;
    op_set = OP_SET_GLOBAL;
  }

  if (can_assign && match(parser, TOKEN_EQUAL)) {
    expression(parser);
    emit_bytes(parser, op_set, slot);
  } else {
    emit_bytes(parser, op_get, slot);
  }
}

static void variable(Parser *parser, bool can_assign) {
  named_variable(parser, parser->previous, can_assign);
}

static void unary(Parser *parser, bool UNUSED(can_assign)) {
  TokenType op = parser->previous.type;

  parse_precedence(parser, PREC_UNARY);

  switch (op) {
  case TOKEN_BANG: {
    emit_byte(parser, OP_NOT);
    break;
  }
  case TOKEN_MINUS: {
    emit_byte(parser, OP_NEG);
    break;
  }
  default:
    return; // Unreachable
  }
}

static void expression(Parser *parser) {
  parse_precedence(parser, PREC_ASSIGNMENT);
}

static void block(Parser *parser) {
  while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
    declaration(parser);
  }

  consume(parser, TOKEN_RIGHT_BRACE, "expect '}' after block");
}

static void function(Parser *parser, FunctionMode mode) {
  Compiler compiler;
  compiler_init(parser, &compiler, mode);

  Gc gc = {parser->vm, parser->compiler};

  if (mode != MODE_SCRIPT) {
    compiler.function->name = str_clone(gc, parser->previous.start,
                                        (size_t)(parser->previous.length));
  }

  scope_begin(parser);

  consume(parser, TOKEN_LEFT_PAREN, "expect '(' after function name");
  if (!check(parser, TOKEN_RIGHT_PAREN)) {
    do {
      parser->compiler->function->arity++;
      if (parser->compiler->function->arity > UINT8_MAX) {
        error_at_current(parser, "too many parameters");
      }

      uint8_t constant = parse_variable(parser, "expect parameter name");
      define_variable(parser, constant);
    } while (match(parser, TOKEN_COMMA));
  }
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after function parameters");
  consume(parser, TOKEN_LEFT_BRACE, "expect '{' before function body");
  block(parser);

  // scope_end(parser) isn't necessary because all locals will get implicitly
  // popped when the VM pops the CallFrame.
  ObjFunction *function = end_compiler(parser);
  emit_bytes(parser, OP_CLOSURE, make_constant(parser, V_OBJ(function)));

  for (int i = 0; i < function->upvalue_count; i++) {
    emit_byte(parser, compiler.upvalues[i].is_local ? 1 : 0);
    emit_byte(parser, compiler.upvalues[i].index);
  }
}

static void decl_fun(Parser *parser) {
  uint8_t global = parse_variable(parser, "expect function name");

  // Allow the function's name to be used within its body to support recursion.
  mark_initialized(parser->compiler);

  function(parser, MODE_FUNCTION);

  define_variable(parser, global);
}

static void decl_var(Parser *parser) {
  uint8_t global = parse_variable(parser, "expect variable name");

  if (match(parser, TOKEN_EQUAL)) {
    expression(parser);
  } else {
    emit_byte(parser, OP_NIL);
  }
  consume(parser, TOKEN_SEMICOLON, "expect ';' after variable declaration");

  define_variable(parser, global);
}

static void stmt_expr(Parser *parser) {
  expression(parser);
  consume(parser, TOKEN_SEMICOLON, "expect ';' after expression");
  emit_byte(parser, OP_POP);
}

static void stmt_if(Parser *parser) {
  // <cond>
  consume(parser, TOKEN_LEFT_PAREN, "expect '(' after 'if'");
  expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after condition");

  // jump_false_peek else
  unsigned int then_jump = emit_jump(parser, OP_JUMP_IF_FALSE);
  // pop cond
  emit_byte(parser, OP_POP);
  // <conseq>
  statement(parser);

  // TODO: Avoid the double-jump if no else branch.

  // jump out
  unsigned int else_jump = emit_jump(parser, OP_JUMP);

  // else:
  patch_jump(parser, then_jump);
  // pop cond
  emit_byte(parser, OP_POP);

  // <alt>
  if (match(parser, TOKEN_ELSE)) {
    statement(parser);
  }
  // out:
  patch_jump(parser, else_jump);
}

static void stmt_print(Parser *parser) {
  expression(parser);
  consume(parser, TOKEN_SEMICOLON, "expect ';' after value");
  emit_byte(parser, OP_PRINT);
}

static void stmt_return(Parser *parser) {
  if (parser->compiler->mode == MODE_SCRIPT) {
    error(parser, "cannot return from top-level code");
  }

  if (match(parser, TOKEN_SEMICOLON)) {
    emit_return(parser);
  } else {
    expression(parser);
    consume(parser, TOKEN_SEMICOLON, "expect ';' after return value");
    emit_byte(parser, OP_RETURN);
  }
}

static void stmt_while(Parser *parser) {
  // start:
  unsigned int loop_start = VEC_LEN(current_chunk(parser)->code);
  // <cond>
  consume(parser, TOKEN_LEFT_PAREN, "expect '(' after 'while'");
  expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after condition");

  // jump_false_peek exit
  unsigned int exit_jump = emit_jump(parser, OP_JUMP_IF_FALSE);
  // pop cond
  emit_byte(parser, OP_POP);
  // <body>
  statement(parser);
  // jump start
  emit_loop(parser, loop_start);
  // exit:
  patch_jump(parser, exit_jump);
  // pop cond
  emit_byte(parser, OP_POP);
}

static void stmt_for(Parser *parser) {
  scope_begin(parser);

  consume(parser, TOKEN_LEFT_PAREN, "expect '(' after 'for'");

  // <init>
  if (match(parser, TOKEN_SEMICOLON)) {
    // No init
  } else if (match(parser, TOKEN_VAR)) {
    decl_var(parser);
  } else {
    stmt_expr(parser);
  }

  // start:
  unsigned int loop_start = VEC_LEN(current_chunk(parser)->code);

  bool has_exit = false;
  unsigned int exit_jump = 0;

  // <cond>
  if (!match(parser, TOKEN_SEMICOLON)) {
    expression(parser);
    consume(parser, TOKEN_SEMICOLON, "expect ';' after for loop condition");

    // jump_false_peek exit
    exit_jump = emit_jump(parser, OP_JUMP_IF_FALSE);
    has_exit = true;

    // pop cond
    emit_byte(parser, OP_POP);
  }

  if (!match(parser, TOKEN_RIGHT_PAREN)) {
    // jump body
    unsigned int body_jump = emit_jump(parser, OP_JUMP);

    // incr:
    unsigned int incr_start = VEC_LEN(current_chunk(parser)->code);
    // <next>
    expression(parser);
    // pop next
    emit_byte(parser, OP_POP);

    consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after for loop clauses");

    // jump start
    emit_loop(parser, loop_start);
    loop_start = incr_start;

    // body:
    patch_jump(parser, body_jump);
  }

  // <body>
  statement(parser);
  // jump start
  emit_loop(parser, loop_start);
  // exit:
  if (has_exit) {
    patch_jump(parser, exit_jump);
    // pop cond
    emit_byte(parser, OP_POP);
  }

  scope_end(parser);
}

static void synchronize(Parser *parser) {
  parser->panicking = false;

  while (parser->current.type != TOKEN_EOF) {
    if (parser->previous.type == TOKEN_SEMICOLON) {
      return;
    }

    switch (parser->current.type) {
    case TOKEN_CLASS:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;

    default:;
    }

    advance(parser);
  }
}

static void declaration(Parser *parser) {
  if (match(parser, TOKEN_FUN)) {
    decl_fun(parser);
  } else if (match(parser, TOKEN_VAR)) {
    decl_var(parser);
  } else {
    statement(parser);
  }

  if (parser->panicking) {
    synchronize(parser);
  }
}

static void statement(Parser *parser) {
  if (match(parser, TOKEN_PRINT)) {
    stmt_print(parser);
  } else if (match(parser, TOKEN_IF)) {
    stmt_if(parser);
  } else if (match(parser, TOKEN_FOR)) {
    stmt_for(parser);
  } else if (match(parser, TOKEN_RETURN)) {
    stmt_return(parser);
  } else if (match(parser, TOKEN_WHILE)) {
    stmt_while(parser);
  } else if (match(parser, TOKEN_LEFT_BRACE)) {
    scope_begin(parser);
    block(parser);
    scope_end(parser);
  } else {
    stmt_expr(parser);
  }
}

ParseRule rules[] = {
  // clang-format off
  [TOKEN_LEFT_PAREN]    = {group,    call,   PREC_CALL      },
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM      },
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM      },
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR    },
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR    },
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE      },
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY  },
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY  },
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE      },
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE      },
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE      },
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND       },
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE      },
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE      },
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR        },
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE      },
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE      },
  // clang-format on
};

static ParseRule *get_rule(TokenType type) { return &rules[type]; }

ObjFunction *compile(Vm *vm, const char *source) {
  Scanner scanner;
  scanner_init(&scanner, source);

  Compiler compiler;

  Parser parser;
  parser.scanner = &scanner;
  parser.compiler = NULL;
  parser.had_error = false;
  parser.panicking = false;
  parser.vm = vm;

  compiler_init(&parser, &compiler, MODE_SCRIPT);

  advance(&parser);

  while (!match(&parser, TOKEN_EOF)) {
    declaration(&parser);
  }

  ObjFunction *fun = end_compiler(&parser);
  return parser.had_error ? NULL : fun;
}
