#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "compiler.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;

  Chunk *chunk;
  Scanner *scanner;

  bool had_error;
  bool panicking;

  // Borrowed from VM
  Obj **objects;
  Table *strings;
  // end borrow
} Parser;

void error_at(Parser *parser, Token *token, const char *msg) {
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

void error(Parser *parser, const char *msg) {
  error_at(parser, &parser->previous, msg);
}

void error_at_current(Parser *parser, const char *msg) {
  error_at(parser, &parser->current, msg);
}

void parser_advance(Parser *parser) {
  parser->previous = parser->current;
  for (;;) {
    parser->current = scanner_next(parser->scanner);

    if (parser->current.type != TOKEN_ERROR)
      break;

    error_at_current(parser, parser->current.start);
  }
}

void consume(Parser *parser, TokenType type, const char *msg) {
  if (parser->current.type == type) {
    parser_advance(parser);
    return;
  }

  error_at_current(parser, msg);
}

bool check(Parser *parser, TokenType type) {
  return parser->current.type == type;
}

bool parser_match(Parser *parser, TokenType type) {
  if (!check(parser, type)) {
    return false;
  }
  parser_advance(parser);
  return true;
}

Chunk *current_chunk(Parser *parser) { return parser->chunk; }

void emit_byte(Parser *parser, uint8_t byte) {
  chunk_write(current_chunk(parser), byte, parser->previous.line);
}

void emit_bytes(Parser *parser, uint8_t b1, uint8_t b2) {
  Chunk *chunk = current_chunk(parser);
  chunk_write(chunk, b1, parser->previous.line);
  chunk_write(chunk, b2, parser->previous.line);
}

void emit_return(Parser *parser) { emit_byte(parser, OP_RETURN); }

uint8_t make_constant(Parser *parser, Value value) {
  return chunk_add_constant(current_chunk(parser), value);
}

void emit_constant(Parser *parser, Value value) {
  emit_bytes(parser, OP_CONSTANT, make_constant(parser, value));
}

void end_compilation(Parser *parser) {
  emit_return(parser);

#ifdef DEBUG_PRINT_CODE
  if (!parser->had_error) {
    disassemble_chunk(current_chunk(parser), "<code>");
  }
#endif
}

// Grammar

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

void expression(Parser *parser);
void parse_precedence(Parser *parser, Precedence precedence);
void statement(Parser *parser);
void declaration(Parser *parser);
ParseRule *get_rule(TokenType type);

void parse_precedence(Parser *parser, Precedence precedence) {
  parser_advance(parser);
  ParseFn prefix = get_rule(parser->previous.type)->prefix;
  if (prefix == NULL) {
    error(parser, "expect expression");
    return;
  }

  bool can_assign = precedence <= PREC_ASSIGNMENT;
  prefix(parser, can_assign);

  while (precedence <= get_rule(parser->current.type)->precedence) {
    parser_advance(parser);

    ParseFn infix = get_rule(parser->previous.type)->infix;
    infix(parser, can_assign);
  }

  if (can_assign && parser_match(parser, TOKEN_EQUAL)) {
    error(parser, "invalid assignment target");
  }
}

uint8_t identifier_constant(Parser *parser, Token *name) {
  ObjString *string = str_clone(parser->objects, parser->strings, name->start,
                                (size_t)(name->length));
  return make_constant(parser, V_OBJ(string));
}

uint8_t parse_variable(Parser *parser, const char *msg) {
  consume(parser, TOKEN_IDENTIFIER, msg);
  return identifier_constant(parser, &parser->previous);
}

void define_variable(Parser *parser, uint8_t global) {
  emit_bytes(parser, OP_DEFINE_GLOBAL, global);
}

void binary(Parser *parser, bool UNUSED(can_assign)) {
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

void literal(Parser *parser, bool UNUSED(can_assign)) {
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

void group(Parser *parser, bool UNUSED(can_assign)) {
  expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after expression");
}

void number(Parser *parser, bool UNUSED(can_assign)) {
  double f = strtod(parser->previous.start, NULL);
  emit_constant(parser, V_NUMBER(f));
}

void string(Parser *parser, bool UNUSED(can_assign)) {
  // Drop the leading quote '"'
  const char *start = parser->previous.start + 1;
  // Drop the trailing quote '"' and one more because pointer math.
  size_t length = (size_t)(parser->previous.length - 2);

  ObjString *str = str_clone(parser->objects, parser->strings, start, length);
  // TODO: This is where handling escape sequence would go.
  emit_constant(parser, V_OBJ(str));
}

void named_variable(Parser *parser, Token name, bool can_assign) {
  uint8_t arg = identifier_constant(parser, &name);

  if (can_assign && parser_match(parser, TOKEN_EQUAL)) {
    expression(parser);
    emit_bytes(parser, OP_SET_GLOBAL, arg);
  } else {
    emit_bytes(parser, OP_GET_GLOBAL, arg);
  }
}

void variable(Parser *parser, bool can_assign) {
  named_variable(parser, parser->previous, can_assign);
}

void unary(Parser *parser, bool UNUSED(can_assign)) {
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

void expression(Parser *parser) { parse_precedence(parser, PREC_ASSIGNMENT); }

void decl_var(Parser *parser) {
  uint8_t global = parse_variable(parser, "expect variable name");

  if (parser_match(parser, TOKEN_EQUAL)) {
    expression(parser);
  } else {
    emit_byte(parser, OP_NIL);
  }
  consume(parser, TOKEN_SEMICOLON, "expect ';' after variable declaration");

  define_variable(parser, global);
}

void stmt_expr(Parser *parser) {
  expression(parser);
  consume(parser, TOKEN_SEMICOLON, "expect ';' after expression");
  emit_byte(parser, OP_POP);
}

void stmt_print(Parser *parser) {
  expression(parser);
  consume(parser, TOKEN_SEMICOLON, "expect ';' after value");
  emit_byte(parser, OP_PRINT);
}

void synchronize(Parser *parser) {
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

    parser_advance(parser);
  }
}

void declaration(Parser *parser) {
  if (parser_match(parser, TOKEN_VAR)) {
    decl_var(parser);
  } else {
    statement(parser);
  }

  if (parser->panicking) {
    synchronize(parser);
  }
}

void statement(Parser *parser) {
  if (parser_match(parser, TOKEN_PRINT)) {
    stmt_print(parser);
  } else {
    stmt_expr(parser);
  }
}

ParseRule rules[] = {
  // clang-format off
  [TOKEN_LEFT_PAREN]    = {group,    NULL,   PREC_NONE      },
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
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE      },
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE      },
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE      },
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE      },
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

ParseRule *get_rule(TokenType type) { return &rules[type]; }

bool compile(const char *source, Chunk *chunk, Obj **objects, Table *strings) {
  Scanner scanner;
  scanner_init(&scanner, source);

  Parser parser;
  parser.scanner = &scanner;
  parser.chunk = chunk;
  parser.had_error = false;
  parser.panicking = false;
  parser.objects = objects;
  parser.strings = strings;

  parser_advance(&parser);

  while (!parser_match(&parser, TOKEN_EOF)) {
    declaration(&parser);
  }
  end_compilation(&parser);

  return !parser.had_error;
}
