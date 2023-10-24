#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;

  Chunk *chunk;

  bool had_error;
  bool panicking;

  Scanner *scanner;
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

Chunk *current_chunk(Parser *parser) { return parser->chunk; }

void emit_byte(Parser *parser, uint8_t byte) {
  chunk_write(current_chunk(parser), byte, parser->previous.line);
}

void emit_bytes(Parser *parser, uint8_t b1, uint8_t b2) {
  chunk_write(current_chunk(parser), b1, parser->previous.line);
  chunk_write(current_chunk(parser), b2, parser->previous.line);
}

void emit_return(Parser *parser) { emit_byte(parser, OP_RETURN); }

void emit_constant(Parser *parser, Value value) {
  uint8_t idx = chunk_add_constant(current_chunk(parser), value);
  emit_bytes(parser, OP_CONSTANT, idx);
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

typedef void (*ParseFn)(Parser *);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

void expression(Parser *parser);
ParseRule *get_rule(TokenType type);
void parse_precedence(Parser *parser, Precedence precedence);

void parse_precedence(Parser *parser, Precedence precedence) {
  parser_advance(parser);
  ParseFn prefix = get_rule(parser->previous.type)->prefix;
  if (prefix == NULL) {
    error(parser, "expect expression");
    return;
  }

  prefix(parser);

  while (precedence <= get_rule(parser->current.type)->precedence) {
    parser_advance(parser);

    ParseFn infix = get_rule(parser->previous.type)->infix;
    infix(parser);
  }
}

void binary(Parser *parser) {
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

void literal(Parser *parser) {
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

void group(Parser *parser) {
  expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "expect ')' after expression");
}

void number(Parser *parser) {
  double value = strtod(parser->previous.start, NULL);
  emit_constant(parser, V_NUMBER(value));
}

void unary(Parser *parser) {
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

ParseRule rules[] = {
  // clang-format off
  [TOKEN_LEFT_PAREN]    = {group,   NULL,   PREC_NONE      },
  [TOKEN_RIGHT_PAREN]   = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_LEFT_BRACE]    = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_RIGHT_BRACE]   = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_COMMA]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_DOT]           = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_MINUS]         = {unary,   binary, PREC_TERM      },
  [TOKEN_PLUS]          = {NULL,    binary, PREC_TERM      },
  [TOKEN_SEMICOLON]     = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_SLASH]         = {NULL,    binary, PREC_FACTOR    },
  [TOKEN_STAR]          = {NULL,    binary, PREC_FACTOR    },
  [TOKEN_BANG]          = {unary,   NULL,   PREC_NONE      },
  [TOKEN_BANG_EQUAL]    = {NULL,    binary, PREC_EQUALITY  },
  [TOKEN_EQUAL]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_EQUAL_EQUAL]   = {NULL,    binary, PREC_EQUALITY  },
  [TOKEN_GREATER]       = {NULL,    binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,    binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,    binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,    binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_STRING]        = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_NUMBER]        = {number,  NULL,   PREC_NONE      },
  [TOKEN_AND]           = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_CLASS]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_ELSE]          = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_FALSE]         = {literal, NULL,   PREC_NONE      },
  [TOKEN_FOR]           = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_FUN]           = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_IF]            = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_NIL]           = {literal, NULL,   PREC_NONE      },
  [TOKEN_OR]            = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_PRINT]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_RETURN]        = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_SUPER]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_THIS]          = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_TRUE]          = {literal, NULL,   PREC_NONE      },
  [TOKEN_VAR]           = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_WHILE]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_ERROR]         = {NULL,    NULL,   PREC_NONE      },
  [TOKEN_EOF]           = {NULL,    NULL,   PREC_NONE      },
  // clang-format on
};

ParseRule *get_rule(TokenType type) { return &rules[type]; }

bool compile(const char *source, Chunk *chunk) {
  Scanner scanner;
  scanner_init(&scanner, source);

  Parser parser;
  parser.scanner = &scanner;
  parser.chunk = chunk;
  parser.had_error = false;
  parser.panicking = false;

  parser_advance(&parser);
  expression(&parser);
  consume(&parser, TOKEN_EOF, "expect end of expression");
  end_compilation(&parser);

  return !parser.had_error;
}
