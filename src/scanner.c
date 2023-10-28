#include <stdbool.h>
#include <string.h>

#include "scanner.h"

void scanner_init(Scanner *scanner, const char *source) {
  scanner->start = source;
  scanner->current = source;
  scanner->line = 1;
}

static bool at_eof(Scanner *scanner) { return *scanner->current == '\0'; }

static Token make_token(Scanner *scanner, TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner->start;
  token.length = (int)(scanner->current - scanner->start);
  token.line = scanner->line;
  return token;
}

static Token make_error(Scanner *scanner, const char *msg) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = msg;
  token.length = (int)(strlen(msg));
  token.line = scanner->line;
  return token;
}

static char advance(Scanner *scanner) {
  scanner->current++;
  return scanner->current[-1];
}

static char peek(Scanner *scanner) { return *scanner->current; }

static char peek_next(Scanner *scanner) {
  if (at_eof(scanner)) {
    return '\0';
  }
  return scanner->current[1];
}

static bool match(Scanner *scanner, const char expected) {
  if (at_eof(scanner)) {
    return false;
  }

  if (*scanner->current != expected) {
    return false;
  }

  scanner->current++;
  return true;
}

static void skip_ignored(Scanner *scanner) {
  for (;;) {
    char c = peek(scanner);
    switch (c) {

    // Whitespace
    case ' ':
    case '\r':
    case '\t':
      advance(scanner);
      break;
    case '\n':
      scanner->line++;
      advance(scanner);
      break;

      // Comments
    case '/':
      if (peek_next(scanner) == '/') {
        while (peek(scanner) != '\n' && !at_eof(scanner)) {
          advance(scanner);
        }
      } else {
        return;
      }
      break;

    default:
      return;
    }
  }
}

static Token scan_string(Scanner *scanner) {
  while (peek(scanner) != '"' && !at_eof(scanner)) {
    if (peek(scanner) == '\n') {
      scanner->line++;
    }
    advance(scanner);
  }

  if (at_eof(scanner)) {
    return make_error(scanner, "unterminated string");
  }

  // Consume the '"' found by peek()
  advance(scanner);
  return make_token(scanner, TOKEN_STRING);
}

static bool is_digit(const char c) { return '0' <= c && c <= '9'; }

static Token scan_number(Scanner *scanner) {
  while (is_digit(peek(scanner))) {
    advance(scanner);
  }

  // Only consume the dot if there are also more digits afterward. Otherwise, it
  // indicates a method call rather than a decimal number.
  if (peek(scanner) == '.' && is_digit(peek_next(scanner))) {
    advance(scanner); // '.'

    // Everything after the dot
    while (is_digit(peek(scanner))) {
      advance(scanner);
    }
  }

  return make_token(scanner, TOKEN_NUMBER);
}

static bool is_alpha(const char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static TokenType keyword_or_identifier(Scanner *scanner) {
  // The chars tracked by the scanner _are not_ a null-terminated string. Be
  // careful with lengths and comparisons!
  const char *id = scanner->start;
  size_t len = (size_t)(scanner->current - scanner->start);

#define KWD_IF(kwd, type)                                                      \
  if (strlen(kwd) == len && strncmp(kwd, id, len) == 0) {                      \
    return type;                                                               \
  }

  KWD_IF("and", TOKEN_AND)
  KWD_IF("class", TOKEN_CLASS)
  KWD_IF("else", TOKEN_ELSE)
  KWD_IF("false", TOKEN_FALSE)
  KWD_IF("if", TOKEN_IF)
  KWD_IF("nil", TOKEN_NIL)
  KWD_IF("or", TOKEN_OR)
  KWD_IF("print", TOKEN_PRINT)
  KWD_IF("return", TOKEN_RETURN)
  KWD_IF("super", TOKEN_SUPER)
  KWD_IF("true", TOKEN_TRUE)
  KWD_IF("var", TOKEN_VAR)
  KWD_IF("while", TOKEN_WHILE)

#undef KWD

  return TOKEN_IDENTIFIER;
}

static Token scan_identifier(Scanner *scanner) {
  while (is_alpha(peek(scanner)) || is_digit(peek(scanner))) {
    advance(scanner);
  }

  TokenType type = keyword_or_identifier(scanner);
  return make_token(scanner, type);
}

Token scanner_next(Scanner *scanner) {
  skip_ignored(scanner);

  scanner->start = scanner->current;

  if (at_eof(scanner)) {
    return make_token(scanner, TOKEN_EOF);
  }

  char c = advance(scanner);

  if (is_alpha(c)) {
    return scan_identifier(scanner);
  }
  if (is_digit(c)) {
    return scan_number(scanner);
  }

  switch (c) {
  // One character, no lookahead
  case '(':
    return make_token(scanner, TOKEN_LEFT_PAREN);
  case ')':
    return make_token(scanner, TOKEN_RIGHT_PAREN);
  case '{':
    return make_token(scanner, TOKEN_LEFT_BRACE);
  case '}':
    return make_token(scanner, TOKEN_RIGHT_BRACE);
  case ';':
    return make_token(scanner, TOKEN_SEMICOLON);
  case ',':
    return make_token(scanner, TOKEN_COMMA);
  case '.':
    return make_token(scanner, TOKEN_DOT);
  case '-':
    return make_token(scanner, TOKEN_MINUS);
  case '+':
    return make_token(scanner, TOKEN_PLUS);
  case '/':
    return make_token(scanner, TOKEN_SLASH);
  case '*':
    return make_token(scanner, TOKEN_STAR);

  // One or two characters, 1 lookahead
  case '!': {
    TokenType type = match(scanner, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG;
    return make_token(scanner, type);
  }
  case '=': {
    TokenType type = match(scanner, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL;
    return make_token(scanner, type);
  }
  case '<': {
    TokenType type = match(scanner, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS;
    return make_token(scanner, type);
  }
  case '>': {
    TokenType type = match(scanner, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER;
    return make_token(scanner, type);
  }

  case '"':
    return scan_string(scanner);

  default:
    return make_error(scanner, "unexpected character");
  }
}
