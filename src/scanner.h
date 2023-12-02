#ifndef clox_scanner_h
#define clox_scanner_h

#include "rlox.h"

void scanner_init(Scanner *scanner, const char *source);

Token scanner_next(Scanner *scanner);

void dbg_token(Token token);

#endif
