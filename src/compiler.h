#ifndef clox_compiler_h
#define clox_compiler_h

#include "chunk.h"
#include "object.h"
#include "scanner.h"
#include "table.h"

typedef struct {
  Token name;
  int depth;
} Local;

typedef struct {
  Local locals[UINT8_COUNT];
  int local_count;
  int scope_depth;
} Compiler;

typedef struct {
  Token current;
  Token previous;

  Chunk *chunk;
  Scanner *scanner;
  Compiler *compiler;

  bool had_error;
  bool panicking;

  // Borrowed from VM
  Obj **objects;
  Table *strings;
  // end borrow
} Parser;

bool compile(const char *source, Chunk *chunk, Obj **objects, Table *strings);

#endif
