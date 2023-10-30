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

typedef enum {
  MODE_FUNCTION,
  MODE_SCRIPT,
} FunctionMode;

typedef struct Compiler Compiler;

struct Compiler {
  Compiler *enclosing;
  ObjFunction *function;
  FunctionMode mode;

  Local locals[UINT8_COUNT];
  int local_count;
  int scope_depth;
};

typedef struct {
  Token current;
  Token previous;

  Scanner *scanner;
  Compiler *compiler;

  bool had_error;
  bool panicking;

  // Borrowed from VM
  Obj **objects;
  Table *strings;
  // end borrow
} Parser;

ObjFunction *compile(const char *source, Obj **objects, Table *strings);

#endif
