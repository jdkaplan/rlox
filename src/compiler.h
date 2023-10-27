#ifndef clox_compiler_h
#define clox_compiler_h

#include "chunk.h"
#include "object.h"
#include "table.h"

bool compile(const char *source, Chunk *chunk, Obj **objects, Table *strings);

#endif
