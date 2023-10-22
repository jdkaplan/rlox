#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"
#include "vec.h"

typedef enum {
  OP_CONSTANT,
  OP_RETURN,
} Opcode;

typedef VEC(uint8_t) Bytecode;
typedef VEC(int) Lines;

typedef struct {
  Bytecode code;
  Values constants;
  Lines lines;
} Chunk;

void chunk_init(Chunk *chunk);
void chunk_free(Chunk *chunk);
void chunk_write(Chunk *chunk, uint8_t byte, int line);

int chunk_add_constant(Chunk *chunk, Value value);

#endif
