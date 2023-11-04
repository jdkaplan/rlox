#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"
#include "vec.h"

typedef enum {
  OP_CONSTANT,

  OP_NIL,
  OP_TRUE,
  OP_FALSE,

  OP_POP,

  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_DEFINE_GLOBAL,
  OP_GET_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,

  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  // TODO: The other three _are_ needed to handle NaN properly.

  OP_NOT,

  OP_NEG,

  OP_PRINT,

  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LOOP,

  OP_CALL,
  OP_CLOSURE,
  OP_CLOSE_UPVALUE,
  OP_RETURN,

  OP_CLASS,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
} Opcode;

typedef VEC(uint8_t) Bytecode;
typedef VEC(int) Lines;

typedef struct {
  Bytecode code;
  Values constants;
  Lines lines;
} Chunk;

void chunk_init(Chunk *chunk);
void chunk_free(Gc gc, Chunk *chunk);
void chunk_write(Gc gc, Chunk *chunk, uint8_t byte, int line);

uint8_t chunk_add_constant(Gc gc, Chunk *chunk, Value value);

#endif
