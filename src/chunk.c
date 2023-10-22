#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vec.h"

void chunk_init(Chunk *chunk) {
  VEC_INIT(chunk->code);
  VEC_INIT(chunk->constants);
  VEC_INIT(chunk->lines);
}

void chunk_free(Chunk *chunk) {
  VEC_FREE(chunk->code);
  VEC_FREE(chunk->constants);
  VEC_FREE(chunk->lines);
}

void chunk_write(Chunk *chunk, uint8_t byte, int line) {
  VEC_APPEND(chunk->code, byte);
  VEC_APPEND(chunk->lines, line);
}

int chunk_add_constant(Chunk *chunk, Value value) {
  VEC_APPEND(chunk->constants, value);
  return VEC_LEN(chunk->constants) - 1;
}
