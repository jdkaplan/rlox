#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "vec.h"

void chunk_init(Chunk *chunk) {
  VEC_INIT(chunk->code);
  VEC_INIT(chunk->constants);
  VEC_INIT(chunk->lines);
}

void chunk_free(Chunk *chunk) {
  VEC_FREE(chunk->code);

  // Free any object constants before dropping the allocation.
  for (unsigned int i = 0; i < VEC_LEN(chunk->constants); i++) {
    Value v = VEC_GET(chunk->constants, i);
    if (IS_OBJ(v)) {
      obj_free(AS_OBJ(v));
    }
  }
  VEC_FREE(chunk->constants);

  VEC_FREE(chunk->lines);
}

void chunk_write(Chunk *chunk, uint8_t byte, int line) {
  VEC_APPEND(chunk->code, byte);
  VEC_APPEND(chunk->lines, line);
}

uint8_t chunk_add_constant(Chunk *chunk, Value value) {
  unsigned int idx = VEC_LEN(chunk->constants);
  if (idx == UINT8_MAX) {
    printf("too many constants");
    exit(2);
  }

  VEC_APPEND(chunk->constants, value);

  return (uint8_t)(idx);
}
