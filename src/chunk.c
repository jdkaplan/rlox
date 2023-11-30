#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "vec.h"
#include "vm.h"

void chunk_init(Chunk *chunk) {
  VEC_INIT(chunk->code);
  VEC_INIT(chunk->constants);
  VEC_INIT(chunk->lines);
}

void chunk_free(Gc gc, Chunk *chunk) {
  VEC_FREE(gc, chunk->code);
  VEC_FREE(gc, chunk->constants);
  VEC_FREE(gc, chunk->lines);
}

void chunk_write(Gc gc, Chunk *chunk, uint8_t byte, int line) {
  VEC_APPEND(gc, chunk->code, byte);
  VEC_APPEND(gc, chunk->lines, line);
}

uint8_t chunk_add_constant(Gc gc, Chunk *chunk, Value value) {
  unsigned int idx = VEC_LEN(chunk->constants);
  // if (idx == UINT8_MAX) {
  //   printf("Too many constants in one chunk.");
  //   exit(2);
  // }

  // GC: Ensure `value` is reachable temporarily in case resizing the constants
  // array triggers garbage collection.
  vm_push(gc.vm, value);
  VEC_APPEND(gc, chunk->constants, value);
  vm_pop(gc.vm);

  return (uint8_t)(idx);
}
