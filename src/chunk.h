#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"
#include "vec.h"

#include "rlox.h"

void chunk_init(Chunk *chunk);
void chunk_free(Gc gc, Chunk *chunk);
void chunk_write(Gc gc, Chunk *chunk, uint8_t byte, int line);

uint8_t chunk_add_constant(Gc gc, Chunk *chunk, Value value);

#endif
