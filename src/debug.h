#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassemble_chunk(Chunk *chunk, const char *name);
unsigned int disassemble_instruction(Chunk *chunk, unsigned int offset);

#endif
