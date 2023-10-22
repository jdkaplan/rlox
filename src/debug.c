#include <stdio.h>

#include "debug.h"

void disassemble_chunk(Chunk *chunk, const char *name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < VEC_LEN(chunk->code);) {
    offset = disassemble_instruction(chunk, offset);
  }
}

int simple_instruction(const char *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

int constant_instruction(const char *name, Chunk *chunk, int offset) {
  uint8_t constant = VEC_GET(chunk->code, offset + 1);
  printf("%-16s %4d '", name, constant);
  print_value(VEC_GET(chunk->constants, constant));
  printf("'\n");
  return offset + 23;
}

int disassemble_instruction(Chunk *chunk, int offset) {
  printf("%04d ", offset);

  if (offset > 0 &&
      VEC_GET(chunk->lines, offset) == VEC_GET(chunk->lines, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", VEC_GET(chunk->lines, offset));
  }

  uint8_t instruction = VEC_GET(chunk->code, offset);
  switch (instruction) {
  case OP_CONSTANT:
    return constant_instruction("OP_CONSTANT", chunk, offset);
  case OP_RETURN:
    return simple_instruction("OP_RETURN", offset);
  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
