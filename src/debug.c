#include <stdio.h>

#include "debug.h"

void disassemble_chunk(Chunk *chunk, const char *name) {
  printf("== %s ==\n", name);

  for (unsigned int offset = 0; offset < VEC_LEN(chunk->code);) {
    offset = disassemble_instruction(chunk, offset);
  }
}

unsigned int simple_instruction(const char *name, unsigned int offset) {
  printf("%s\n", name);
  return offset + 1;
}

unsigned int constant_instruction(const char *name, Chunk *chunk,
                                  unsigned int offset) {
  uint8_t constant = VEC_GET(chunk->code, offset + 1);
  printf("%-16s %4d '", name, constant);
  print_value(VEC_GET(chunk->constants, constant));
  printf("'\n");
  return offset + 2;
}

unsigned int disassemble_instruction(Chunk *chunk, unsigned int offset) {
  printf("%04d ", offset);

  if (offset > 0 &&
      VEC_GET(chunk->lines, offset) == VEC_GET(chunk->lines, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", VEC_GET(chunk->lines, offset));
  }

  Opcode instruction = (Opcode)VEC_GET(chunk->code, offset);
  switch (instruction) {
  case OP_CONSTANT:
    return constant_instruction("OP_CONSTANT", chunk, offset);

  case OP_NEG:
    return simple_instruction("OP_NEGATE", offset);

  case OP_ADD:
    return simple_instruction("OP_ADD", offset);
  case OP_SUB:
    return simple_instruction("OP_SUB", offset);
  case OP_MUL:
    return simple_instruction("OP_MUL", offset);
  case OP_DIV:
    return simple_instruction("OP_DIV", offset);

  case OP_RETURN:
    return simple_instruction("OP_RETURN", offset);

  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
