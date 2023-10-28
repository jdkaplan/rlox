#include <stdio.h>

#include "debug.h"

void disassemble_chunk(Chunk *chunk, const char *name) {
  printf("== %s ==\n", name);

  for (unsigned int offset = 0; offset < VEC_LEN(chunk->code);) {
    offset = disassemble_instruction(chunk, offset);
  }
}

static unsigned int simple_instruction(const char *name, unsigned int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static unsigned int constant_instruction(const char *name, Chunk *chunk,
                                         unsigned int offset) {
  uint8_t constant = VEC_GET(chunk->code, offset + 1);
  printf("%-16s %4d '", name, constant);
  print_value(VEC_GET(chunk->constants, constant));
  printf("'\n");
  return offset + 2;
}

static unsigned int byte_instruction(const char *name, Chunk *chunk,
                                     unsigned int offset) {
  uint8_t slot = VEC_GET(chunk->code, offset + 1);
  printf("%-16s %4d\n", name, slot);
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

#define CONSTANT(op)                                                           \
  case op:                                                                     \
    return constant_instruction(#op, chunk, offset);

    CONSTANT(OP_CONSTANT)
    CONSTANT(OP_GET_GLOBAL)
    CONSTANT(OP_DEFINE_GLOBAL)
    CONSTANT(OP_SET_GLOBAL)

#undef CONSTANT

#define BYTE(op)                                                               \
  case op:                                                                     \
    return byte_instruction(#op, chunk, offset);

    BYTE(OP_GET_LOCAL)
    BYTE(OP_SET_LOCAL)

#undef BYTE

#define SIMPLE(op)                                                             \
  case op:                                                                     \
    return simple_instruction(#op, offset);

    SIMPLE(OP_NIL)
    SIMPLE(OP_TRUE)
    SIMPLE(OP_FALSE)

    SIMPLE(OP_POP)

    SIMPLE(OP_EQUAL)
    SIMPLE(OP_GREATER)
    SIMPLE(OP_LESS)

    SIMPLE(OP_NOT)

    SIMPLE(OP_NEG)

    SIMPLE(OP_ADD)
    SIMPLE(OP_SUB)
    SIMPLE(OP_MUL)
    SIMPLE(OP_DIV)

    SIMPLE(OP_PRINT)

    SIMPLE(OP_RETURN)

#undef SIMPLE

  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
