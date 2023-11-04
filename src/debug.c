#include <stdio.h>

#include "debug.h"
#include "object.h"

void disassemble_chunk(Chunk *chunk, const char *name) {
  printf("== %s ==\n", name);

  for (unsigned int i = 0; i < VEC_LEN(chunk->constants); i++) {
    printf("CONSTANT %4d = ", i);
    print_value(VEC_GET(chunk->constants, i));
    printf("\n");
  }

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

static unsigned int jump_instruction(const char *name, int sign, Chunk *chunk,
                                     unsigned int offset) {

  uint16_t jump = (uint16_t)(VEC_GET(chunk->code, offset + 1) << 8);
  jump |= VEC_GET(chunk->code, offset + 2);

  printf("%-16s %4d -> %d\n", name, offset, (int)offset + 3 + sign * jump);
  return offset + 3;
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
    CONSTANT(OP_CLASS)
    CONSTANT(OP_METHOD)
    CONSTANT(OP_DEFINE_GLOBAL)
    CONSTANT(OP_GET_GLOBAL)
    CONSTANT(OP_SET_GLOBAL)
    CONSTANT(OP_GET_PROPERTY)
    CONSTANT(OP_SET_PROPERTY)

#undef CONSTANT

#define BYTE(op)                                                               \
  case op:                                                                     \
    return byte_instruction(#op, chunk, offset);

    BYTE(OP_GET_LOCAL)
    BYTE(OP_SET_LOCAL)
    BYTE(OP_GET_UPVALUE)
    BYTE(OP_SET_UPVALUE)
    BYTE(OP_CALL)

#undef BYTE

#define JUMP(op, sign)                                                         \
  case op:                                                                     \
    return jump_instruction(#op, sign, chunk, offset);

    JUMP(OP_JUMP, +1)
    JUMP(OP_JUMP_IF_FALSE, +1)
    JUMP(OP_LOOP, -1)

#undef JUMP

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

    SIMPLE(OP_CLOSE_UPVALUE)
    SIMPLE(OP_RETURN)

#undef SIMPLE

  case OP_INVOKE: {
    uint8_t constant = VEC_GET(chunk->code, offset + 1);
    uint8_t argc = VEC_GET(chunk->code, offset + 2);
    printf("%-16s (%d args) %4d ", "OP_INVOKE", argc, constant);
    print_value(VEC_GET(chunk->constants, constant));
    printf("\n");
    return offset + 3;
  }

  case OP_CLOSURE: {
    offset++;
    uint8_t constant = VEC_GET(chunk->code, offset++);
    printf("%-16s %4d ", "OP_CLOSURE", constant);
    print_value(VEC_GET(chunk->constants, constant));
    printf("\n");

    ObjFunction *function = AS_FUNCTION(VEC_GET(chunk->constants, constant));
    for (int j = 0; j < function->upvalue_count; j++) {
      int is_local = VEC_GET(chunk->code, offset++);
      int index = VEC_GET(chunk->code, offset++);
      printf("%04d      |                     %s %d\n", offset - 2,
             is_local ? "local" : "upvalue", index);
    }

    return offset;
  }

  default:
    printf("Unknown opcode %d\n", instruction);
    return offset + 1;
  }
}
