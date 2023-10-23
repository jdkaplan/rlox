#include <stdio.h>

#include "common.h"

#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char *argv[]) {
  Chunk chunk;
  chunk_init(&chunk);

  uint8_t constant = chunk_add_constant(&chunk, 1.2);
  chunk_write(&chunk, OP_CONSTANT, 123);
  chunk_write(&chunk, constant, 123);

  constant = chunk_add_constant(&chunk, 3.4);
  chunk_write(&chunk, OP_CONSTANT, 123);
  chunk_write(&chunk, constant, 123);

  chunk_write(&chunk, OP_ADD, 123);

  constant = chunk_add_constant(&chunk, 5.6);
  chunk_write(&chunk, OP_CONSTANT, 123);
  chunk_write(&chunk, constant, 123);

  chunk_write(&chunk, OP_DIV, 123);

  chunk_write(&chunk, OP_NEG, 123);

  chunk_write(&chunk, OP_RETURN, 123);

  disassemble_chunk(&chunk, "<test>");

  printf("---\n");

  Vm vm;
  vm_init(&vm);
  vm_interpret(&vm, &chunk);

  vm_free(&vm);
  chunk_free(&chunk);
  return 0;
}
