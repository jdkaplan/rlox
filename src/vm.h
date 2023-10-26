#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
  Chunk *chunk;
  uint8_t *ip;

  Value stack[STACK_MAX];
  Value *stack_top;

  Obj *objects;
} Vm;

void vm_init(Vm *vm);
void vm_free(Vm *vm);

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

InterpretResult vm_interpret(Vm *vm, const char *source);
void vm_push(Vm *vm, Value value);
Value vm_pop(Vm *vm);

#endif
