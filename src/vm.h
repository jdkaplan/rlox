#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX  (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjClosure *closure;
  uint8_t *ip;
  Value *slots;
} CallFrame;

struct Vm {
  CallFrame frames[FRAMES_MAX];
  unsigned int frame_count;

  Value stack[STACK_MAX];
  Value *stack_top;
  Table globals;

  Table strings;
  ObjUpvalue *open_upvalues;
  Obj *objects;

  unsigned int gc_pending_len;
  unsigned int gc_pending_cap;
  Obj **gc_pending_stack;

  size_t bytes_allocated;
  size_t next_gc;
};

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

void gc_mark_roots_vm(Vm *vm);

#endif
