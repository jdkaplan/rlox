#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#include "rlox.h"

void vm_init(Vm *vm);
void vm_free(Vm *vm);

InterpretResult vm_interpret(Vm *vm, const char *source);
void vm_push(Vm *vm, Value value);
Value vm_pop(Vm *vm);

void gc_mark_roots_vm(Vm *vm);

#endif
