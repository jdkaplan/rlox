#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "debug.h"
#include "vm.h"

void vm_reset_stack(Vm *vm) { vm->stack_top = vm->stack; }

void vm_init(Vm *vm) { vm_reset_stack(vm); }

void vm_free(Vm *vm) {}

void vm_push(Vm *vm, Value value) {
  *vm->stack_top = value;
  vm->stack_top++;
}

Value vm_pop(Vm *vm) {
  vm->stack_top--;
  return *vm->stack_top;
}

InterpretResult vm_run(Vm *vm) {
#define READ_BYTE() ((Opcode)(*vm->ip++))
#define READ_CONSTANT() (VEC_GET(vm->chunk->constants, READ_BYTE()))

#define BINARY_OP(op)                                                          \
  do {                                                                         \
    double b = vm_pop(vm);                                                     \
    double a = vm_pop(vm);                                                     \
    vm_push(vm, a op b);                                                       \
  } while (0)

  for (;;) {
    Opcode instruction;

#ifdef DEBUG_TRACE_EXECUTION
    printf("# ");
    for (Value *slot = vm->stack; slot < vm->stack_top; slot++) {
      printf("[ ");
      print_value(*slot);
      printf(" ]");
    }
    printf("\n");

    printf("# ");
    disassemble_instruction(vm->chunk,
                            (unsigned int)(vm->ip - vm->chunk->code.items));
#endif

    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      vm_push(vm, constant);
      break;
    }

    case OP_NEG: {
      Value a = vm_pop(vm);
      vm_push(vm, -a);
      break;
    }

    case OP_ADD: {
      BINARY_OP(+);
      break;
    }
    case OP_SUB: {
      BINARY_OP(-);
      break;
    }
    case OP_MUL: {
      BINARY_OP(*);
      break;
    }
    case OP_DIV: {
      BINARY_OP(/);
      break;
    }

    case OP_RETURN: {
      Value v = vm_pop(vm);
      print_value(v);
      printf("\n");
      return INTERPRET_OK;
    }

    default: {
      printf("unknown opcode: %x", instruction);
      exit(1);
    }
    }
  }

#undef READ_CONSTANT
#undef READ_BYTE
}

InterpretResult vm_interpret(Vm *vm, const char *source) {
  compile(source);
  return INTERPRET_OK;
}
