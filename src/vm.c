#include <stdarg.h>
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

Value vm_peek(Vm *vm, int offset) { return vm->stack_top[-1 - offset]; }

bool is_falsey(Value v) { return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v)); }

void runtime_error(Vm *vm, const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  // ip has already moved _past_ the instruction we tried to execute.
  unsigned int instruction = (unsigned int)(vm->ip - vm->chunk->code.items - 1);
  int line = VEC_GET(vm->chunk->lines, instruction);
  fprintf(stderr, "[line %d] in script\n", line);
  vm_reset_stack(vm);
}

InterpretResult vm_run(Vm *vm) {
#define READ_BYTE()     ((Opcode)(*vm->ip++))
#define READ_CONSTANT() (VEC_GET(vm->chunk->constants, READ_BYTE()))

#define BINARY_OP(vm, vtype, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(vm_peek(vm, 0)) || !IS_NUMBER(vm_peek(vm, 1))) {            \
      runtime_error(vm, "operands must be numbers");                           \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double b = AS_NUMBER(vm_pop(vm));                                          \
    double a = AS_NUMBER(vm_pop(vm));                                          \
    vm_push(vm, vtype(a op b));                                                \
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

    case OP_NIL: {
      vm_push(vm, V_NIL);
      break;
    }
    case OP_TRUE: {
      vm_push(vm, V_BOOL(true));
      break;
    }
    case OP_FALSE: {
      vm_push(vm, V_BOOL(false));
      break;
    }

    case OP_EQUAL: {
      Value b = vm_pop(vm);
      Value a = vm_pop(vm);
      vm_push(vm, V_BOOL(value_eq(a, b)));
      break;
    }
    case OP_GREATER: {
      BINARY_OP(vm, V_BOOL, >);
      break;
    }
    case OP_LESS: {
      BINARY_OP(vm, V_BOOL, <);
      break;
    }

    case OP_NOT: {
      Value a = vm_pop(vm);
      vm_push(vm, V_BOOL(is_falsey(a)));
      break;
    }

    case OP_NEG: {
      if (!IS_NUMBER(vm_peek(vm, 0))) {
        runtime_error(vm, "operand must be a number");
        return INTERPRET_RUNTIME_ERROR;
      }
      double a = AS_NUMBER(vm_pop(vm));
      vm_push(vm, V_NUMBER(-a));
      break;
    }

    case OP_ADD: {
      BINARY_OP(vm, V_NUMBER, +);
      break;
    }
    case OP_SUB: {
      BINARY_OP(vm, V_NUMBER, -);
      break;
    }
    case OP_MUL: {
      BINARY_OP(vm, V_NUMBER, *);
      break;
    }
    case OP_DIV: {
      BINARY_OP(vm, V_NUMBER, /);
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
  Chunk chunk;
  chunk_init(&chunk);

  if (!compile(source, &chunk)) {
    chunk_free(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm->chunk = &chunk;
  vm->ip = vm->chunk->code.items;

  InterpretResult result = vm_run(vm);

  chunk_free(&chunk);
  return result;
}
