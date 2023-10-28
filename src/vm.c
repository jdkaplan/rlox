#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "vm.h"

void vm_reset_stack(Vm *vm) { vm->stack_top = vm->stack; }

void vm_init(Vm *vm) {
  vm_reset_stack(vm);
  table_init(&vm->globals);
  table_init(&vm->strings);
  vm->objects = NULL;
}

void vm_free(Vm *vm) {
  table_free(&vm->globals);
  table_free(&vm->strings);
  free_objects(vm->objects);
}

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

ObjString *concatenate(Obj **objs, Table *strings, ObjString *a, ObjString *b) {
  size_t length = a->length + b->length;
  char *chars = ALLOCATE(char, length + 1);

  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  return str_take(objs, strings, chars, length);
}

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
#define READ_STRING()   (AS_STRING(READ_CONSTANT()))

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

    case OP_POP: {
      vm_pop(vm);
      break;
    }

    case OP_GET_GLOBAL: {
      ObjString *name = READ_STRING();
      Value value;
      if (!table_get(&vm->globals, name, &value)) {
        runtime_error(vm, "undefined variable: '%s'", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      vm_push(vm, value);
      break;
    }

    case OP_DEFINE_GLOBAL: {
      ObjString *name = READ_STRING();
      table_set(&vm->globals, name, vm_peek(vm, 0));
      vm_pop(vm);
      break;
    }

    case OP_SET_GLOBAL: {
      ObjString *name = READ_STRING();
      if (table_set(&vm->globals, name, vm_peek(vm, 0))) {
        // If this was a new key, that means the global wasn't actually defined
        // yet! Delete it back out and throw the runtime error.
        table_delete(&vm->globals, name);
        runtime_error(vm, "undefined variable: '%s'", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
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
      if (IS_STRING(vm_peek(vm, 0)) && IS_STRING(vm_peek(vm, 1))) {
        ObjString *b = AS_STRING(vm_pop(vm));
        ObjString *a = AS_STRING(vm_pop(vm));
        ObjString *result = concatenate(&vm->objects, &vm->strings, a, b);
        vm_push(vm, V_OBJ(result));
      } else if (IS_NUMBER(vm_peek(vm, 0)) && IS_NUMBER(vm_peek(vm, 1))) {
        double b = AS_NUMBER(vm_pop(vm));
        double a = AS_NUMBER(vm_pop(vm));
        vm_push(vm, V_NUMBER(a + b));
      } else {
        runtime_error(vm, "operands must be two numbers or two strings");
        return INTERPRET_RUNTIME_ERROR;
      }
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

    case OP_PRINT: {
      print_value(vm_pop(vm));
      printf("\n");
      break;
    }

    case OP_RETURN: {
      return INTERPRET_OK;
    }

    default: {
      printf("unknown opcode: %x", instruction);
      exit(1);
    }
    }
  }

#undef READ_STRING
#undef READ_CONSTANT
#undef READ_BYTE
}

InterpretResult vm_interpret(Vm *vm, const char *source) {
  Chunk chunk;
  chunk_init(&chunk);

  if (!compile(source, &chunk, &vm->objects, &vm->strings)) {
    chunk_free(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm->chunk = &chunk;
  vm->ip = vm->chunk->code.items;

  InterpretResult result = vm_run(vm);

  chunk_free(&chunk);
  return result;
}
