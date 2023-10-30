#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "chunk.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "value.h"
#include "vm.h"

static Value clock_native(unsigned int UNUSED(argc), Value *UNUSED(args)) {
  return V_NUMBER((double)(clock()) / CLOCKS_PER_SEC);
}

static void define_native(Vm *vm, const char *name, NativeFn fn);

static void vm_reset_stack(Vm *vm) {
  vm->stack_top = vm->stack;
  vm->frame_count = 0;
}

void vm_init(Vm *vm) {
  vm_reset_stack(vm);
  table_init(&vm->globals);
  table_init(&vm->strings);
  vm->objects = NULL;

  define_native(vm, "clock", clock_native);
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

static Value vm_peek(Vm *vm, int offset) { return vm->stack_top[-1 - offset]; }

static void runtime_error(Vm *vm, const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = (int)(vm->frame_count) - 1; i >= 0; i--) {
    CallFrame *frame = &vm->frames[i];
    ObjFunction *fun = frame->function;

    // The ip has already moved past the instruction that failed, so subtract
    // one extra.
    uint8_t instruction = (uint8_t)(frame->ip - fun->chunk.code.items - 1);

    fprintf(stderr, "[line %d] in ", VEC_GET(fun->chunk.lines, instruction));
    if (fun->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", fun->name->chars);
    }
  }

  CallFrame *frame = &vm->frames[vm->frame_count - 1];
  // ip has already moved _past_ the instruction we tried to execute.
  uint8_t instruction =
      (uint8_t)(frame->ip - frame->function->chunk.code.items - 1);
  int line = VEC_GET(frame->function->chunk.lines, instruction);
  fprintf(stderr, "[line %d] in script\n", line);
  vm_reset_stack(vm);
}

static void define_native(Vm *vm, const char *name, NativeFn fn) {
  vm_push(vm, V_OBJ(str_clone(&vm->objects, &vm->strings, name, strlen(name))));
  vm_push(vm, V_OBJ(native_new(&vm->objects, fn)));
  table_set(&vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
  vm_pop(vm);
  vm_pop(vm);
}

static bool call(Vm *vm, ObjFunction *fun, unsigned int argc) {
  if (argc != fun->arity) {
    runtime_error(vm, "expected %d arguments but got %d", fun->arity, argc);
    return false;
  }

  if (vm->frame_count == FRAMES_MAX) {
    runtime_error(vm, "stack overflow");
    return false;
  }

  CallFrame *frame = &vm->frames[vm->frame_count++];
  frame->function = fun;
  frame->ip = fun->chunk.code.items;
  // Subtract an extra slot for stack slot zero (which contains the caller).
  frame->slots = vm->stack_top - argc - 1;
  return true;
}

static bool call_value(Vm *vm, Value callee, unsigned int argc) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case O_FUNCTION:
      return call(vm, AS_FUNCTION(callee), argc);
    case O_NATIVE: {
      NativeFn fn = AS_NATIVE(callee);
      Value res = fn(argc, vm->stack_top - argc);
      vm->stack_top -= argc + 1;
      vm_push(vm, res);
      return true;
    }
    default:
      break; // Not callable
    }
  }
  runtime_error(vm, "value was not callable");
  return false;
}

static bool is_falsey(Value v) {
  return IS_NIL(v) || (IS_BOOL(v) && !AS_BOOL(v));
}

static ObjString *concatenate(Obj **objs, Table *strings, ObjString *a,
                              ObjString *b) {
  size_t length = a->length + b->length;
  char *chars = ALLOCATE(char, length + 1);

  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  return str_take(objs, strings, chars, length);
}

static InterpretResult vm_run(Vm *vm) {
  CallFrame *frame = &vm->frames[vm->frame_count - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT()                                                           \
  (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (VEC_GET(frame->function->chunk.constants, READ_BYTE()))
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
    disassemble_instruction(
        &frame->function->chunk,
        (unsigned int)(frame->ip - frame->function->chunk.code.items));
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

    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      vm_push(vm, frame->slots[slot]);
      break;
    }

    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      frame->slots[slot] = vm_peek(vm, 0);
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

    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      frame->ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (is_falsey(vm_peek(vm, 0))) {
        frame->ip += offset;
      }
      break;
    }
    case OP_LOOP: {
      uint16_t offset = READ_SHORT();
      frame->ip -= offset;
      break;
    }

    case OP_CALL: {
      unsigned int arg_count = READ_BYTE();
      if (!call_value(vm, vm_peek(vm, (int)(arg_count)), arg_count)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm->frames[vm->frame_count - 1];
      break;
    }

    case OP_RETURN: {
      Value res = vm_pop(vm);

      vm->frame_count--;
      if (vm->frame_count == 0) {
        vm_pop(vm);
        return INTERPRET_OK;
      }

      vm->stack_top = frame->slots;
      vm_push(vm, res);
      frame = &vm->frames[vm->frame_count - 1];
      break;
    }

    default: {
      printf("unknown opcode: %x", instruction);
      exit(1);
    }
    }
  }

#undef READ_STRING
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_BYTE
}

InterpretResult vm_interpret(Vm *vm, const char *source) {
  ObjFunction *function = compile(source, &vm->objects, &vm->strings);
  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  vm_push(vm, V_OBJ(function));
  call(vm, function, 0);

  return vm_run(vm);
}
