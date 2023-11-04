#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(gc, type, object_type)                                    \
  (type *)allocate_obj(gc, sizeof(type), object_type)

static Obj *allocate_obj(Gc gc, size_t size, ObjType type) {
  Obj *object = (Obj *)(reallocate(gc, NULL, 0, size));
  object->type = type;
  object->is_marked = false;

  object->next = gc.vm->objects;
  gc.vm->objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for type=%d\n", (void *)(object), size, type);
#endif

  return object;
}

ObjBoundMethod *bound_method_new(Gc gc, Value receiver, ObjClosure *method) {
  ObjBoundMethod *bound = ALLOCATE_OBJ(gc, ObjBoundMethod, O_BOUND_METHOD);
  bound->receiver = receiver;
  bound->method = method;
  return bound;
}

ObjClass *class_new(Gc gc, ObjString *name) {
  ObjClass *klass = ALLOCATE_OBJ(gc, ObjClass, O_CLASS);
  klass->name = name;
  table_init(&klass->methods);
  return klass;
}

ObjClosure *closure_new(Gc gc, ObjFunction *function) {
  // TODO: This is a Vec
  ObjUpvalue **upvalues =
      ALLOCATE(gc, ObjUpvalue *, (size_t)(function->upvalue_count));
  for (int i = 0; i < function->upvalue_count; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure *closure = ALLOCATE_OBJ(gc, ObjClosure, O_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalue_count = function->upvalue_count;
  return closure;
}

ObjFunction *function_new(Gc gc) {
  ObjFunction *fun = ALLOCATE_OBJ(gc, ObjFunction, O_FUNCTION);
  fun->arity = 0;
  fun->upvalue_count = 0;
  fun->name = NULL;
  chunk_init(&fun->chunk);
  return fun;
};

ObjInstance *instance_new(Gc gc, ObjClass *klass) {
  ObjInstance *inst = ALLOCATE_OBJ(gc, ObjInstance, O_INSTANCE);
  inst->klass = klass;
  table_init(&inst->fields);
  return inst;
}

ObjNative *native_new(Gc gc, NativeFn fn) {
  ObjNative *nat = ALLOCATE_OBJ(gc, ObjNative, O_NATIVE);
  nat->fn = fn;
  return nat;
}

ObjUpvalue *upvalue_new(Gc gc, Value *slot) {
  ObjUpvalue *upvalue = ALLOCATE_OBJ(gc, ObjUpvalue, O_UPVALUE);
  upvalue->location = slot;
  upvalue->closed = V_NIL;
  upvalue->next = NULL;
  return upvalue;
}

static ObjString *allocate_string(Gc gc, char *chars, size_t length,
                                  uint32_t hash) {
  ObjString *str = ALLOCATE_OBJ(gc, ObjString, O_STRING);
  str->length = length;
  str->chars = chars;
  str->hash = hash;

  // GC: Ensure `str` is reachable temporarily in case resizing the table
  // triggers garbage collection.
  vm_push(gc.vm, V_OBJ(str));
  table_set(gc, &gc.vm->strings, str, V_NIL);
  vm_pop(gc.vm);

  return str;
}

static uint32_t str_hash(const char *key, size_t length) {
  uint32_t hash = 2166136261u;
  for (unsigned int i = 0; i < length; i++) {
    hash ^= (uint8_t)(key[i]);
    hash *= 16777619;
  }
  return hash;
}

ObjString *str_take(Gc gc, char *chars, size_t length) {
  uint32_t hash = str_hash(chars, length);

  ObjString *interned = table_find_string(&gc.vm->strings, chars, length, hash);
  if (interned != NULL) {
    // This takes ownership of `chars`, so free the memory if it's not going to
    // be stored anywhere.
    FREE_ARRAY(gc, char, chars, length + 1);
    return interned;
  }

  return allocate_string(gc, chars, length, hash);
}

ObjString *str_clone(Gc gc, const char *chars, size_t length) {
  uint32_t hash = str_hash(chars, length);

  ObjString *interned = table_find_string(&gc.vm->strings, chars, length, hash);
  if (interned != NULL) {
    return interned;
  }

  char *heap_chars = ALLOCATE(gc, char, length + 1);
  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return allocate_string(gc, heap_chars, length, hash);
}

void print_function(ObjFunction *fun) {
  if (fun->name == NULL) {
    printf("<script>");
  } else {
    printf("<fn %s>", fun->name->chars);
  }
}

void print_object(Value value) {
  switch (OBJ_TYPE(value)) {
  case O_BOUND_METHOD: {
    print_function(AS_BOUND_METHOD(value)->method->function);
    break;
  }
  case O_CLASS: {
    printf("<class %s>", AS_CLASS(value)->name->chars);
    break;
  }
  case O_CLOSURE: {
    print_function(AS_CLOSURE(value)->function);
    break;
  }
  case O_FUNCTION: {
    print_function(AS_FUNCTION(value));
    break;
  }
  case O_INSTANCE: {
    printf("<instance of %s>", AS_INSTANCE(value)->klass->name->chars);
    break;
  }
  case O_NATIVE: {
    printf("<native fn>");
    break;
  }
  case O_STRING: {
    printf("%s", AS_CSTRING(value));
    break;
  }
  case O_UPVALUE: {
    printf("upvalue");
  }
  }
}

void free_objects(Gc gc, Obj *root) {
  while (root != NULL) {
    Obj *next = root->next;
    obj_free(gc, root);
    root = next;
  }
}

void obj_free(Gc gc, Obj *obj) {
#ifdef DEBUG_LOG_GC
  printf("%p free type=%d value=", (void *)(obj), obj->type);
  print_value(V_OBJ(obj));
  printf("\n");
#endif

  switch (obj->type) {
  case O_BOUND_METHOD: {
    FREE(gc, ObjBoundMethod, obj);
    break;
  }
  case O_CLASS: {
    ObjClass *klass = (ObjClass *)(obj);
    table_free(gc, &klass->methods);
    FREE(gc, ObjClass, obj);
    break;
  }
  case O_CLOSURE: {
    FREE(gc, ObjClosure, obj);
    break;
  }
  case O_FUNCTION: {
    ObjFunction *function = (ObjFunction *)(obj);
    chunk_free(gc, &function->chunk);
    FREE(gc, ObjFunction, obj);
    break;
  }
  case O_INSTANCE: {
    ObjInstance *inst = (ObjInstance *)(obj);
    table_free(gc, &inst->fields);
    FREE(gc, ObjInstance, obj);
    break;
  }
  case O_NATIVE: {
    FREE(gc, ObjNative, obj);
    break;
  }
  case O_STRING: {
    ObjString *str = (ObjString *)(obj);
    FREE_ARRAY(gc, char, str->chars, str->length + 1);
    FREE(gc, ObjString, obj);
    break;
  }
  case O_UPVALUE: {
    FREE(gc, ObjUpvalue, obj);
    break;
  }
  }
}
