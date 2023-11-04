#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

static void gc_mark_obj(Gc gc, Obj *obj) {
  if (obj == NULL) {
    return;
  }
  if (obj->is_marked) {
    return;
  }

#ifdef DEBUG_LOG_GC
  printf("%p mark type=%d ", (void *)(obj), obj->type);
  print_value(V_OBJ(obj));
  printf("\n");
#endif

  obj->is_marked = true;

  Vm *vm = gc.vm;

  // Don't use `reallocate` or any of its wrappers to avoid triggering recursive
  // garbage collection.
  if (vm->gc_pending_len + 1 >= vm->gc_pending_cap) {
    vm->gc_pending_cap = GROW_CAP(vm->gc_pending_cap);
    vm->gc_pending_stack = (Obj **)(realloc(
        vm->gc_pending_stack, sizeof(Obj *) * vm->gc_pending_cap));

    if (vm->gc_pending_stack == NULL) {
      printf("not enough memory available to collect garbage");
      exit(1);
    }
  }

  vm->gc_pending_stack[vm->gc_pending_len++] = obj;
}

static void gc_mark_value(Gc gc, Value value) {
  if (IS_OBJ(value)) {
    gc_mark_obj(gc, AS_OBJ(value));
  }
}

static void gc_mark_values(Gc gc, Values vec) {
#ifdef DEBUG_LOG_GC
  printf("mark %d value(s)\n", VEC_LEN(vec));
#endif

  for (unsigned int i = 0; i < VEC_LEN(vec); i++) {
    gc_mark_value(gc, VEC_GET(vec, i));
  }
}

static void gc_mark_table(Gc gc, Table *table) {
  for (unsigned int i = 0; i < table->cap; i++) {
    Entry *entry = &table->entries[i];
    gc_mark_obj(gc, (Obj *)(entry->key));
    gc_mark_value(gc, entry->value);
  }
}

static void gc_mark_roots(Gc gc) {
  for (Value *slot = gc.vm->stack; slot < gc.vm->stack_top; slot++) {
    gc_mark_value(gc, *slot);
  }

  for (unsigned int i = 0; i < gc.vm->frame_count; i++) {
    gc_mark_obj(gc, (Obj *)(gc.vm->frames[i].closure));
  }

  for (ObjUpvalue *upvalue = gc.vm->open_upvalues; upvalue != NULL;
       upvalue = upvalue->next) {
    gc_mark_obj(gc, (Obj *)(upvalue));
  }

  gc_mark_table(gc, &gc.vm->globals);

  gc_mark_obj(gc, (Obj *)(gc.vm->init_string));

  Compiler *compiler = gc.compiler;
  while (compiler != NULL) {
    gc_mark_obj(gc, (Obj *)(compiler->function));
    compiler = compiler->enclosing;
  }
}

static void gc_expand_obj(Gc gc, Obj *obj) {
#ifdef DEBUG_LOG_GC
  printf("%p expand type=%d ", (void *)(obj), obj->type);
  print_value(V_OBJ(obj));
  printf("\n");
#endif

  switch (obj->type) {
  case O_BOUND_METHOD: {
    ObjBoundMethod *bound = (ObjBoundMethod *)(obj);
    gc_mark_value(gc, bound->receiver);
    gc_mark_obj(gc, (Obj *)(bound->method));
    break;
  }
  case O_CLASS: {
    ObjClass *klass = (ObjClass *)(obj);
    gc_mark_obj(gc, (Obj *)(klass->name));
    gc_mark_table(gc, &klass->methods);
    break;
  }

  case O_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)(obj);
    gc_mark_obj(gc, (Obj *)(closure->function));
    for (int i = 0; i < closure->upvalue_count; i++) {
      gc_mark_obj(gc, (Obj *)(closure->upvalues[i]));
    }
    break;
  }

  case O_FUNCTION: {
    ObjFunction *fun = (ObjFunction *)(obj);
    gc_mark_obj(gc, (Obj *)(fun->name));
    gc_mark_values(gc, fun->chunk.constants);
    break;
  }

  case O_INSTANCE: {
    ObjInstance *inst = (ObjInstance *)(obj);
    gc_mark_obj(gc, (Obj *)(inst->klass));
    gc_mark_table(gc, &inst->fields);
    break;
  }

  case O_UPVALUE: {
    gc_mark_value(gc, ((ObjUpvalue *)(obj))->closed);
    break;
  }

  // No further references.
  case O_NATIVE:
  case O_STRING:
    break;
  }
}

static void gc_sweep(Gc gc) {
  Obj *prev = NULL;
  Obj *obj = gc.vm->objects;
  while (obj != NULL) {
    // Reachable => reset and continue
    if (obj->is_marked) {
#ifdef DEBUG_LOG_GC
      printf("%p keep type=%d ", (void *)(obj), obj->type);
      print_value(V_OBJ(obj));
      printf("\n");
#endif
      obj->is_marked = false;

      prev = obj;
      obj = obj->next;
      continue;
    }

    // Pop the unreachable object from the list...
    Obj *unreachable = obj;
    obj = unreachable->next;
    if (prev != NULL) {
      prev->next = obj;
    } else {
      // The first object was the unreachable one, so start the VM's object list
      // at the next one.
      gc.vm->objects = obj;
    }

    // ... and free it.
    obj_free(gc, unreachable);
  }
}

static void gc_trace_references(Gc gc) {
  while (gc.vm->gc_pending_len > 0) {
    Obj *obj = gc.vm->gc_pending_stack[--gc.vm->gc_pending_len];
    gc_expand_obj(gc, obj);
  }
}

static void collect_garbage(Gc gc) {
#ifdef DEBUG_LOG_GC
  printf("-- gc start\n");
  size_t before = gc.vm->bytes_allocated;
#endif

  gc_mark_roots(gc);
  gc_trace_references(gc);
  table_remove_unreachable(&gc.vm->strings);
  gc_sweep(gc);

  gc.vm->next_gc = gc.vm->bytes_allocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  size_t after = gc.vm->bytes_allocated;
  printf("   gc collected %zu bytes (%zu => %zu) next at %zu\n", before - after,
         before, after, gc.vm->next_gc);
  printf("-- gc end\n");
#endif
}

void *_reallocate(void *ptr, size_t new) {
  if (new == 0) {
    free(ptr);
    return NULL;
  }

  void *result = realloc(ptr, new);
  if (result == NULL) {
    exit(1);
  }
  return result;
}

void *reallocate(Gc gc, void *ptr, size_t old, size_t new) {
  if (gc.vm == NULL) {
    return _reallocate(ptr, new);
  }

  if (new >= old) {
    gc.vm->bytes_allocated += new - old;
  } else {
    gc.vm->bytes_allocated -= old - new;
  }

  if (new > old) {
#ifdef DEBUG_STRESS_GC
#ifdef DEBUG_LOG_GC
    printf("-- gc stress");
#endif
    collect_garbage(gc);
#endif
  }

  if (gc.vm->bytes_allocated > gc.vm->next_gc) {
#ifdef DEBUG_LOG_GC
    printf("-- gc now: %zu > %zu\n", gc.vm->bytes_allocated, gc.vm->next_gc);
#endif
    collect_garbage(gc);
  }

  return _reallocate(ptr, new);
}
