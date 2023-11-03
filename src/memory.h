#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define FRAMES_MAX 64
#define STACK_MAX  (FRAMES_MAX * UINT8_COUNT)

#define ALLOCATE(gc, type, count)                                              \
  (type *)reallocate(gc, NULL, 0, sizeof(type) * (count))

#define GROW_CAP(cap) ((cap) < 8 ? 8 : (cap)*2)

#define GROW_ARRAY(gc, item, ptr, old, new)                                    \
  (item *)reallocate(gc, ptr, sizeof(item) * (old), sizeof(item) * (new))

#define FREE_ARRAY(gc, item, ptr, size)                                        \
  reallocate(gc, ptr, sizeof(item) * (size), 0)

#define FREE(gc, type, ptr) reallocate(gc, ptr, sizeof(type), 0)

typedef struct Vm Vm;
typedef struct Compiler Compiler;

typedef struct {
  Vm *vm;
  Compiler *compiler;
} Gc;

void *reallocate(Gc gc, void *ptr, size_t old_size, size_t new_size);

#endif
