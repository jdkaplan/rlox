#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define GROW_CAP(cap) ((cap) < 8 ? 8 : (cap)*2)

#define GROW_ARRAY(item, pointer, old, new)                                    \
  (item *)reallocate(pointer, sizeof(item) * (old), sizeof(item) * (new))

#define FREE_ARRAY(item, pointer, size)                                        \
  reallocate(pointer, sizeof(item) * (size), 0)

void *reallocate(void *pointer, size_t old_size, size_t new_size);

#endif
