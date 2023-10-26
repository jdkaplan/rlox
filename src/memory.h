#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

#define ALLOCATE(type, count)                                                  \
  (type *)reallocate(NULL, 0, sizeof(type) * (count))

#define GROW_CAP(cap) ((cap) < 8 ? 8 : (cap)*2)

#define GROW_ARRAY(item, ptr, old, new)                                        \
  (item *)reallocate(ptr, sizeof(item) * (old), sizeof(item) * (new))

#define FREE_ARRAY(item, ptr, size) reallocate(ptr, sizeof(item) * (size), 0)

#define FREE(type, ptr) reallocate(ptr, sizeof(type), 0)

void *reallocate(void *ptr, size_t old_size, size_t new_size);

#endif
