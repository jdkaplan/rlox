#ifndef clox_vec_h
#define clox_vec_h

#include "memory.h"

#define VEC(item)                                                              \
  struct {                                                                     \
    unsigned int len;                                                          \
    unsigned int cap;                                                          \
    item *items;                                                               \
  }

#define VEC_INIT(vec)                                                          \
  do {                                                                         \
    vec.items = NULL;                                                          \
    vec.len = 0;                                                               \
    vec.cap = 0;                                                               \
  } while (0)

#define VEC_FREE(vec)                                                          \
  do {                                                                         \
    FREE_ARRAY(typeof(vec.items), vec.items, vec.cap);                         \
    VEC_INIT(vec);                                                             \
  } while (0)

#define VEC_APPEND(vec, item)                                                  \
  do {                                                                         \
    if (vec.len + 1 >= vec.cap) {                                              \
      unsigned int old_cap = vec.cap;                                          \
      vec.cap = GROW_CAP(old_cap);                                             \
      vec.items = GROW_ARRAY(typeof(item), vec.items, old_cap, vec.cap);       \
    }                                                                          \
    vec.items[vec.len] = item;                                                 \
    vec.len++;                                                                 \
  } while (0)

#define VEC_LEN(vec)      vec.len
#define VEC_GET(vec, idx) vec.items[idx]

#endif
