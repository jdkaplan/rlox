#include <stdlib.h>

#include "memory.h"

void collect_garbage(void) {
  // TODO
}

void *reallocate(void *ptr, size_t old, size_t new) {
  if (old > new) {
    collect_garbage();
  }

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
