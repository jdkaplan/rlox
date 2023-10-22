#include <stdlib.h>

#include "memory.h"

void *reallocate(void *pointer, size_t old, size_t new) {
  if (new == 0) {
    free(pointer);
    return NULL;
  }

  void *result = realloc(pointer, new);
  if (result == NULL) {
    exit(1);
  }
  return result;
}
