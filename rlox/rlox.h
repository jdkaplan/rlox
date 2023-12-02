#ifndef rlox_bindings_h
#define rlox_bindings_h

#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

void hello(void);

void *_reallocate(void *ptr, uintptr_t new_);

#endif /* rlox_bindings_h */
