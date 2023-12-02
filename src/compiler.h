#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "scanner.h"
#include "table.h"

#include "rlox.h"

ObjFunction *compile(Vm *vm, const char *source);

#endif
