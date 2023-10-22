#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "vec.h"

typedef double Value;
typedef VEC(Value) Values;

void print_value(Value value);

#endif
