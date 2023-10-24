#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "vec.h"

typedef enum {
  T_BOOL,
  T_NIL,
  T_NUMBER,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
  } as;
} Value;

#define IS_BOOL(value)   ((value).type == T_BOOL)
#define IS_NIL(value)    ((value).type == T_NIL)
#define IS_NUMBER(value) ((value).type == T_NUMBER)

#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

#define V_BOOL(value)   ((Value){T_BOOL, {.boolean = value}})
#define V_NIL           ((Value){T_NIL, {.number = 0}})
#define V_NUMBER(value) ((Value){T_NUMBER, {.number = value}})

typedef VEC(Value) Values;

void print_value(Value value);
bool value_eq(Value a, Value b);

#endif
