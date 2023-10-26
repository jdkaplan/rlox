#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "vec.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
  T_BOOL,
  T_NIL,
  T_NUMBER,
  T_OBJ,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj *obj;
  } as;
} Value;

#define IS_BOOL(value)   ((value).type == T_BOOL)
#define IS_NIL(value)    ((value).type == T_NIL)
#define IS_NUMBER(value) ((value).type == T_NUMBER)
#define IS_OBJ(value)    ((value).type == T_OBJ)

#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value)    ((value).as.obj)

#define V_BOOL(value)   ((Value){T_BOOL, {.boolean = value}})
#define V_NIL           ((Value){T_NIL, {.number = 0}})
#define V_NUMBER(value) ((Value){T_NUMBER, {.number = value}})
#define V_OBJ(value)    ((Value){T_OBJ, {.obj = (Obj *)value}})

typedef VEC(Value) Values;

void print_value(Value value);
bool value_eq(Value a, Value b);

#endif
