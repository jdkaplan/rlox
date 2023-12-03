#ifndef clox_value_h
#define clox_value_h

#include "common.h"
#include "vec.h"

#include "rlox.h"

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

#endif
