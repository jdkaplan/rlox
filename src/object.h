#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

typedef enum {
  O_CLOSURE,
  O_FUNCTION,
  O_NATIVE,
  O_STRING,
  O_UPVALUE,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj *next;
};

typedef struct {
  Obj obj;
  unsigned int arity;
  int upvalue_count;
  Chunk chunk;
  ObjString *name;
} ObjFunction;

typedef Value (*NativeFn)(unsigned int argc, Value *args);

typedef struct {
  Obj obj;
  NativeFn fn;
} ObjNative;

struct ObjString {
  Obj obj;
  size_t length;
  char *chars;
  uint32_t hash;
};

typedef struct ObjUpvalue ObjUpvalue;

struct ObjUpvalue {
  Obj obj;
  Value *location;
  Value closed;
  ObjUpvalue *next;
};

typedef struct {
  Obj obj;
  ObjFunction *function;

  ObjUpvalue **upvalues;
  int upvalue_count;
} ObjClosure;

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLOSURE(value)  (is_obj_type(value, O_CLOSURE))
#define IS_FUNCTION(value) (is_obj_type(value, O_FUNCTION))
#define IS_NATIVE(value)   (is_obj_type(value, O_NATIVE))
#define IS_STRING(value)   (is_obj_type(value, O_STRING))

#define AS_CLOSURE(value)  ((ObjClosure *)(AS_OBJ(value)))
#define AS_FUNCTION(value) ((ObjFunction *)(AS_OBJ(value)))
#define AS_NATIVE(value)   (((ObjNative *)(AS_OBJ(value)))->fn)
#define AS_STRING(value)   ((ObjString *)(AS_OBJ(value)))
#define AS_CSTRING(value)  (AS_STRING(value)->chars)

static inline bool is_obj_type(Value value, ObjType type) {
  return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

ObjClosure *closure_new(Obj **objs, ObjFunction *function);
ObjFunction *function_new(Obj **objs);
ObjNative *native_new(Obj **objs, NativeFn fn);
ObjUpvalue *upvalue_new(Obj **objs, Value *slot);

ObjString *str_take(Obj **objs, Table *strings, char *chars, size_t length);
ObjString *str_clone(Obj **objs, Table *strings, const char *chars,
                     size_t length);
void print_object(Value value);

void free_objects(Obj *root);
void obj_free(Obj *obj);

#endif
