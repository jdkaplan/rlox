#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

typedef enum {
  O_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj *next;
};

struct ObjString {
  Obj obj;
  size_t length;
  char *chars;
};

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_STRING(value) (is_obj_type(value, O_STRING))

#define AS_STRING(value)  ((ObjString *)(AS_OBJ(value)))
#define AS_CSTRING(value) (AS_STRING(value)->chars)

static inline bool is_obj_type(Value value, ObjType type) {
  return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

ObjString *str_take(Obj **objs, char *chars, size_t length);
ObjString *str_clone(const char *chars, size_t length);
void print_object(Value value);

void free_objects(Obj *root);
void obj_free(Obj *obj);

#endif
