#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(objs, type, object_type)                                  \
  (type *)allocate_obj(objs, sizeof(type), object_type)

static Obj *allocate_obj(Obj **objs, size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;

  if (objs != NULL) {
    object->next = *objs;
    *objs = object;
  }

  return object;
}

static ObjString *allocate_string(Obj **objs, char *chars, size_t length) {
  ObjString *string = ALLOCATE_OBJ(objs, ObjString, O_STRING);
  string->length = length;
  string->chars = chars;
  return string;
}

ObjString *str_take(Obj **objs, char *chars, size_t length) {
  return allocate_string(objs, chars, length);
}

ObjString *str_clone(const char *chars, size_t length) {
  char *heap_chars = ALLOCATE(char, length + 1);
  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return allocate_string(NULL, heap_chars, length);
}

void print_object(Value value) {
  switch (OBJ_TYPE(value)) {
  case O_STRING: {
    printf("%s", AS_CSTRING(value));
    break;
  }
  }
}

void free_objects(Obj *root) {
  while (root != NULL) {
    Obj *next = root->next;
    obj_free(root);
    root = next;
  }
}

void obj_free(Obj *obj) {
  switch (obj->type) {
  case O_STRING: {
    ObjString *str = (ObjString *)(obj);
    FREE_ARRAY(char, str->chars, str->length + 1);
    FREE(ObjString, obj);
    break;
  }
  }
}
