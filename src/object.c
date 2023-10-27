#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
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

static ObjString *allocate_string(Obj **objs, Table *strings, char *chars,
                                  size_t length, uint32_t hash) {
  ObjString *string = ALLOCATE_OBJ(objs, ObjString, O_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
  table_set(strings, string, V_NIL);
  return string;
}

static uint32_t str_hash(const char *key, size_t length) {
  uint32_t hash = 2166136261u;
  for (unsigned int i = 0; i < length; i++) {
    hash ^= (uint8_t)(key[i]);
    hash *= 16777619;
  }
  return hash;
}

ObjString *str_take(Obj **objs, Table *strings, char *chars, size_t length) {
  uint32_t hash = str_hash(chars, length);

  ObjString *interned = table_find_string(strings, chars, length, hash);
  if (interned != NULL) {
    // This takes ownership of `chars`, so free the memory if it's not going to
    // be stored anywhere.
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }

  return allocate_string(objs, strings, chars, length, hash);
}

ObjString *str_clone(Obj **objs, Table *strings, const char *chars,
                     size_t length) {
  uint32_t hash = str_hash(chars, length);

  ObjString *interned = table_find_string(strings, chars, length, hash);
  if (interned != NULL) {
    return interned;
  }

  char *heap_chars = ALLOCATE(char, length + 1);
  memcpy(heap_chars, chars, length);
  heap_chars[length] = '\0';
  return allocate_string(objs, strings, heap_chars, length, hash);
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
