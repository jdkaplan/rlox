#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#include "rlox.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) (is_obj_type(value, O_BOUND_METHOD))
#define IS_CLASS(value)        (is_obj_type(value, O_CLASS))
#define IS_CLOSURE(value)      (is_obj_type(value, O_CLOSURE))
#define IS_FUNCTION(value)     (is_obj_type(value, O_FUNCTION))
#define IS_INSTANCE(value)     (is_obj_type(value, O_INSTANCE))
#define IS_NATIVE(value)       (is_obj_type(value, O_NATIVE))
#define IS_STRING(value)       (is_obj_type(value, O_STRING))

#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)(AS_OBJ(value)))
#define AS_CLASS(value)        ((ObjClass *)(AS_OBJ(value)))
#define AS_CLOSURE(value)      ((ObjClosure *)(AS_OBJ(value)))
#define AS_FUNCTION(value)     ((ObjFunction *)(AS_OBJ(value)))
#define AS_INSTANCE(value)     ((ObjInstance *)(AS_OBJ(value)))
#define AS_NATIVE(value)       (((ObjNative *)(AS_OBJ(value)))->fn)
#define AS_STRING(value)       ((ObjString *)(AS_OBJ(value)))
#define AS_CSTRING(value)      (AS_STRING(value)->chars)

static inline bool is_obj_type(Value value, ObjType type) {
  return IS_OBJ(value) && OBJ_TYPE(value) == type;
}

ObjBoundMethod *bound_method_new(Gc gc, Value receiver, ObjClosure *method);
ObjClass *class_new(Gc gc, ObjString *name);
ObjClosure *closure_new(Gc gc, ObjFunction *function);
ObjInstance *instance_new(Gc gc, ObjClass *klass);
ObjFunction *function_new(Gc gc);
ObjNative *native_new(Gc gc, NativeFn fn);
ObjUpvalue *upvalue_new(Gc gc, Value *slot);

ObjString *str_take(Gc gc, char *chars, size_t length);
ObjString *str_clone(Gc gc, const char *chars, size_t length);

void free_objects(Gc gc, Obj *root);
void obj_free(Gc gc, Obj *obj);

#endif
