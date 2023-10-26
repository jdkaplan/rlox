#include <stdio.h>
#include <string.h>

#include "object.h"
#include "value.h"

void print_value(Value value) {
  switch (value.type) {
  case T_BOOL: {
    printf(AS_BOOL(value) ? "true" : "false");
    break;
  }
  case T_NIL: {
    printf("nil");
    break;
  }
  case T_NUMBER: {
    printf("%g", AS_NUMBER(value));
    break;
  }
  case T_OBJ: {
    print_object(value);
    break;
  }
  }
}

bool value_eq(Value a, Value b) {
  if (a.type != b.type) {
    return false;
  }

  switch (a.type) {
  case T_BOOL:
    return AS_BOOL(a) == AS_BOOL(b);

  case T_NIL:
    return true;

  case T_NUMBER:
// This really is comparing exact float equality. This is just "forwarding" the
// bad comparison from the target language into the host.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfloat-equal"
    return AS_NUMBER(a) == AS_NUMBER(b);
#pragma GCC diagnostic pop

  case T_OBJ: {
    ObjString *a_str = AS_STRING(a);
    ObjString *b_str = AS_STRING(b);
    return a_str->length == b_str->length &&
           memcmp(a_str->chars, b_str->chars, (size_t)(a_str->length)) == 0;
  }

  default:
    return false; // Unreachable
  }
}
