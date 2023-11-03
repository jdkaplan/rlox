#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

typedef struct {
  ObjString *key;
  Value value;
} Entry;

typedef struct {
  unsigned int size;
  unsigned int cap;
  Entry *entries;
} Table;

void table_init(Table *table);
void table_free(Gc gc, Table *table);
bool table_get(Table *table, ObjString *key, Value *value);
bool table_set(Gc gc, Table *table, ObjString *key, Value value);
bool table_delete(Table *table, ObjString *key);
void table_extend(Gc gc, Table *dest, Table *src);
ObjString *table_find_string(Table *table, const char *chars, size_t length,
                             uint32_t hash);
void table_remove_unreachable(Table *table);

#endif
