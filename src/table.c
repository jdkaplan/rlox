#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "table.h"
#include "value.h"

void table_init(Table *table) {
  table->size = 0;
  table->cap = 0;
  table->entries = NULL;
}

void table_free(Gc gc, Table *table) {
  FREE_ARRAY(gc, Entry, table->entries, table->cap);
  table_init(table);
}

static Entry *find(Entry *entries, uint32_t cap, ObjString *key) {
  uint32_t idx = key->hash % cap;
  Entry *tombstone = NULL;
  for (;;) {
    Entry *entry = &entries[idx];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry. If there was a tombstone along the way, return that
        // instead to reuse the space.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // Found a tombstone. Store it for later unless we already found one.
        if (tombstone == NULL) {
          tombstone = entry;
        }
      }
    } else if (entry->key == key) {
      return entry;
    }

    idx = (idx + 1) % cap;
  }
}

static void resize(Gc gc, Table *table, uint32_t cap) {
  Entry *entries = ALLOCATE(gc, Entry, cap);
  for (unsigned int i = 0; i < cap; i++) {
    entries[i].key = NULL;
    entries[i].value = V_NIL;
  }

  table->size = 0;
  for (unsigned int i = 0; i < table->cap; i++) {
    Entry *entry = &table->entries[i];
    if (entry->key == NULL) {
      continue;
    }

    Entry *dest = find(entries, cap, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->size++;
  }

  FREE_ARRAY(gc, Entry, table->entries, table->cap);
  table->entries = entries;
  table->cap = cap;
}

bool table_get(const Table *table, const ObjString *key, Value *value) {
  if (table->size == 0) {
    return false;
  }

  Entry *entry = find(table->entries, table->cap, key);
  if (entry->key == NULL) {
    return false;
  }

  *value = entry->value;
  return true;
}

bool table_set(Gc gc, Table *table, ObjString *key, Value value) {
  if (table->size + 1 > table->cap * TABLE_MAX_LOAD) {
    unsigned int cap = GROW_CAP(table->cap);
    resize(gc, table, cap);
  }

  Entry *entry = find(table->entries, table->cap, key);
  bool is_new = entry->key == NULL;
  if (is_new && IS_NIL(entry->value)) {
    table->size++;
  }

  entry->key = key;
  entry->value = value;
  return is_new;
}

bool table_delete(Table *table, const ObjString *key) {
  if (table->size == 0) {
    return false;
  }

  Entry *entry = find(table->entries, table->cap, key);
  if (entry->key == NULL) {
    return false;
  }

  // Put a tombstone in the entry so linear probing still considers this as part
  // of the chain.
  entry->key = NULL;
  entry->value = V_BOOL(false);
  return true;
}

void table_extend(Gc gc, Table *dst, Table *src) {
  for (unsigned int i = 0; i < src->cap; i++) {
    Entry *entry = &src->entries[i];
    if (entry->key != NULL) {
      table_set(gc, dst, entry->key, entry->value);
    }
  }
}

ObjString *table_find_string(Table *table, const char *chars, size_t length,
                             uint32_t hash) {
  if (table->size == 0) {
    return NULL;
  }

  uint32_t idx = hash % table->cap;
  for (;;) {
    Entry *entry = &table->entries[idx];
    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Truly empty.
        return NULL;
      }
      // This was a tombstone, so keep searching.
    } else if (entry->key->length == length && entry->key->hash == hash &&
               memcmp(entry->key->chars, chars, length) == 0) {
      // Found it!
      return entry->key;
    }

    idx = (idx + 1) % table->cap;
  }
}

void table_remove_unreachable(Table *table) {
  for (unsigned int i = 0; i < table->cap; i++) {
    Entry *entry = &table->entries[i];
    if (entry->key != NULL && !entry->key->obj.is_marked) {
      table_delete(table, entry->key);
    }
  }
}
