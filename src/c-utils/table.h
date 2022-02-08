#ifndef C_UTILS_TABLE_H
#define C_UTILS_TABLE_H

#include "integer.h"

#define TABLE_MAX_LOAD 0.75

// a table from a 64-bit hash to VALUE_TYPE.
// built using code from https://craftinginterpreters.com/hash-tables.html
#define GENERATE_TABLE_HEADER(VALUE_TYPE,TABLE_NAME) \
typedef struct TABLE_NAME##_Entry { \
  u64 key; \
  VALUE_TYPE value; \
} TABLE_NAME##_Entry; \
typedef struct TABLE_NAME { \
  u32 count; \
  u32 capacity; \
  TABLE_NAME##_Entry *entries; \
} TABLE_NAME; \
TABLE_NAME init_##TABLE_NAME(); \
void free_##TABLE_NAME(TABLE_NAME *table); \
void expand_##TABLE_NAME(TABLE_NAME *table, u32 capacity); \
void allocate_##TABLE_NAME(TABLE_NAME *table, u32 capacity); \
bool get_##TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE *value); \
bool set_##TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE value); \
VALUE_TYPE *put_and_get_ptr_##TABLE_NAME(TABLE_NAME *table, u64 key); \
void add_all_##TABLE_NAME(TABLE_NAME *from, TABLE_NAME *to);

#define GENERATE_TABLE_CODE(VALUE_TYPE, TABLE_NAME) \
TABLE_NAME init_##TABLE_NAME() { \
  TABLE_NAME table; \
  table.count = 0; \
  table.capacity = 0; \
  table.entries = NULL; \
  return table; \
} \
void free_##TABLE_NAME(TABLE_NAME* table) { \
  if(table->entries) free(table->entries); \
  table->count = 0; \
  table->capacity = 0; \
  table->entries = NULL; \
} \
TABLE_NAME##_Entry *find_entry_##TABLE_NAME(TABLE_NAME *table, u64 key) { \
  u32 index = key & (table->capacity-1); \
  while(true) { \
    TABLE_NAME##_Entry* entry = &table->entries[index]; \
    if (entry->key == key || entry->key == 0) { \
      return entry; \
    } \
    index = (index + 1) & (table->capacity-1); \
  } \
} \
void allocate_##TABLE_NAME(TABLE_NAME *table, u32 capacity) { \
  table->entries = calloc(capacity, sizeof(TABLE_NAME##_Entry)); \
  table->capacity = capacity; \
  table->count = 0; \
} \
void expand_##TABLE_NAME(TABLE_NAME *table, u32 capacity) { \
  assert(capacity > table->capacity && "You can't expand a table to a capacity <= its current capacity"); \
  TABLE_NAME old = *table; \
  allocate_##TABLE_NAME(table, capacity); \
  add_all_##TABLE_NAME(&old, table); \
  free_##TABLE_NAME(&old); \
} \
void add_all_##TABLE_NAME(TABLE_NAME *from, TABLE_NAME *to) { \
  for (int i = 0; i < from->capacity; i++) { \
    TABLE_NAME##_Entry entry = from->entries[i]; \
    if (entry.key != 0) { \
      set_##TABLE_NAME(to, entry.key, entry.value); \
    } \
  } \
} \
bool set_##TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE value) { \
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) { \
    int capacity = table->capacity < 8 ? 8 : table->capacity*2; \
    expand_##TABLE_NAME(table, capacity); \
  } \
  TABLE_NAME##_Entry *entry = find_entry_##TABLE_NAME(table, key); \
  bool isNewKey = entry->key == 0; \
  if (isNewKey) table->count++; \
  \
  entry->key = key; \
  entry->value = value; \
  return isNewKey; \
} \
bool get_##TABLE_NAME(TABLE_NAME *table, u64 key, VALUE_TYPE *value) { \
  if (table->count == 0) return false; \
  \
  TABLE_NAME##_Entry* entry = find_entry_##TABLE_NAME(table, key); \
  if (entry->key == 0) return false; \
  \
  *value = entry->value; \
  return true; \
} \
VALUE_TYPE *put_and_get_ptr_##TABLE_NAME(TABLE_NAME *table, u64 key) {\
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) { \
    int capacity = table->capacity < 8 ? 8 : table->capacity*2; \
    expand_##TABLE_NAME(table, capacity); \
  } \
  TABLE_NAME##_Entry* entry = find_entry_##TABLE_NAME(table, key); \
  if (entry->key == 0) table->count++; \
  entry->key = key; \
  return &entry->value; \
}


#endif
