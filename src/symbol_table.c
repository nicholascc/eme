#include "symbol_table.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/table.h"
#include "c-utils/hash.h"

GENERATE_TABLE_HEADER(char *, String_Table);
GENERATE_TABLE_CODE(char *, String_Table);



String_Table table;
void st_init() { // called at start of program
  table = init_String_Table(1024);
}

// these two function are a black-box for storing strings permanently until the symbol table itself is freed.
// Takes a non-null-terminated string and spits out a pointer to a null-terminated string.
char *copy_to_symbol_memory(char *str, int length) {
  char *new = malloc(length+1);
  assert(new != NULL && "malloc failed in copy_to_symbol_memory");
  memcpy(new, str, length);
  new[length] = 0;
  return new;
}

void free_symbol_memory(char *str) {
  free(str);
}


symbol st_get_id_of(char *str, int length) {
  u64 hash;
  if (length < 0) hash = hash_string_nullterm(str);  // string is null terminated
  else hash = hash_string(str, length);

  char **val = put_and_get_ptr_String_Table(&table, hash);
  if(*val != NULL) return hash;

  if(length < 0) { // string is null terminated
    for(length = 0; str[length]; length++);
  }

  *val = copy_to_symbol_memory(str, length);
  return hash;
}

char *st_get_str_of(symbol sym) {
  u64 hash = sym;
  char **val = put_and_get_ptr_String_Table(&table, hash);
  assert(*val != NULL && "uninitialized symbol");
  return *val;
}
