#include "symbol_table.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"

// @Speed: currently symbol table implementation is a list, meaning looking up an id from a string is very inefficient. In the future I should use hash table or self-organizing list instead.
GENERATE_DARRAY_HEADER(char *, Symbol_Table);
GENERATE_DARRAY_CODE(char *, Symbol_Table);



Symbol_Table table;
void st_init() { // called at start of program
  table = init_Symbol_Table(1024);
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
  for(u64 i = 0; i < table.length; i++) {
    char *entry = table.data[i];
    bool match = true;
    for(int j = 0; ; j++) {
      if(entry[j] == 0) {
        if(j != length) match = false;
        break;
      }
      if(entry[j] != str[j]) {
        match = false;
        break;
      }
    }
    if(match) return entry;
  }

  char *entry = malloc(length+1);
  assert(entry != NULL && "malloc failed");
  memcpy(entry, str, length);
  entry[length] = 0;
  Symbol_Table_push(&table, entry);

  return entry;
}

symbol st_get_id_of_null_terminated(char *str) {
  for(u64 i = 0; i < table.length; i++) {
    char *entry = table.data[i];
    bool match = true;
    for(int j = 0; ; j++) {
      if(entry[j] == 0 || str[j] == 0) {
        if(entry[j] != 0 || str[j] != 0) match = false;
        break;
      }
      if(entry[j] != str[j]) {
        match = false;
        break;
      }
    }
    if(match) return entry;
  }

  int length;
  for(length = 0; str[length]; length++);
  
  char *entry = malloc(length+1);
  assert(entry != NULL && "malloc failed");
  memcpy(entry, str, length+1);
  Symbol_Table_push(&table, entry);

  return entry;
}

char *st_get_str_of(symbol id) {
 return id;
}
