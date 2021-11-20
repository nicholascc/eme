#include "symbol_table.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"

// Symbol table str -> id is a darray of darrays of symbol table entries
// each internal darray of symbol table entries corresponds to a different
// length.
typedef struct String_To_Id_Entry {
  u64 id;
  char *str; // NOT NULL TERMINATED
} String_To_Id_Entry;

// @Speed: currently symbol table (string to id) implementation is a list (actually technically list of lists), meaning looking up an id from a string is very inefficient. In the future (if speed is a problem in the lexer) I should use hash table or self-organizing list instead.
GENERATE_DARRAY_HEADER(String_To_Id_Entry, String_To_Id_Internal_Array);
GENERATE_DARRAY_CODE(String_To_Id_Entry, String_To_Id_Internal_Array);
GENERATE_DARRAY_HEADER(String_To_Id_Internal_Array, String_To_Id_Symbol_Table);
GENERATE_DARRAY_CODE(String_To_Id_Internal_Array, String_To_Id_Symbol_Table);

// Symbol table id -> str is a darray indexed by ID.
typedef struct Id_To_String_Entry {
  char *str;
  int length;
} Id_To_String_Entry;

GENERATE_DARRAY_HEADER(Id_To_String_Entry, Id_To_String_Symbol_Table);
GENERATE_DARRAY_CODE(Id_To_String_Entry, Id_To_String_Symbol_Table);



String_To_Id_Symbol_Table string_to_id_symbol_table;
Id_To_String_Symbol_Table id_to_string_symbol_table;
void st_init() { // called at start of program
  string_to_id_symbol_table = init_String_To_Id_Symbol_Table(64);
  id_to_string_symbol_table = init_Id_To_String_Symbol_Table(1024);
}

// these two function are a black-box for storing strings permanently until the symbol table itself is freed.
char *copy_to_symbol_memory(char *str, int length) {
  char *new = malloc(length);
  assert(new != NULL && "malloc failed in copy_to_symbol_memory");
  memcpy(new, str, length);
  return new;
}

void free_symbol_memory(char *str) {
  free(str);
}

// gets the symbol id for `str` if it exists, otherwise creates a new entry
// and generates a new id
// str should not be null terminated.
u64 st_get_id_of(char *str, int length) {
  int internal_index = length - 1;
  while(internal_index >= string_to_id_symbol_table.length) {
    String_To_Id_Symbol_Table_push(&string_to_id_symbol_table,
                                   init_String_To_Id_Internal_Array(128)); // @Speed maybe use better initial reserve size?
  }

  String_To_Id_Internal_Array *iarray = &string_to_id_symbol_table.data[internal_index];

  for(u64 i = 0; i < iarray->length; i++) {  // @Speed use more efficient search algorithm, maybe also search most recently added symbols? (since symbols are likely to be used right after being defined)
    String_To_Id_Entry entry = iarray->data[i];
    bool match = true;
    for(int j = 0; j < length; j++) {
      if(entry.str[j] != str[j]) {
        match = false;
        break;
      }
    }
    if(match) return entry.id;
  }

  String_To_Id_Entry new_sti_entry;
  new_sti_entry.id = id_to_string_symbol_table.length;
  new_sti_entry.str = copy_to_symbol_memory(str, length);
  String_To_Id_Internal_Array_push(iarray, new_sti_entry);

  Id_To_String_Entry new_its_entry;
  new_its_entry.str = new_sti_entry.str;
  new_its_entry.length = length;
  Id_To_String_Symbol_Table_push(&id_to_string_symbol_table, new_its_entry);
  return new_sti_entry.id;
}

char *st_get_str_of(u64 id, int *length) {
  *length = id_to_string_symbol_table.data[id].length;
  return id_to_string_symbol_table.data[id].str;
}
