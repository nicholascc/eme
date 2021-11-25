#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "c-utils/integer.h"

void st_init(); // must be called at start of program in main()

typedef char * symbol;

// gets the symbol id for `str` if it exists, otherwise creates a new entry
// and generates a new id (literally just a unique string)
// str should not be null terminated.
symbol st_get_id_of(char *str, int length);
// the same but where str is null terminated
symbol st_get_id_of_null_terminated(char *str);

// implementation is just a cast
char *st_get_str_of(symbol id);

#endif /* end of include guard: SYMBOL_TABLE_H */
