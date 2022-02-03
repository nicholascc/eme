#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "c-utils/integer.h"

void st_init(); // must be called at start of program in main()

typedef u64 symbol; // hash of the string
#define NULL_SYMBOL 0

// gets the symbol id for `str` if it exists, otherwise creates a new entry
// and generates a new id
// a length parameter < 0 indicates that str is null terminated.
symbol st_get_id_of(char *str, int length);

char *st_get_str_of(symbol id);

#endif /* end of include guard: SYMBOL_TABLE_H */
