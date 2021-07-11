#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "c-utils/integer.h"

void st_init(); // must be called at start of program in main()
u64 st_get_id_of(char *str, int length);
char *st_get_str_of(u64 id, int *length);

#endif /* end of include guard: SYMBOL_TABLE_H */
