#ifndef FILES_H
#define FILES_H

#include "stdbool.h"

#include "c-utils/darray.h"
#include "lexer.h"

typedef struct Compilation_Unit Compilation_Unit;

typedef struct File_Data {
  char *filename;
  char *contents;
  bool parsed;
  Token_Array tokens;
  Compilation_Unit *module;
} File_Data;

GENERATE_DARRAY_HEADER(File_Data, File_Array);

extern File_Array files;

void init_file_array(void);
int add_file(char *filename, Location loc);

#endif /* end of include guard: FILES_H */
