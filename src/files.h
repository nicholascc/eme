#ifndef FILES_H
#define FILES_H

#include "c-utils/darray.h"

typedef struct File_Data {
  char *filename;
  char *contents;
} File_Data;

GENERATE_DARRAY_HEADER(File_Data, File_Array);

extern File_Array files;

void init_file_array(void);
char *add_file(char *filename);

#endif /* end of include guard: FILES_H */
