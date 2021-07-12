#include "files.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "c-utils/darray.h"

char *os_load_file(char *filename) {
  int size = 0;
	FILE *f = fopen(filename, "rb");
	assert(f != NULL && "could not open file");
	fseek(f, 0, SEEK_END);
	size = ftell(f);
	fseek(f, 0, SEEK_SET);
	char *result = (char *)malloc(size+2);
	assert(result != NULL && "malloc failed");
	assert(size == fread(result, sizeof (unsigned char), size, f) && "failed to read file");
	fclose(f);
	result[size] = ' ';
	result[size+1] = 0;
	return result;
}

GENERATE_DARRAY_CODE(File_Data, File_Array);

File_Array files;

void init_file_array(void) {
  files = init_File_Array(2);
}

// @Incomplete: check if file has already been added
char *add_file(char *filename) {
  File_Data d;
  d.filename = filename;
  d.contents = os_load_file(filename);
  File_Array_push(&files, d);
  return d.contents;
}
