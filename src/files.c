#include "files.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "c-utils/darray.h"
#include "errors.h"

char *concat(char *a, char *b) {
  int size = 0;
  while(a[size++] != 0);
  int a_size = size;
  while(b[size++ - a_size] != 0);

  char *c = malloc(size-1);
  for(int i = 0; i < a_size-1; i++) c[i] = a[i];
  for(int i = 0; i < size-a_size-1; i++) c[i+a_size-1] = b[i];
  c[size-2] = 0;
  return c;
}

char *os_load_file(char *filename, Location loc) {
  int size = 0;
	FILE *f = fopen(filename, "rb");
  if(f == NULL) {
    print_error(loc);
    printf("I could not open file \"%s\".\n", filename);
  }
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

int add_file(char *root_directory, char *filename, Location loc) {
  char *full_filename = concat(root_directory, filename);
  for(int i = 0; i < files.length; i++) {
    if(strcmp(full_filename, files.data[i].filename) == 0) {
      return i;
    }
  }
  File_Data d;
  d.filename = full_filename;
  d.contents = os_load_file(full_filename, loc);
  d.parsed = false;
  File_Array_push(&files, d);
  return files.length-1;
}
