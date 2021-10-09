#include "error.h"

#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include "files.h"

bool should_exit_after_parsing = false;
bool should_exit_after_type_inference = false;

int int_log10(int x) {
  return (int)log10((double)x); // @Cleanup this is ugly
}

void print_error(int file_id, int line, int character) {
  printf("\n\n");
  if(file_id >= 0 && file_id < files.length) {
    if(line >= 0 && character >= 0) {
      print_lines_in_file(file_id, line, 5);
      for(int i = 0; i < character+int_log10(line); i++)
        printf(" ");
      printf("   ^\n");
      //printf("Error at line %d, character %d: ", line, character);
      printf("Error at line %d: ", line);
    } else if(line >= 0) {
      print_lines_in_file(file_id, line, 5);
      printf("Error at line %d: ", line);
    } else {
      printf("Error: ");
    }
  } else {
    printf("Error: ");
  }
}

void print_one_line_in_file(int file_id, int line) {
  assert(file_id >= 0 && file_id < files.length && "(internal compiler error) file_id does not exist");
  char *filename = files.data[file_id].filename;
  char *contents = files.data[file_id].contents;
  printf("[In file: %s]\n", filename);
  char c;
  int i = -1;
  if(line == 0) {
    printf("%d | ", line);
    while(c = contents[++i]) {
      printf("%c", c);
      if(c == '\n') {
        return;
      }
    }
    printf("\n");
    return;
  } else {
    int current_line = 1;

    while(c = contents[++i]) {
      if(c == '\n') {
        current_line++;

        if(current_line == line) {
          printf("%d | ", line);
          while(c = contents[++i]) {
            printf("%c", c);
            if(c == '\n') {
              break;
            }
          }
          return;
        }
      }
    }
    print_error(file_id, -1, -1);
    printf("Internal compiler error in error reporting: line/character past end of file\n");
    return;
  }
}

void print_lines_in_file(int file_id, int line, int lines_to_print) {
  assert(file_id >= 0 && file_id < files.length && "(internal compiler error) file_id does not exist");

  char *filename = files.data[file_id].filename;
  char *contents = files.data[file_id].contents;
  printf("[In file: %s]", filename);
  char c;
  int i = -1;

  int current_line = 1;

  if(current_line > line-lines_to_print) {
    printf("\n");
    if(current_line > line)
      return;

    // ensure alignment of line numbers by adding leading spaces
    for(int j = 0; j < int_log10(line)-int_log10(current_line); j++)
      printf(" ");
    printf("%d | ", current_line);
  }

  while(c = contents[++i]) {
    if(c == '\n') {
      current_line++;

      if(current_line > line-lines_to_print) {
        printf("\n");
        if(current_line > line)
          return;

        // ensure alignment of line numbers by adding leading spaces
        for(int j = 0; j < int_log10(line)-int_log10(current_line); j++)
          printf(" ");
        printf("%d | ", current_line);
      }
      continue;
    }

    if(current_line > line-lines_to_print)
        printf("%c", c);
  }
  print_error(file_id, -1, -1);
  printf("Internal compiler error in error reporting: line/character past end of file\n");
  return;
}
