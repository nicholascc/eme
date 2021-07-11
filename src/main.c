#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "c-utils/integer.h"
#include "lexer.h"
#include "ast.h"
#include "symbol_table.h"
#include "parser.h"
#include "files.h"
#include "error.h"


int main(void) {
  st_init(); // init symbol table
  init_file_array();
  char *contents = add_file("./examples/test.eme");
  printf("--- Compiling file\n\n%s\n--- End file\n\n", contents);
  Lexer lexer = new_lexer(contents, 0);
  printf("Parsed result:\n\n");
  Ast_File arr = parse_file(&lexer);
  print_ast_file(arr);
  //print_error_message("message",0,1,3);
  printf("\n");
  return 0;
}
