#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "c-utils/integer.h"
#include "lexer.h"
#include "ast.h"
#include "symbol_table.h"
#include "parser.h"
#include "files.h"
#include "errors.h"
#include "type_inference.h"



int main(void) {
  st_init();
  init_file_array();

  char *contents = add_file("./examples/test.eme");
  printf("--- Compiling file\n\n%s\n--- End file\n\n", contents);
  Lexer lexer = new_lexer(contents, 0);
  Ast *ast = parse_file(&lexer);
  if(should_exit_after_parsing) {
    printf("An error occurred during parsing, exiting.\n");
    exit(1);
  }

  printf("Parsed result:\n\n");
  print_ast(*ast);
  printf("\n\n");
  infer_types_of_ast(ast);

  if(should_exit_after_type_inference) {
    printf("An error occurred during type inference, exiting.\n");
    exit(1);
  }

  return 0;
}
