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
#include "bytecode.h"
#include "interpreter.h"



int main(int argc, char *argv[]) {
  char *out_obj;
  char *out_asm;
  char *out_ir;
  if(argc == 2) {
    out_obj = argv[1];
  } else if(argc == 3) {
    out_obj = argv[1];
    out_asm = argv[2];
  } else if(argc == 4) {
    out_obj = argv[1];
    out_asm = argv[2];
    out_ir = argv[3];
  } else {
    print_and_exit("I expect 1-3 arguments.\n");
  }


  st_init();
  init_file_array();

  char *contents = add_file("./examples/test.eme");
  //printf("--- Compiling file\n\n%s\n--- End file\n\n", contents);
  printf("\n\n");

  Ast *ast;
  {
    Token_Array tokens = lex_string(contents, 0);
    print_token_array(tokens);
    Token_Reader reader = (Token_Reader){tokens, 0, 0};
    ast = parse_file(&reader);
    free(tokens.data);
  }
  if(should_exit_after_parsing) {
    printf("An error occurred during parsing, exiting.\n");
    exit(1);
  }

  printf("Parsed result:\n\n");
  print_ast(*ast);

  bool compilation_has_errors = false;

  for(int i = 0; i < ast->scope.entries.length; i++) {
    Scope_Entry entry = ast->scope.entries.data[i];
    Compilation_Unit *unit = entry.declaration.unit;
    if(unit->type == UNIT_FUNCTION_SIGNATURE) {
      Compilation_Unit *body = unit->data.body;
      infer_types_of_compilation_unit(body, &ast->scope);
      generate_bytecode_compilation_unit(body, &ast->scope);
      if(body->poisoned) {
        compilation_has_errors = true;
      } else {
        print_bytecode_compilation_unit(body);
        interpret_bytecode_function(*body->bytecode.function);
      }
    }
  }

  if(compilation_has_errors) {
    printf("\n\nThere were errors during compilation, exiting.\n");
    exit(1);
  }


  llvm_generate_module(*ast, out_obj, out_asm, out_ir);

  return 0;
}
