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
  char *out_obj = NULL;
  char *out_asm = NULL;
  char *out_ir = NULL;
  if(argc == 2) {
    out_obj = argv[1];
  } else if(argc == 3) {
    out_obj = argv[1];
    out_asm = argv[2];
  } else if(argc == 4) {
    out_obj = argv[1];
    out_asm = argv[2];
    out_ir = argv[3];
  } else if(argc != 1) {
    print_and_exit("I expect a maximum of 3 arguments.\n");
  }


  st_init();
  init_primitive_types();
  register_parser_symbols();
  init_file_array();
  bytecode_units = init_Bytecode_Unit_Ptr_Array(2);

  char *contents = add_file("./examples/test.eme");
  //printf("--- Compiling file\n\n%s\n--- End file\n\n", contents);
  printf("\n\n");

  Ast *ast;
  {
    Token_Array tokens = lex_string(contents, 0);
    Token_Reader reader = (Token_Reader){tokens, 0, 0};
    ast = parse_file(&reader);
    free(tokens.data);
  }
  if(should_exit_after_parsing) {
    printf("An error occurred during parsing, exiting.\n");
    exit(1);
  }

  printf("\n\nParsed result:\n\n");
  print_ast(*ast);

  bool compilation_has_errors = false;

  Bytecode_Unit *main;
  bool main_found = false;
  for(int i = 0; i < ast->scope.entries.length; i++) {
    Scope_Entry entry = ast->scope.entries.data[i];
    Compilation_Unit *unit = entry.data.unit.unit;
    if(unit->type == UNIT_FUNCTION_SIGNATURE) {
      Compilation_Unit *body = unit->data.signature.body;
      type_infer_compilation_unit(body);
      generate_bytecode_compilation_unit(body);
      Ast_Function_Definition *n = (Ast_Function_Definition*)body->node;
      assert(body->node->type == NODE_FUNCTION_DEFINITION);
      bool is_main = n->symbol == st_get_id_of("eme", 3);
      if(is_main) main_found = true;
      if(body->poisoned) {
        compilation_has_errors = true;
      } else {
        if(is_main) {
          main = body->data.body.bytecode;
        }
      }
    }
  }

  if(!main_found) {
    print_error_message("I can't find the main function.", NULL_LOCATION);
    exit(1);
  }

  if(compilation_has_errors) {
    printf("Compilation had errors. Exiting...\n");
    exit(1);
  }

  printf("Result: %lli\n", *((s64 *)interpret_bytecode_unit(main, NULL)));

  if(compilation_has_errors) {
    printf("\n\nThere were errors during compilation, exiting.\n");
    exit(1);
  }


  llvm_generate_module(bytecode_units, out_obj, out_asm, out_ir);

  return 0;
}
