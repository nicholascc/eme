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

typedef enum Compilation_Mode {
  MODE_INTERPRET,
  MODE_COMPILE
} Compilation_Mode;

int main(int argc, char *argv[]) {
  if(argc < 3)
    print_and_exit("You must provide at least two arguments: the source path, and the compilation mode (-i or -o).");

  char *source_file = argv[1];
  Compilation_Mode mode;
  if(strcmp(argv[2], "-i") == 0) {
    mode = MODE_INTERPRET;
  } else if(strcmp(argv[2], "-o") == 0) {
    mode = MODE_COMPILE;
  } else
    print_and_exit("Your second argument must be the compilation mode (-i or -o).");


  st_init();
  init_primitive_types();
  register_parser_symbols();
  init_file_array();
  bytecode_units = init_Bytecode_Unit_Ptr_Array(2);

  int main_file_id = add_file(source_file);

  Compilation_Unit *main_module = parse_file(main_file_id);
  assert(main_module->type == UNIT_MODULE);

  if(should_exit_after_parsing) {
    printf("An error occurred during parsing, exiting.\n");
    exit(1);
  }

  //printf("\n\nParsed result:\n\n");
  // print_compilation_unit(main_module);

  bool compilation_has_errors = false;

  Bytecode_Unit *main;
  bool main_found = false;
  for(int i = 0; i < main_module->data.module.scope.entries.length; i++) {
    Scope_Entry entry = main_module->data.module.scope.entries.data[i];
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
    print_error_message("I can't find the main function. Declare a function 'eme' to serve as the entry point to your program.", NULL_LOCATION);
    exit(1);
  }

  if(compilation_has_errors) {
    printf("Compilation had errors. Exiting...\n");
    exit(1);
  }

  if(mode == MODE_INTERPRET) {
    printf("Running input code...\n");
    printf("Result: %lli\n", *((s64 *)interpret_bytecode_unit(main, NULL)));
  } else if(mode == MODE_COMPILE) {
    char *out_obj = NULL;
    char *out_asm = NULL;
    char *out_ir = NULL;
    if(argc == 4) {
      out_obj = argv[3];
    } else if(argc == 5) {
      out_obj = argv[3];
      out_asm = argv[4];
    } else if(argc == 6) {
      out_obj = argv[3];
      out_asm = argv[4];
      out_ir = argv[5];
    } else if(argc != 3) {
      print_and_exit("I expect a maximum of 3 output file arguments.\n");
    }

    llvm_generate_module(bytecode_units, out_obj, out_asm, out_ir);
  }

  return 0;
}
