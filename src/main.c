#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef _WIN32
#include <windows.h>
#endif

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
  MODE_COMPILE,
  MODE_NOTHING
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
  } else if(strcmp(argv[2], "-b") == 0) {
    mode = MODE_NOTHING;
  } else
    print_and_exit("Your second argument must be the compilation mode (-i or -o).");


  st_init();
  init_primitive_types();
  register_parser_symbols();
  init_file_array();
  bytecode_units = init_Bytecode_Unit_Ptr_Array(2);

#ifdef _WIN32
  LARGE_INTEGER main_frequency;
  LARGE_INTEGER main_start;

  QueryPerformanceFrequency(&main_frequency);
  QueryPerformanceCounter(&main_start);
#endif

  int main_file_id = add_file(source_file, NULL_LOCATION);

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
  for(int i = 0; i < files.length; i++) {
    Compilation_Unit *module = parse_file(i);
    for(int j = 0; j < module->data.module.scope.entries.length; j++) {
      Scope_Entry entry = module->data.module.scope.entries.data[j];
      Compilation_Unit *unit = entry.data.unit.unit;
      if(unit->type == UNIT_FUNCTION_SIGNATURE) {
        Compilation_Unit *body = unit->data.signature.body;
        type_infer_compilation_unit(body);
        generate_bytecode_compilation_unit(body);
        Ast_Function_Definition *n = (Ast_Function_Definition*)body->node;
        assert(body->node->type == NODE_FUNCTION_DEFINITION);
        bool is_main = n->symbol == st_get_id_of("main", -1);
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
  }

  if(!main_found) {
    print_error_message("I can't find the main function. Declare a function 'main' to serve as the entry point to your program.", NULL_LOCATION);
    exit(1);
  }

  if(compilation_has_errors) {
    printf("Compilation had errors. Exiting...\n");
    exit(1);
  }

#ifdef _WIN32
  {
    LARGE_INTEGER end;
    double interval;
    QueryPerformanceCounter(&end);
    interval = (double) (end.QuadPart - main_start.QuadPart) / main_frequency.QuadPart;

    printf("Compilation took %fs.\n", interval);
  }

  LARGE_INTEGER output_frequency;
  LARGE_INTEGER output_start;

  QueryPerformanceFrequency(&output_frequency);
  QueryPerformanceCounter(&output_start);
#endif


  if(mode == MODE_INTERPRET) {
    printf("Running input code...\n");
    interpret_bytecode_unit(main, NULL);
#ifdef _WIN32
    {
      LARGE_INTEGER end;
      double interval;
      QueryPerformanceCounter(&end);
      interval = (double) (end.QuadPart - output_start.QuadPart) / output_frequency.QuadPart;

      printf("Interpreter took %fs.\n", interval);
    }
#endif
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

#ifdef _WIN32
    {
      LARGE_INTEGER end;
      double interval;
      QueryPerformanceCounter(&end);
      interval = (double) (end.QuadPart - output_start.QuadPart) / output_frequency.QuadPart;

      printf("LLVM took %fs.\n", interval);
    }
#endif
  } else if(mode == MODE_NOTHING);

  printf("Done.\n");
  return 0;
}
