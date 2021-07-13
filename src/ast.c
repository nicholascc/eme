#include "ast.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"

GENERATE_DARRAY_CODE(Ast_Node, Ast_Node_Array);
GENERATE_DARRAY_CODE(Node_Ptr_Array_Entry, Node_Ptr_Array);
GENERATE_DARRAY_CODE(Scope_Entry, Scope);
GENERATE_DARRAY_CODE(Linear_Ast_Unit_Ptr, Linear_Ast_Unit_Ptr_Array);
GENERATE_DARRAY_CODE(Linear_Ast_Unit, Linear_Ast_Unit_Array);


void print_symbol(u64 symbol) {
  int length;
  char *str = st_get_str_of(symbol, &length);
  for(int i = 0; i < length; i++) {
    printf("%c",str[i]);
  }
}

void print_scope(Scope s) {
  printf("[\n");
  for(int i = 0; i < s.length; i++) {
    Scope_Entry e = s.data[i];
    print_symbol(e.symbol);
    printf("\n");
    /*printf("  [decl: ");
    print_ast_node(*e.declaration);
    printf("]\n");*/
    printf("  [ refs: ");
    for(int j = 0; j < e.references.length; j++) {
      printf("\n    ");
      print_ast_node(*e.references.data[j]);
    }
    printf("]\n");
  }
  printf("]\n");
}

void print_ast(Ast ast) {
  print_scope(ast.scope);
  for(int i = 0; i < ast.linear_ast_units.length; i++) {
    print_ast_node(*ast.linear_ast_units.data[i].node);
  }
}

void print_ast_statement_array(Ast_Node_Array nodes) {
  for(int i = 0; i < nodes.length; i++) {
    Ast_Node node = nodes.data[i];
    print_ast_node(node);
    printf(";\n");
  }
}

void print_ast_node(Ast_Node n) {
  switch(n.type) {
    case NODE_NULL:
      printf("<NULL>");
      break;
    case NODE_LITERAL:
      printf("%i", n.data.literal.value);
      break;
    case NODE_BINARY_OP:
      printf("(");
      print_ast_node(*n.data.binary_op.first);
      switch(n.data.binary_op.op) {
        case OPPLUS:  printf(" + "); break;
        case OPMINUS: printf(" - "); break;
        case OPMUL:   printf(" * "); break;
        case OPDIV:   printf(" / "); break;
        case OPMOD:   printf(" %% "); break;

        case OPSET_EQUALS: printf(" = "); break;
        case OPPLUS_EQUALS: printf(" += "); break;
        case OPMINUS_EQUALS: printf(" -= "); break;

        case OPSTRUCT_MEMBER: printf("."); break;
        case OPSUBSCRIPT: printf("["); break;

        case OPTEST_EQUALS: printf(" == "); break;
      }
      print_ast_node(*n.data.binary_op.second);
      if(n.data.binary_op.op == OPSUBSCRIPT) {
        printf("]");
      }
      printf(")");
      break;
    case NODE_UNARY_OP:
      switch(n.data.unary_op.operator) {
        case OPNEGATE: printf("-"); break;
        case OPREFERENCE: printf("&"); break;
        case OPDEREFERENCE: printf("*"); break;
        case OPPLUS_PLUS_FIRST: printf("++"); break;
        case OPMINUS_MINUS_FIRST: printf("--"); break;
      }
      print_ast_node(*n.data.unary_op.operand);
      switch(n.data.unary_op.operator) {
        case OPPLUS_PLUS_SECOND: printf("++"); break;
        case OPMINUS_MINUS_SECOND: printf("--"); break;
      }
      break;
    case NODE_SYMBOL: {
      print_symbol(n.data.symbol);
      break;
    }

    case NODE_TERNARY_IF:
      printf("(");
      print_ast_node(*n.data.ternary_if.cond);
      printf(" ? ");
      print_ast_node(*n.data.ternary_if.first);
      printf(" : ");
      print_ast_node(*n.data.ternary_if.second);
      printf(")");
      break;

    case NODE_FUNCTION_CALL:
      print_ast_node(*n.data.function_call.function);
      printf("(");
      for(int i = 0; i < n.data.function_call.arguments.length; i++) {
        if(i > 0) printf(", ");
        print_ast_node(n.data.function_call.arguments.data[i]);
      }
      printf(")");
      break;

    case NODE_DECL_WITH_SET:
      print_symbol(n.data.decl_with_set.symbol);
      if(n.data.decl_with_set.has_type) {
        printf(": ");
        print_ast_node(*n.data.decl_with_set.type);
        printf(" = ");
      } else {
        printf(" := ");
      }
      print_ast_node(*n.data.decl_with_set.value);
      break;

    case NODE_DECL:
      print_symbol(n.data.decl.symbol);
      printf(": ");
      print_ast_node(*n.data.decl.type);
      break;

    case NODE_RETURN:
      printf("return ");
      print_ast_node(*n.data._return.value);
      break;

    case NODE_FUNCTION_DEFINITION:
      print_symbol(n.data.function_definition.symbol);
      printf(" :: () -> ");
      print_ast_node(*n.data.function_definition.return_type);
      printf(" ");
      print_ast_node(*n.data.function_definition.body);
      break;

    case NODE_BLOCK:
      printf("{\n");
      print_scope(n.data.block.scope);
      print_ast_statement_array(n.data.block.statements);
      printf("}");
      break;
    default: printf("CANNOT PRINT NODE"); break;
  }
}
