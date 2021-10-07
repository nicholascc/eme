#include "ast.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"

GENERATE_DARRAY_CODE(Ast_Node *, Ast_Node_Ptr_Array);
GENERATE_DARRAY_CODE(Scope_Entry, Scope_Entry_Array);
GENERATE_DARRAY_CODE(Compilation_Unit *, Compilation_Unit_Ptr_Array);

Type_Info UNKNOWN_TYPE_INFO = {TYPE_UNKNOWN, 0, {0}};
Type_Info NOTHING_TYPE_INFO = {TYPE_NOTHING, 0, {0}};


Ast_Node *allocate_null_ast_node() {
  Ast_Node *result = malloc(sizeof(Ast_Node));
  result->type = NODE_NULL;
  return result;
}

Ast_Node *allocate_ast_node(Ast_Node node) {
  Ast_Node *result = malloc(sizeof(Ast_Node));
  *result = node;
  return result;
}

Compilation_Unit *allocate_null_compilation_unit() {
  return malloc(sizeof(Compilation_Unit));
}

Compilation_Unit *allocate_compilation_unit(Compilation_Unit unit) {
  Compilation_Unit *result = malloc(sizeof(Compilation_Unit));
  *result = unit;
  return result;
}


void print_symbol(u64 symbol) {
  int length;
  char *str = st_get_str_of(symbol, &length);
  for(int i = 0; i < length; i++) {
    printf("%c",str[i]);
  }
}

void print_ast(Ast ast) {
  for(int i = 0; i < ast.compilation_units.length; i++) {
    print_ast_node(*ast.compilation_units.data[i]->node);
    printf("\n\n");
  }
}

void print_ast_statement_array(Ast_Node_Ptr_Array nodes) {
  for(int i = 0; i < nodes.length; i++) {
    Ast_Node *node = nodes.data[i];
    print_ast_node(*node);
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
      print_ast_node(*n.data.function_call.identifier);
      printf("(");
      for(int i = 0; i < n.data.function_call.arguments.length; i++) {
        if(i > 0) printf(", ");
        print_ast_node(*n.data.function_call.arguments.data[i]);
      }
      printf(")");
      break;

    case NODE_TYPED_DECL_SET:
    case NODE_UNTYPED_DECL_SET:
      print_symbol(n.data.decl.symbol);
      if(n.type == NODE_TYPED_DECL_SET) {
        printf(": ");
        print_ast_node(*n.data.decl.type);
        printf(" = ");
      } else {
        printf(" := ");
      }
      print_ast_node(*n.data.decl.value);
      break;

    case NODE_TYPED_DECL:
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
      print_ast_statement_array(n.data.block.statements);
      printf("}");
      break;

    case NODE_PRIMITIVE_TYPE:
      printf("%s", type_info_to_string(n.data.primitive_type));
      break;

    default: printf("CANNOT PRINT NODE TYPE:%i", n.type); break;
  }
}

char *type_info_to_string(Type_Info t) { // @Incomplete this should probably be better but strings are hard :(
  switch(t.type) {
    case TYPE_UNKNOWN:     return "unknown";
    case TYPE_NOTHING:     return "nothing";
    case TYPE_INT:         return "some integer"; // @Incomplete: this bad
    case TYPE_UNKNOWN_INT: return "literal integer";
    default:
      return "[unprintable type]";
  }
}

void error_at_ast_node(char *message, Ast_Node n) {
  print_error_message(message, n.file_id, n.line, n.character);
}
