#include "ast.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"
#include "type.h"

GENERATE_DARRAY_CODE(Ast_Node *, Ast_Node_Ptr_Array);
GENERATE_DARRAY_CODE(Scope_Entry, Scope_Entry_Array);
GENERATE_DARRAY_CODE(Compilation_Unit *, Compilation_Unit_Ptr_Array);
GENERATE_TABLE_CODE(Type, Type_Table);

Ast_Node NULL_AST_NODE = {NODE_NULL, {-1,-1,-1}};

Ast_Node *allocate_ast_node(Ast_Node_Type type, u32 size) {
  Ast_Node *result = malloc(size);
  result->type = type;
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


void print_symbol(symbol symbol) {
  char *str = st_get_str_of(symbol);
  printf("%s", str);
}

void print_ast(Ast ast) {
  for(int i = 0; i < ast.compilation_units.length; i++) {
    print_compilation_unit(ast.compilation_units.data[i]);
    printf("\n\n");
  }
}

void print_ast_statement_array(Ast_Node_Ptr_Array nodes) {
  for(int i = 0; i < nodes.length; i++) {
    Ast_Node *node = nodes.data[i];
    print_ast_node(node);
    printf(";\n");
  }
}

void print_ast_node(Ast_Node *node) {
  switch(node->type) {
    case NODE_NULL: {
      printf("<NULL>");
      break;
    }

    case NODE_LITERAL: {
      Ast_Literal *n = (Ast_Literal *)node;
      printf("%lli", n->value);
      break;
    }

    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = (Ast_Binary_Op *)node;
      printf("(");
      print_ast_node(n->first);
      switch(n->operator) {
        case OPPLUS:  printf(" + "); break;
        case OPMINUS: printf(" - "); break;
        case OPMUL:   printf(" * "); break;
        case OPDIV:   printf(" / "); break;
        case OPMOD:   printf(" %% "); break;

        case OPSET_EQUALS: printf(" = "); break;
        case OPPLUS_EQUALS: printf(" += "); break;
        case OPMINUS_EQUALS: printf(" -= "); break;

        case OPLESS_THAN: printf(" < "); break;
        case OPLESS_THAN_OR_EQUAL_TO: printf(" <= "); break;
        case OPGREATER_THAN: printf(" > "); break;
        case OPGREATER_THAN_OR_EQUAL_TO: printf(" >= "); break;

        case OPSTRUCT_MEMBER: printf("."); break;
        case OPSTRUCT_MEMBER_REF: printf(".^"); break;
        case OPSUBSCRIPT: printf("["); break;

        case OPTEST_EQUALS: printf(" == "); break;
      }
      print_ast_node(n->second);
      if(n->operator == OPSUBSCRIPT) {
        printf("]");
      }
      printf(")");
      break;
    }


    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPNEGATE: printf("-"); break;
        case OPREFERENCE: printf("^"); break;
        case OPDEREFERENCE: printf("*"); break;
        case OPPLUS_PLUS_FIRST: printf("++"); break;
        case OPMINUS_MINUS_FIRST: printf("--"); break;
      }
      print_ast_node(n->operand);
      switch(n->operator) {
        case OPPLUS_PLUS_SECOND: printf("++"); break;
        case OPMINUS_MINUS_SECOND: printf("--"); break;
      }
      break;
    }

    case NODE_SYMBOL: {
      Ast_Symbol *n = (Ast_Symbol *)node;
      print_symbol(n->symbol);
      break;
    }

    case NODE_IF: {
      Ast_If *n = (Ast_If *)node;
      printf("( if ");
      print_ast_node(n->cond);
      printf(" ");
      print_ast_node(n->first);
      printf(" else ");
      print_ast_node(n->second);
      printf(")");
      break;
    }

    case NODE_WHILE: {
      Ast_While *n = (Ast_While *)node;
      printf("while ");
      print_ast_node(n->cond);
      printf(" ");
      print_ast_node(n->body);
      break;
    }

    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      print_ast_node(n->identifier);
      printf("(");
      for(int i = 0; i < n->arguments.length; i++) {
        if(i > 0) printf(", ");
        print_ast_node(n->arguments.data[i]);
      }
      printf(")");
      break;
    }

    case NODE_TYPED_DECL_SET: {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      printf(" = ");
      print_ast_node(n->value);
      break;
    }

    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
      print_symbol(n->symbol);
      printf(" := ");
      print_ast_node(n->value);
      break;
    }

    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      break;
    }

    case NODE_RETURN: {
      Ast_Return *n = (Ast_Return *)node;
      printf("return ");
      print_ast_node(n->value);
      break;
    }

    case NODE_FUNCTION_PARAMETER: {
      Ast_Function_Parameter *n = (Ast_Function_Parameter *)node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      break;
    }

    case NODE_FUNCTION_DEFINITION: {
      Ast_Function_Definition *n = (Ast_Function_Definition *)node;
      print_symbol(n->symbol);
      printf(" :: fn (");
      for(int i = 0; i < n->parameters.length; i++) {
        if(i != 0) printf(", ");
        print_ast_node(n->parameters.data[i]);
      }
      printf(") -> ");
      print_ast_node(n->return_type_node);
      printf(" ");
      print_ast_node(n->body);
      break;
    }

    case NODE_STRUCT_DEFINITION: {
      Ast_Struct_Definition *n = (Ast_Struct_Definition *)node;
      print_symbol(n->symbol);
      printf(" :: struct {\n");
      for(int i = 0; i < n->members.length; i++) {
        print_compilation_unit(n->members.data[i]);
      }
      printf("}");
      break;
    }

    case NODE_POLY_STRUCT_DEFINITION: {
      Ast_Poly_Struct_Definition *n = (Ast_Poly_Struct_Definition *)node;
      print_symbol(n->symbol);
      printf(" :: struct (");
      for(int i = 0; i < n->parameters.length; i++) {
        if(i != 0) printf(", ");
        print_ast_node(n->parameters.data[i]);
      }
      printf("){\n");
      for(int i = 0; i < n->members.length; i++) {
        print_ast_node(n->members.data[i]);
      }
      printf("}");
      break;
    }

    case NODE_BLOCK: {
      Ast_Block *n = (Ast_Block *)node;
      printf("{\n");
      print_ast_statement_array(n->statements);
      printf("}");
      break;
    }

    case NODE_PRIMITIVE_TYPE: {
      Ast_Primitive_Type *n = (Ast_Primitive_Type *)node;
      print_type(n->type);
      break;
    }

    default: printf("CANNOT PRINT NODE TYPE: %i\n", node->type); break;
  }
}

void print_compilation_unit(Compilation_Unit *unit) {
  if(unit->poisoned) printf("POISONED:\n");

  printf("type inference: ");
  if(unit->type_inferred) printf("done\n");
  else if(unit->type_inference_seen) printf("seen\n");
  else printf("not yet\n");

  printf("bytecode generation: ");
  if(unit->bytecode_generated) printf("done\n");
  else if(unit->bytecode_generating) printf("in progress\n");
  else printf("not yet\n");

  switch(unit->type) {
    case UNIT_FUNCTION_BODY: {
      printf("Function body:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    case UNIT_STRUCT: {
      printf("Struct:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    };
    default: {
      printf("Unknown type of compilation unit.\n");
    }
  }
}
