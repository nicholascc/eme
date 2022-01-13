#include "ast.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"

GENERATE_DARRAY_CODE(Ast_Node *, Ast_Node_Ptr_Array);
GENERATE_DARRAY_CODE(Scope_Entry, Scope_Entry_Array);
GENERATE_DARRAY_CODE(Compilation_Unit *, Compilation_Unit_Ptr_Array);

Ast_Node NULL_AST_NODE = {NODE_NULL, {-1,-1,-1}};


void init_primitive_types() {
  Type_Info *infos = malloc(32*sizeof(Type_Info)); // just allocate more space than necessary
  int n = 0;
  infos[n] = (Type_Info) {TYPE_UNKNOWN, {0}};
  UNKNOWN_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_NOTHING, {0}};
  NOTHING_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_POISON, {0}};
  POISON_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_BOOL, {0}};
  BOOL_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_BOOL, {0}};
  BOOL_TYPE = (Type) {0, &infos[n++]};

  for(int i = 0; i < 2; i++) {
    bool is_signed = i;
    for(int j = 0; j < 4; j++) {
      u8 width;
      switch(j) {
        case 0: width = 8; break;
        case 1: width = 16; break;
        case 2: width = 32; break;
        case 3: width = 64; break;
        default: assert(false);
      }
      infos[n] = (Type_Info) {TYPE_INT, {.integer={is_signed,width}}};
      INTEGER_TYPES[i][j] = (Type) {0, &infos[n++]};
    }
  }
  INT_TYPE = INTEGER_TYPES[1][3];
  UINT_TYPE = INTEGER_TYPES[0][3];
}

Type integer_type_with(bool is_signed, u8 width) {
  switch(width) {
    case 8: return INTEGER_TYPES[is_signed][0];
    case 16: return INTEGER_TYPES[is_signed][1];
    case 32: return INTEGER_TYPES[is_signed][2];
    case 64: return INTEGER_TYPES[is_signed][3];
    default: assert(false);
  }
}

Type allocate_unknown_int_type(int value) {
  Type_Info *info = malloc(sizeof(Type_Info));
  info->type = TYPE_UNKNOWN_INT;
  info->data.unknown_int = value;
  return (Type){0, info};
}

bool type_equals(Type a, Type b) {
  return a.info == b.info && a.reference_count == b.reference_count;
}

void print_type_info(Type_Info t) {
  switch(t.type) {
    case TYPE_UNKNOWN: printf("unknown"); break;
    case TYPE_NOTHING: printf("nothing"); break;
    case TYPE_INT: {
      if(t.data.integer.is_signed) printf("s%i", t.data.integer.width);
      else printf("u%i", t.data.integer.width);
      break;
    }
    case TYPE_BOOL: printf("bool"); break;
    case TYPE_UNKNOWN_INT: printf("literal integer"); break;
    default:
      printf("(unprintable type)"); break;
  }
}

void print_type(Type t) {
  print_type_info(*t.info);
}


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
      Ast_Literal *n = node;
      printf("%lli", n->value);
      break;
    }

    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = node;
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
      Ast_Unary_Op *n = node;
      switch(n->operator) {
        case OPNEGATE: printf("-"); break;
        case OPREFERENCE: printf("&"); break;
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
      Ast_Symbol *n = node;
      print_symbol(n->symbol);
      break;
    }

    case NODE_IF: {
      Ast_If *n = node;
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
      Ast_While *n = node;
      printf("while ");
      print_ast_node(n->cond);
      printf(" ");
      print_ast_node(n->body);
      break;
    }

    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = node;
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
      Ast_Typed_Decl_Set *n = node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      printf(" = ");
      print_ast_node(n->value);
      break;
    }

    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *n = node;
      print_symbol(n->symbol);
      printf(" := ");
      print_ast_node(n->value);
      break;
    }

    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *n = node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      break;
    }

    case NODE_RETURN: {
      Ast_Return *n = node;
      printf("return ");
      print_ast_node(n->value);
      break;
    }

    case NODE_FUNCTION_PARAMETER: {
      Ast_Function_Parameter *n = node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      break;
    }

    case NODE_FUNCTION_DEFINITION: {
      Ast_Function_Definition *n = node;
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
      Ast_Struct_Definition *n = node;
      print_symbol(n->symbol);
      printf(" :: struct {\n");
      print_ast_statement_array(n->members);
      printf("}");
      break;
    }

    case NODE_BLOCK: {
      Ast_Block *n = node;
      printf("{\n");
      print_ast_statement_array(n->statements);
      printf("}");
      break;
    }

    case NODE_PRIMITIVE_TYPE: {
      Ast_Primitive_Type *n = node;
      print_type(n->type);
      break;
    }

    default: printf("CANNOT PRINT NODE TYPE:%i", node->type); break;
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
    case UNIT_FUNCTION_SIGNATURE: {
      printf("Function signature.\n");
      break;
    }
    case UNIT_STRUCT_BODY: {
      printf("Struct body:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    case UNIT_STRUCT_MEMBER: {
      printf("Struct member: ");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    default: {
      printf("Unknown type of compilation unit.\n");
    }
  }
}
