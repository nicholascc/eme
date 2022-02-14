#include "ast.h"

#include <stdio.h>
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"

GENERATE_DARRAY_CODE(Struct_Member, Struct_Member_Array);
GENERATE_DARRAY_CODE(Ast_Node *, Ast_Node_Ptr_Array);
GENERATE_DARRAY_CODE(Scope_Entry, Scope_Entry_Array);
GENERATE_DARRAY_CODE(Compilation_Unit *, Compilation_Unit_Ptr_Array);

Ast_Node NULL_AST_NODE = {NODE_NULL, {-1,-1,-1}};


u32 size_of_type(Type t) {
  if(t.reference_count > 0) return 8;
  else {
    Type_Info *info = t.info;
    if(info->sized) return info->size;
    if(info->sizing_seen) {
      if(info->type == TYPE_STRUCT) {
        print_error_message("This struct includes itself as a member or member of a member, which is illegal.", info->data.struct_.definition->n.loc);
        exit(1);
      } else assert(false && "non-struct object has circular size dependency");
    }
    info->sizing_seen = true;

    switch(info->type) {
      case TYPE_STRUCT: {
        info->size = 0;
        for(int i = 0; i < info->data.struct_.members.length; i++) {
          Struct_Member *member = &info->data.struct_.members.data[i];
          Compilation_Unit *unit = member->unit;
          assert(unit->type == UNIT_STRUCT_MEMBER);
          infer_types_of_compilation_unit(unit);
          s32 type_size = size_of_type(unit->data.struct_member.type);
          s32 alignment = alignment_of_size(type_size);
          if(info->size % alignment != 0) info->size += alignment - info->size % alignment;
          assert(info->size % alignment == 0);
          member->offset = info->size;

          info->size += type_size;
        }
        break;
      }
      case TYPE_INT:
      case TYPE_UNKNOWN_INT:
      case TYPE_BOOL:
      case TYPE_UNKNOWN:
      case TYPE_NOTHING:
      case TYPE_POISON:
      default: assert(false);
    }

    info->sized = true;
    return info->size;
  }
}

s32 alignment_of_size(s32 size) {
  if(size < 1) assert(false);
  if(size == 1) return 1;
  if(size == 2) return 2;
  if(size <= 4) return 4;
  return 8;
}

void init_primitive_types() {
  Type_Info *infos = malloc(32*sizeof(Type_Info)); // just allocate more space than necessary
  int n = 0;
  infos[n] = (Type_Info) {TYPE_UNKNOWN, true, true, 0, {0}};
  UNKNOWN_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_NOTHING, true, true, 0, {0}};
  NOTHING_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_POISON, true, true, 0, {0}};
  POISON_TYPE = (Type) {0, &infos[n++]};
  infos[n] = (Type_Info) {TYPE_BOOL, true, true, 1, {0}};
  BOOL_TYPE = (Type) {0, &infos[n++]};

  for(int i = 0; i < 2; i++) {
    bool is_signed = i;
    for(int j = 0; j < 4; j++) {
      u8 width_bytes;
      switch(j) {
        case 0: width_bytes = 1; break;
        case 1: width_bytes = 2; break;
        case 2: width_bytes = 4; break;
        case 3: width_bytes = 8; break;
        default: assert(false);
      }
      infos[n] = (Type_Info) {TYPE_INT, true, true, width_bytes, {.integer={is_signed,width_bytes*8}}};
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
    case TYPE_UNKNOWN_INT: printf("literal integer (%lli)", t.data.unknown_int); break;
    case TYPE_STRUCT: print_symbol(t.data.struct_.definition->symbol); break;
    default:
      printf("(unprintable type)"); break;
  }
}

void print_type(Type t) {
  for(int i = 0; i < t.reference_count; i++) printf("*");
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
        case OPREF_TYPE: printf("^"); break;
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
