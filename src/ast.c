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
GENERATE_TABLE_CODE(Compilation_Unit *, Compilation_Unit_Ptr_Table);


Ast_Node NULL_AST_NODE = {NODE_NULL, {-1,-1,-1}};

Ast_Node *allocate_ast_node_type(Ast_Node_Type type, u32 size) {
  Ast_Node *result = malloc(size);
  result->type = type;
  return result;
}

Ast_Node *allocate_ast_node(Ast_Node *node, u32 size) {
  Ast_Node *result = malloc(size);
  memcpy(result, node, size);
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
      if(n->type.info->type == TYPE_UNKNOWN_INT)
        printf("%lli", n->value);
      else if(n->type.info->type == TYPE_BOOL)
        printf(n->value ? "true" : "false");
      else assert(false);
      break;
    }

    case NODE_LITERAL_STRING: {
      Ast_Literal_String *n = (Ast_Literal_String *)node;
      printf("\"%s\"", n->string);
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

    case NODE_BIND_SYMBOL: {
      Ast_Bind_Symbol *n = (Ast_Bind_Symbol *)node;
      printf("$");
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

    case NODE_EACH: {
      Ast_Each *n = (Ast_Each *)node;
      printf("each(");
      print_symbol(n->element);
      printf(", ");
      print_symbol(n->index);
      printf(": ");
      print_ast_node(n->collection);
      printf(") ");
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
      if(n->is_return_nothing)
        printf("return");
      else {
        printf("return ");
        print_ast_node(n->value);
      }
      break;
    }

    case NODE_PASSED_PARAMETER: {
      Ast_Passed_Parameter *n = (Ast_Passed_Parameter *)node;
      print_symbol(n->symbol);
      printf(": ");
      print_ast_node(n->type_node);
      break;
    }

    case NODE_MATCHED_PARAMETER: {
      Ast_Matched_Parameter *n = (Ast_Matched_Parameter *)node;
      print_ast_node(n->node);
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

    case NODE_FOREIGN_DEFINITION: {
      Ast_Foreign_Definition *n = (Ast_Foreign_Definition *)node;
      print_symbol(n->symbol);
      printf(" :: foreign (");
      for(int i = 0; i < n->parameters.length; i++) {
        if(i != 0) printf(", ");
        print_ast_node(n->parameters.data[i]);
      }
      printf(") -> ");
      print_ast_node(n->return_type_node);
      printf(";");
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

    case NODE_IMPORT: {
      Ast_Import *n = (Ast_Import *)node;
      printf("import \"%s\"", n->filename);
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

void print_scope(Scope scope, bool print_entries) {
  if(scope.has_parent) {
    print_scope(*scope.parent, false);
    printf(".");
  }
  switch(scope.type) {
    case SYMBOL_SCOPE: printf("SYMBOL"); break;
    case BASIC_SCOPE: printf("BASIC"); break;
    case REGISTER_SCOPE: printf("REG"); break;
    case UNIT_SCOPE: printf("UNIT"); break;
    case STRUCT_SCOPE: printf("STRUCT"); break;
    case TYPE_SCOPE: printf("TYPE"); break;
    default: assert(false);
  }
  if(!print_entries) return;

  printf(": ");
  for(int i = 0; i < scope.entries.length; i++) {
    if(i != 0) printf(", ");
    Scope_Entry e = scope.entries.data[i];
    print_symbol(e.symbol);
    printf("{");
    switch(scope.type) {
      case SYMBOL_SCOPE: break;
      case BASIC_SCOPE: print_type(e.data.basic.type); break;
      case REGISTER_SCOPE: print_type(e.data.reg.type); break;
      case UNIT_SCOPE: break;
      case STRUCT_SCOPE: print_ast_node(e.data.struct_.node); break;
      case TYPE_SCOPE: print_type(e.data.type.value); break;
      default: assert(false);
    }
    printf("}");
  }
  printf(".");
}

void print_compilation_unit(Compilation_Unit *unit) {
  printf("type inference: ");
  if(unit->type_inferred) printf("done\n");
  else if(unit->type_inference_seen) printf("seen\n");
  else printf("not yet\n");

  printf("bytecode generation: ");
  if(unit->bytecode_generated) printf("done\n");
  else if(unit->bytecode_generating) printf("in progress\n");
  else printf("not yet\n");

  printf("poisoned: ");

  if(unit->poisoned) printf("true\n");
  else printf("false\n");

  switch(unit->type) {
    case UNIT_POLY_FUNCTION: {
      printf("Polymorphic function:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
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
    }
    case UNIT_FUNCTION_SIGNATURE: {
      printf("Signature:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    case UNIT_FOREIGN_FUNCTION: {
      printf("Foreign function:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    case UNIT_IMPORT: {
      printf("Import:\n");
      print_ast_node(unit->node);
      printf("\n");
      break;
    }
    case UNIT_MODULE: {
      printf("Module (%i):\n", unit->data.module.file_id);
      for(int i = 0; i < unit->data.module.compilation_units.length; i++) {
        print_compilation_unit(unit->data.module.compilation_units.data[i]);
        printf("\n\n");
      }
      break;
    }
    default: {
      printf("Unknown type of compilation unit.\n");
    }
  }
}

Scope copy_scope(Scope s, Scope *parent) {
  Scope r = s;
  switch(s.type) {
    case SYMBOL_SCOPE:
    case BASIC_SCOPE:
    case REGISTER_SCOPE:
    case TYPE_SCOPE:
      break;
    case UNIT_SCOPE:
    case STRUCT_SCOPE:
    default:
      assert(false);
  }
  assert(r.has_parent);
  r.parent = parent;
  r.entries.length = s.entries.length;
  r.entries.reserved = r.entries.length;
  r.entries.data = malloc(r.entries.reserved *sizeof(Scope_Entry));
  memcpy(r.entries.data, s.entries.data, s.entries.length*sizeof(Scope_Entry));
  return r;
}

Ast_Node_Ptr_Array copy_ast_node_ptr_array(Ast_Node_Ptr_Array arr, Scope *scope) {
  Ast_Node_Ptr_Array result = init_Ast_Node_Ptr_Array(arr.length > 2 ? arr.length : 2);
  for(int i = 0; i < arr.length; i++) {
    Ast_Node_Ptr_Array_push(&result, copy_ast_node(arr.data[i], scope));
  }
  return result;
}

Ast_Node *copy_ast_node(Ast_Node *node, Scope *scope) {
  switch(node->type) {
    case NODE_NULL: {
      return allocate_ast_node(node, sizeof(Ast_Node));
    }
    case NODE_LITERAL: {
      return allocate_ast_node(node, sizeof(Ast_Literal));
    }
    case NODE_LITERAL_STRING: {
      return allocate_ast_node(node, sizeof(Ast_Literal_String));
    }
    case NODE_BINARY_OP: {
      Ast_Binary_Op *r = (Ast_Binary_Op *)allocate_ast_node(node, sizeof(Ast_Binary_Op));
      r->first = copy_ast_node(r->first, scope);
      r->second = copy_ast_node(r->second, scope);
      return (Ast_Node *)r;
    }
    case NODE_UNARY_OP: {
      Ast_Unary_Op *r = (Ast_Unary_Op *)allocate_ast_node(node, sizeof(Ast_Unary_Op));
      r->operand = copy_ast_node(r->operand, scope);
      return (Ast_Node *)r;
    }
    case NODE_IF: {
      Ast_If *r = (Ast_If *)allocate_ast_node(node, sizeof(Ast_If));
      r->cond = copy_ast_node(r->cond, scope);
      r->first = copy_ast_node(r->first, scope);
      r->second = copy_ast_node(r->second, scope);
      return (Ast_Node *)r;
    }
    case NODE_WHILE: {
      Ast_While *r = (Ast_While *)allocate_ast_node(node, sizeof(Ast_While));
      r->cond = copy_ast_node(r->cond, scope);
      r->body = copy_ast_node(r->body, scope);
      return (Ast_Node *)r;
    }
    case NODE_EACH: {
      Ast_Each *r = (Ast_Each *)allocate_ast_node(node, sizeof(Ast_Each));
      r->collection = copy_ast_node(r->collection, scope);
      r->scope = copy_scope(r->scope, scope);
      r->body = copy_ast_node(r->body, &r->scope);
      return (Ast_Node *)r;
    }
    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *r = (Ast_Function_Call *)allocate_ast_node(node, sizeof(Ast_Function_Call));
      r->identifier = copy_ast_node(r->identifier, scope);
      r->arguments = copy_ast_node_ptr_array(r->arguments, scope);
      return (Ast_Node *)r;
    }
    case NODE_SYMBOL: {
      return allocate_ast_node(node, sizeof(Ast_Symbol));
    }
    case NODE_BIND_SYMBOL: {
      return allocate_ast_node(node, sizeof(Ast_Bind_Symbol));
    }
    case NODE_BLOCK: {
      Ast_Block *r = (Ast_Block *)allocate_ast_node(node, sizeof(Ast_Block));
      r->scope = copy_scope(r->scope, scope);
      r->statements = copy_ast_node_ptr_array(r->statements, &r->scope);
      return (Ast_Node *)r;
    }

    case NODE_PRIMITIVE_TYPE: {
      return allocate_ast_node(node, sizeof(Ast_Primitive_Type));
    }

    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *r = (Ast_Typed_Decl *)allocate_ast_node(node, sizeof(Ast_Typed_Decl));
      r->type_node = copy_ast_node(r->type_node, scope);
      return (Ast_Node *)r;
    }
    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *r = (Ast_Untyped_Decl_Set *)allocate_ast_node(node, sizeof(Ast_Untyped_Decl_Set));
      r->value = copy_ast_node(r->value, scope);
      return (Ast_Node *)r;
    }
    case NODE_TYPED_DECL_SET: {
      Ast_Typed_Decl_Set *r = (Ast_Typed_Decl_Set *)allocate_ast_node(node, sizeof(Ast_Typed_Decl_Set));
      r->type_node = copy_ast_node(r->type_node, scope);
      r->value = copy_ast_node(r->value, scope);
      return (Ast_Node *)r;
    }
    case NODE_PASSED_PARAMETER: {
      Ast_Passed_Parameter *r = (Ast_Passed_Parameter *)allocate_ast_node(node, sizeof(Ast_Passed_Parameter));
      r->type_node = copy_ast_node(r->type_node, scope);
      return (Ast_Node *)r;
    }
    case NODE_MATCHED_PARAMETER: {
      Ast_Matched_Parameter *r = (Ast_Matched_Parameter *)allocate_ast_node(node, sizeof(Ast_Matched_Parameter));
      r->node = copy_ast_node(r->node, scope);
      return (Ast_Node *)r;
    }
    case NODE_RETURN: {
      Ast_Return *r = (Ast_Return *)allocate_ast_node(node, sizeof(Ast_Return));
      r->value = copy_ast_node(r->value, scope);
      return (Ast_Node *)r;
    }
    case NODE_FUNCTION_DEFINITION:
    case NODE_POLY_STRUCT_DEFINITION:
    default:
      assert(false);
  }
}
