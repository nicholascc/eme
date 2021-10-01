#include "type_inference.h"

#include <stdio.h>

#include "ast.h"
#include "error.h"

bool is_unknown_int(Type_Info_Type t) {
  return t == TYPE_UNKNOWN_INT;
}

bool is_signed_int(Type_Info_Type t) {
  switch(t) {
    case TYPE_INT:
    case TYPE_S8:
    case TYPE_S16:
    case TYPE_S32:
    case TYPE_S64:
      return true;
  }
  return false;
}

bool is_unsigned_int(Type_Info_Type t) {
  switch(t) {
    case TYPE_UINT:
    case TYPE_U8:
    case TYPE_U16:
    case TYPE_U32:
    case TYPE_U64:
      return true;
  }
  return false;
}

bool is_concrete_int(Type_Info_Type t) {
  return is_signed_int(t) || is_unsigned_int(t);
}

bool is_any_int(Type_Info_Type t) {
  return is_concrete_int(t) || is_unknown_int(t);
}

int integer_width(Type_Info_Type t) {
  switch(t) {
    case TYPE_UINT:
    case TYPE_INT: assert(false);
    case TYPE_U8:
    case TYPE_S8: return 8;
    case TYPE_U16:
    case TYPE_S16: return 16;
    case TYPE_U32:
    case TYPE_S32: return 32;
    case TYPE_U64:
    case TYPE_S64: return 64;
  }
}

s64 power_of_two(int n) {
  return ((s64)1) << ((s64) n);
}

s64 s64_abs(s64 x) {
  return x > 0 ? x : -x;
}

bool can_implicitly_cast(Type_Info before, Type_Info after) {
  printf("TESTING CAST, types %s -> %s\n", type_info_to_string(before), type_info_to_string(after));
  if(is_concrete_int(before.type) && is_concrete_int(after.type)) {
    if(before.type == after.type)
      return true;
    else if(is_signed_int(before.type) && is_unsigned_int(after.type))
      return false;
    else if(integer_width(before.type) >= integer_width(after.type))
      return false;
    else
      return true;

  } else if(is_unknown_int(before.type) && is_concrete_int(after.type)) {
    // Can depromote this unknown integer?
    s64 literal = before.data.unknown_int;
    if(is_unsigned_int(after.type)) {
      if(literal < 0) return false;
      switch(after.type) {
        case TYPE_UINT: assert(false && "Not handling uints yet. Maybe get rid of them?");
        case TYPE_U8: return literal < power_of_two(8);
        case TYPE_U16: return literal < power_of_two(16);
        case TYPE_U32: return literal < power_of_two(32);
        case TYPE_U64: return true;
      }
    } else {
      switch(after.type) {
        case TYPE_INT: assert(false && "Not handling ints yet. Maybe get rid of them?");
        case TYPE_S8:  return s64_abs(literal) < power_of_two(7);
        case TYPE_S16: return s64_abs(literal) < power_of_two(15);
        case TYPE_S32: return s64_abs(literal) < power_of_two(31);
        case TYPE_S64: return true;
      }
    }

    return true;
  } else if(is_unknown_int(before.type) && is_unknown_int(after.type)) {
    return true;
  }
  return false;
}

Type_Info solidify_type(Type_Info x, Ast_Node *node) {
  if(is_unknown_int(x.type)) {
    Type_Info r;
    r.reference_count = 0;
    // @Incomplete is this really the behavior we want?
    if(s64_abs(x.data.unknown_int) < power_of_two(31))
      r.type = TYPE_S32;
    else
      r.type = TYPE_S64;
    return r;
  } else if(is_concrete_int(x.type))
    return x;
  else {
    error_at_ast_node("I don't know how to make this type concrete.", *node);
    exit(1);
  }
}







void error_cannot_implicitly_cast(Type_Info a, Type_Info b, Ast_Node node) {
  char message[1024];
  sprintf(message, "You cannot implicity perform the cast %s -> %s.", type_info_to_string(a), type_info_to_string(b));
  error_at_ast_node(message, node);
  should_exit_after_type_inference = true;
}

Type_Info type_info_of_type_expr(Ast_Node *type_node) {
  if(type_node->type != NODE_PRIMITIVE_TYPE) {
    error_at_ast_node("Internal compiler error: Compiler expected a primitive type.",
                      *type_node);
    exit(1);
  }
  Type_Info given_type;
  given_type.type = type_node->data.primitive_type;
  given_type.reference_count = 0;
  return given_type;
}


Type_Info infer_type_info_of_decl(Ast_Node *decl, Scope *scope) {
  assert(decl->type == NODE_TYPED_DECL);
  if(decl->data.decl.type_info.type != TYPE_UNKNOWN) return decl->data.decl.type_info;

  Type_Info type = type_info_of_type_expr(decl->data.decl.type);
  decl->data.decl.type_info = type;
  return decl->data.decl.type_info;
}

Type_Info infer_type_info_of_decl_set(Ast_Node *decl, Scope *scope) {
  assert(decl->type == NODE_TYPED_DECL_SET || decl->type == NODE_UNTYPED_DECL_SET);

  if(decl->data.decl_set.type_info.type != TYPE_UNKNOWN) return decl->data.decl_set.type_info;

  Type_Info inferred_type = infer_type_of_expr(decl->data.decl_set.value, scope);
  if(decl->type == NODE_TYPED_DECL_SET) {
    Type_Info given_type = type_info_of_type_expr(decl->data.decl_set.type);
    if(can_implicitly_cast(inferred_type, given_type)) {
      decl->data.decl_set.type_info = given_type;
      return given_type;
    } else {
      error_cannot_implicitly_cast(inferred_type, given_type, *decl);
      inferred_type = given_type;
    }
  }
  decl->data.decl_set.type_info = solidify_type(inferred_type, decl);
  return inferred_type;
}


Type_Info infer_type_info_of_decl_or_decl_set(Ast_Node *decl, Scope *scope) {
  if(decl->type == NODE_TYPED_DECL)
    return infer_type_info_of_decl(decl, scope);
  else if(decl->type == NODE_TYPED_DECL_SET ||
          decl->type == NODE_UNTYPED_DECL_SET)
    return infer_type_info_of_decl_set(decl, scope);
  else {
    error_at_ast_node("Internal compiler error: This is not a decl or decl_set.", *decl);
    exit(1);
  }
}


Type_Info get_type_of_identifier_in_scope(u64 symbol, Scope *scope, Ast_Node *node) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry e = scope->entries.data[i];
      if(e.symbol == symbol) {
        if(scope->is_global)
          return infer_type_info_of_decl_or_decl_set(e.declaration, scope);
        else {
          Ast_Node *decl = e.declaration;
          if(decl->type == NODE_TYPED_DECL && decl->data.decl.type_info.type != TYPE_UNKNOWN)
            return decl->data.decl.type_info;
          else if((decl->type == NODE_TYPED_DECL_SET || decl->type == NODE_UNTYPED_DECL_SET)
                  && decl->data.decl_set.type_info.type != TYPE_UNKNOWN)
            return decl->data.decl_set.type_info;
        }
      }
    }

    if(scope->is_global)
      break;
    else scope = scope->parent;
  }
  error_at_ast_node("This variable is undefined in this scope.", *node);
  exit(1);
}

Type_Info infer_type_of_expr(Ast_Node *n, Scope *scope) {
  switch(n->type) {
    case NODE_LITERAL:
      return n->data.literal.type;
    case NODE_BINARY_OP: {
      switch(n->data.binary_op.op) {
        case OPPLUS:
        case OPMINUS:
        case OPMUL:
        case OPDIV: {
          Type_Info first = infer_type_of_expr(n->data.binary_op.first, scope);
          Type_Info second = infer_type_of_expr(n->data.binary_op.second, scope);
          if(can_implicitly_cast(first, second)) return second;
          if(can_implicitly_cast(second, first)) return first;
          error_cannot_implicitly_cast(second, first, *n);
          return first;
        }

        case OPSET_EQUALS: {
          if(scope->is_global) {
            error_at_ast_node("Cannot use a set statement in the global scope.", *n);
            should_exit_after_type_inference = true;
          }
          Type_Info left = infer_type_of_expr(n->data.binary_op.first, scope);
          Type_Info right = infer_type_of_expr(n->data.binary_op.second, scope);
          if(can_implicitly_cast(right, left)) return left;
          error_cannot_implicitly_cast(right, left, *n);
          exit(1);
        }

        default:
          error_at_ast_node("Cannot infer the type of this expression yet.", *n);
          exit(1);
      }
      break;
    }
    case NODE_SYMBOL: {
      return get_type_of_identifier_in_scope(n->data.symbol, scope, n);
    }
    case NODE_BLOCK: {
      return infer_types_of_block(n);
      break;
    }
    default: {
      error_at_ast_node("Cannot infer the type of this expression yet.", *n);
      exit(1);
    }
  }
}

Type_Info infer_types_of_block(Ast_Node *block) {
  assert(block->type == NODE_BLOCK);
  Type_Info last_statement_type = NOTHING_TYPE_INFO;
  for(int i = 0; i < block->data.block.statements.length; i++) {
    Ast_Node *node = block->data.block.statements.data[i];
    switch(node->type) {
      case NODE_LITERAL:
      case NODE_BINARY_OP:
      case NODE_UNARY_OP:
      case NODE_TERNARY_IF:
      case NODE_FUNCTION_CALL:
      case NODE_SYMBOL:
      case NODE_BLOCK: {
        last_statement_type = infer_type_of_expr(node, &block->data.block.scope);
        break;
      }

      case NODE_TYPED_DECL:
      case NODE_UNTYPED_DECL_SET:
      case NODE_TYPED_DECL_SET: {
        last_statement_type = infer_type_info_of_decl_or_decl_set(node, &block->data.block.scope);
        break;
      }

      case NODE_RETURN: {
        break;
      }

      case NODE_FUNCTION_DEFINITION:
      case NODE_NULL: {
        error_at_ast_node("Null nodes or function definitions are not allowed in a function definition.", *node);
        exit(1);
      }

      case NODE_PRIMITIVE_TYPE:
      default: {
        error_at_ast_node("Internal compiler error: I cannot infer the type of this node.",
                          *node);
        exit(1);
      }
    }
  }
  return last_statement_type;
}

void infer_types_of_ast(Ast *ast) {
  for(int i = 0; i < ast->scope.entries.length; i++) {
    Scope_Entry entry = ast->scope.entries.data[i];
    Ast_Node *decl = entry.declaration;
    switch(decl->type) {
      case NODE_UNTYPED_DECL_SET: {
        error_at_ast_node("Declarations in the global scope must have a type.", *decl);
        should_exit_after_type_inference = true;
        continue;
      }
      case NODE_TYPED_DECL_SET: {
        if(decl->data.decl_set.type_info.type == TYPE_UNKNOWN)
          infer_type_info_of_decl_set(decl, &ast->scope);
        break;
      }
      case NODE_TYPED_DECL: {
        break;
      }
      case NODE_FUNCTION_DEFINITION: {
        infer_types_of_block(decl->data.function_definition.body);
        break;
      }
      default:
        error_at_ast_node("Internal compiler error: This node referred to by a scope entry is not a declaration",
                          *decl);
        exit(1);
    }
  }
}
