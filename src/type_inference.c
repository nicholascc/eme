#include "type_inference.h"

#include <stdio.h>

#include "ast.h"
#include "errors.h"


void type_inference_error(char *message, Location l) {
  print_error_message(message, l);
  should_exit_after_type_inference = true;
}

void error_cannot_implicitly_cast(Type_Info a, Type_Info b, Ast_Node node, bool cast_either_way) {
  type_inference_error(NULL, node.loc);
  printf("I cannot implicitly cast ");
  print_type_info(a);
  printf(" -> ");
  print_type_info(b);
  if(cast_either_way)
    printf(" or vice versa.\n");
  else
    printf(".\n");
}



s64 power_of_two(int n) {
  return ((s64)1) << ((s64) n);
}

s64 s64_abs(s64 x) {
  return x > 0 ? x : -x;
}

bool can_implicitly_cast(Type_Info before, Type_Info after) {
  printf("TESTING CAST, types ");
  print_type_info(before);
  printf(" -> ");
  print_type_info(after);
  printf("\n");

  if(after.type == TYPE_POISON) return true;

  if(before.type == TYPE_INT && after.type == TYPE_INT) {
    if(before.data.integer.width == after.data.integer.width &&
       before.data.integer.is_signed == after.data.integer.is_signed)
      return true;
    else if(before.data.integer.is_signed && !after.data.integer.is_signed)
      return false;
    else if(before.data.integer.width >= after.data.integer.width)
      return false;
    else
      return true;

  } else if(before.type == TYPE_UNKNOWN_INT && after.type == TYPE_INT) {
    s64 literal = before.data.unknown_int;
    if(!after.data.integer.is_signed) {
      if(literal < 0) return false;
      if(after.data.integer.width == 64) return true;
      return literal < power_of_two(after.data.integer.width);
    } else {
      if(after.data.integer.width == 64) return true;
      return s64_abs(literal) < power_of_two(after.data.integer.width-1);
    }

    return true;
  } else if(before.type == TYPE_UNKNOWN_INT && after.type == TYPE_UNKNOWN_INT) {
    type_inference_error("Internal compiler error: Cannot implicitly cast a literal integer to a literal integer.",
                         NULL_LOCATION);
    exit(1);
  }
  return false;
}

Type_Info solidify_type(Type_Info x, Ast_Node node) {
  if(x.type == TYPE_POISON) return POISON_TYPE_INFO;
  else if(x.type == TYPE_UNKNOWN_INT) {
    Type_Info r;
    r.type = TYPE_INT;
    r.reference_count = 0;
    r.data.integer.is_signed = true;
    r.data.integer.width = 64;
    return r;
  } else if(x.type == TYPE_INT)
    return x;
  else {
    type_inference_error("I don't know how to make this type concrete.", node.loc);
    return POISON_TYPE_INFO;
  }
}






Type_Info type_info_of_type_expr(Ast_Node *type_node) {
  if(type_node->type != NODE_PRIMITIVE_TYPE) {
    type_inference_error("Internal compiler error: Compiler expected a primitive type.", type_node->loc);
    exit(1);
  }
  Ast_Primitive_Type *n = (Ast_Primitive_Type *)type_node;
  return n->type_info;
}

Type_Info infer_type_info_of_decl(Ast_Node *decl, Scope *scope) {
  if(decl->type == NODE_TYPED_DECL) {
    Ast_Typed_Decl *n = (Ast_Typed_Decl *)decl;
    if(n->type_info.type == TYPE_UNKNOWN) n->type_info = type_info_of_type_expr(n->type);
    return n->type_info;

  } else if(decl->type == NODE_TYPED_DECL_SET) {
    Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)decl;
    if(n->type_info.type != TYPE_UNKNOWN) return n->type_info;

    Type_Info inferred_type = infer_type_of_expr(n->value, scope);
    Type_Info given_type = type_info_of_type_expr(n->type);

    if(inferred_type.type == TYPE_POISON || given_type.type == TYPE_POISON) {
      n->type_info = POISON_TYPE_INFO;
      return POISON_TYPE_INFO;
    } else if(can_implicitly_cast(inferred_type, given_type)) {
      n->type_info = given_type;
      return given_type;
    } else {
      error_cannot_implicitly_cast(inferred_type, given_type, n->n, false);
      n->type_info = POISON_TYPE_INFO;
      return POISON_TYPE_INFO;
    }
    n->type_info = solidify_type(inferred_type, n->n);
    return inferred_type;

  } else if(decl->type == NODE_UNTYPED_DECL_SET) {
    Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)decl;
    if(n->type_info.type != TYPE_UNKNOWN) return n->type_info;

    Type_Info inferred_type = infer_type_of_expr(n->value, scope);
    n->type_info = solidify_type(inferred_type, n->n);
    return inferred_type;

  } else {
    type_inference_error("Internal compiler error: This is not a declaration.", decl->loc);
    exit(1);
  }
}

Type_Info infer_type_info_of_decl_unit(Compilation_Unit *unit, Scope *scope) {
  if(unit->seen_in_type_inference && !unit->type_inferred) {
    type_inference_error("I found a circular dependency at this node.", unit->node->loc);
    exit(1);
  }
  unit->seen_in_type_inference = true;
  Type_Info r = infer_type_info_of_decl(unit->node, scope);
  unit->type_inferred = true;
  return r;
}


Type_Info get_type_of_identifier_in_scope(u64 symbol, Scope *scope, Ast_Node *node) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry e = scope->entries.data[i];
      if(e.symbol == symbol) {
        if(!scope->is_ordered)
          return infer_type_info_of_decl_unit(e.declaration.unit, scope);
        else {
          Ast_Node *node = e.declaration.node;
          if(node->type == NODE_TYPED_DECL) {
            Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
            assert(n->type_info.type != TYPE_UNKNOWN);
            return n->type_info;
          } else if(node->type == NODE_TYPED_DECL_SET) {
            Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
            assert(n->type_info.type != TYPE_UNKNOWN);
            return n->type_info;
          } else if(node->type == NODE_UNTYPED_DECL_SET) {
            Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
            assert(n->type_info.type != TYPE_UNKNOWN);
            return n->type_info;
          }
          type_inference_error("Internal compiler error: Node referenced in local scope is not a declaration.", node->loc);
          exit(1);
        }
      }
    }

    if(scope->has_parent)
      scope = scope->parent;
    else
      break;
  }
  type_inference_error("This variable is undefined in this scope.", node->loc);
  exit(1);
}

Type_Info infer_type_of_expr(Ast_Node *node, Scope *scope) {
  switch(node->type) {
    case NODE_LITERAL:
      return ((Ast_Literal*)node)->type;
    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = (Ast_Binary_Op *)node;
      switch(n->operator) {
        case OPPLUS:
        case OPMINUS:
        case OPMUL:
        case OPDIV: {
          Type_Info first = infer_type_of_expr(n->first, scope);
          Type_Info second = infer_type_of_expr(n->second, scope);
          if(first.type == TYPE_POISON || second.type == TYPE_POISON) return POISON_TYPE_INFO;

          if((first.type != TYPE_INT && first.type != TYPE_UNKNOWN_INT) ||
             (second.type != TYPE_INT && second.type != TYPE_UNKNOWN_INT))
            type_inference_error("The operands to an arithmetic operator must be integers.", node->loc);

          if(first.type == TYPE_UNKNOWN_INT && second.type == TYPE_UNKNOWN_INT) {
            first.data.unknown_int += second.data.unknown_int;
            return first;
          }
          if(can_implicitly_cast(first, second)) return second;
          if(can_implicitly_cast(second, first)) return first;
          error_cannot_implicitly_cast(second, first, *node, true);
          return POISON_TYPE_INFO;
        }

        case OPSET_EQUALS: {
          if(!scope->is_ordered) {
            type_inference_error("You cannot use a set statement outside of a block.", node->loc);
          }
          Type_Info left = infer_type_of_expr(n->first, scope);
          Type_Info right = infer_type_of_expr(n->second, scope);
          if(left.type == TYPE_POISON || right.type == TYPE_POISON) return POISON_TYPE_INFO;
          if(can_implicitly_cast(right, left)) return left;
          error_cannot_implicitly_cast(right, left, *node, false);
          return POISON_TYPE_INFO;
        }

        default:
          type_inference_error("I cannot infer the type of this expression yet.", node->loc);
          exit(1);
      }
      break;
    }
    case NODE_SYMBOL: {
      return get_type_of_identifier_in_scope(((Ast_Symbol*)node)->symbol, scope, node);
    }
    case NODE_BLOCK: {
      return infer_types_of_block(node);
      break;
    }
    default: {
      type_inference_error("I cannot infer the type of this expression yet.", node->loc);
      exit(1);
    }
  }
}

Type_Info infer_types_of_block(Ast_Node *node_block) {
  assert(node_block->type == NODE_BLOCK);
  Type_Info last_statement_type = NOTHING_TYPE_INFO;
  Ast_Block *block = (Ast_Block *) node_block;
  for(int i = 0; i < block->statements.length; i++) {
    Ast_Node *node = block->statements.data[i];
    switch(node->type) {
      case NODE_LITERAL:
      case NODE_BINARY_OP:
      case NODE_UNARY_OP:
      case NODE_IF:
      case NODE_FUNCTION_CALL:
      case NODE_SYMBOL:
      case NODE_BLOCK: {
        last_statement_type = infer_type_of_expr(node, &block->scope);
        break;
      }

      case NODE_TYPED_DECL:
      case NODE_UNTYPED_DECL_SET:
      case NODE_TYPED_DECL_SET: {
        last_statement_type = infer_type_info_of_decl(node, &block->scope);
        break;
      }

      case NODE_RETURN: {
        break;
      }

      case NODE_FUNCTION_DEFINITION:
      case NODE_NULL: {
        type_inference_error("Null nodes or function definitions are not allowed in a function definition.",
                             node->loc);
        exit(1);
      }

      case NODE_PRIMITIVE_TYPE:
      default: {
        type_inference_error("Internal compiler error: I cannot infer the type of this node.",
                             node->loc);
        exit(1);
      }
    }
  }
  return last_statement_type;
}

void infer_types_of_ast(Ast *ast) {
  assert(!ast->scope.is_ordered && !ast->scope.has_parent);
  for(int i = 0; i < ast->scope.entries.length; i++) {
    Scope_Entry entry = ast->scope.entries.data[i];
    Compilation_Unit *unit = entry.declaration.unit;
    unit->seen_in_type_inference = true;
    Ast_Node *decl = unit->node;
    switch(decl->type) {
      case NODE_UNTYPED_DECL_SET:
      case NODE_TYPED_DECL_SET:
      case NODE_TYPED_DECL: {
        infer_type_info_of_decl(decl, &ast->scope);
        break;
      }
      case NODE_FUNCTION_DEFINITION: {
        infer_types_of_block(((Ast_Function_Definition*)decl)->body);
        break;
      }
      default:
        type_inference_error("Internal compiler error: This node referred to by a scope entry is not a declaration",
                             decl->loc);
        exit(1);
    }
    unit->type_inferred = true;
  }
}
