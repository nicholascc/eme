#include "type_inference.h"

#include <stdio.h>

#include "ast.h"
#include "errors.h"


void type_inference_error(char *message, Location l, bool *unit_poisoned) {
  print_error_message(message, l);
  *unit_poisoned = true;
}

void error_cannot_implicitly_cast(Type_Info a, Type_Info b, Location loc, bool cast_either_way, bool *unit_poisoned) {
  type_inference_error(NULL, loc, unit_poisoned);
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
    return true;
  }
  return false;
}

Type_Info solidify_type(Type_Info x, Ast_Node node) {
  if(x.type == TYPE_POISON) return POISON_TYPE_INFO;
  else if(x.type == TYPE_UNKNOWN_INT) {
    return INT_TYPE_INFO(true, 64);
  } else if(x.type == TYPE_INT || x.type == TYPE_BOOL)
    return x;
  else {
    print_type_info(x);
    print_error_message("I don't know how to make this type concrete.", node.loc);
    return POISON_TYPE_INFO;
  }
}






Type_Info type_info_of_type_expr(Ast_Node *type_node, bool *unit_poisoned) {
  if(type_node->type != NODE_PRIMITIVE_TYPE) {
    type_inference_error("Internal compiler error: I expected a primitive type.", type_node->loc, unit_poisoned);
    exit(1);
  }
  Ast_Primitive_Type *n = (Ast_Primitive_Type *)type_node;
  return n->type_info;
}

Type_Info infer_type_info_of_decl(Ast_Node *decl, Scope *scope, Ast_Function_Definition *fn_def, bool *unit_poisoned) {
  if(decl->type == NODE_TYPED_DECL) {
    Ast_Typed_Decl *n = (Ast_Typed_Decl *)decl;
    if(n->type_info.type == TYPE_UNKNOWN) n->type_info = type_info_of_type_expr(n->type, unit_poisoned);
    return n->type_info;

  } else if(decl->type == NODE_TYPED_DECL_SET) {
    Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)decl;
    if(n->type_info.type != TYPE_UNKNOWN) return n->type_info;

    Type_Info inferred_type = infer_type_of_expr(n->value, scope, fn_def, true, unit_poisoned);
    Type_Info given_type = type_info_of_type_expr(n->type, unit_poisoned);

    if(inferred_type.type == TYPE_POISON || given_type.type == TYPE_POISON) {
      n->type_info = POISON_TYPE_INFO;
      return POISON_TYPE_INFO;
    } else if(can_implicitly_cast(inferred_type, given_type)) {
      n->type_info = given_type;
      return given_type;
    } else {
      error_cannot_implicitly_cast(inferred_type, given_type, n->n.loc, false, unit_poisoned);
      n->type_info = POISON_TYPE_INFO;
      return POISON_TYPE_INFO;
    }
    n->type_info = solidify_type(inferred_type, n->n);
    if(n->type_info.type == TYPE_POISON) *unit_poisoned = true;
    return n->type_info;

  } else if(decl->type == NODE_UNTYPED_DECL_SET) {
    Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)decl;
    if(n->type_info.type != TYPE_UNKNOWN) return n->type_info;

    Type_Info inferred_type = infer_type_of_expr(n->value, scope, fn_def, true, unit_poisoned);
    n->type_info = solidify_type(inferred_type, n->n);
    if(n->type_info.type == TYPE_POISON) *unit_poisoned = true;
    return n->type_info;

  } else {
    type_inference_error("Internal compiler error: This is not a declaration.", decl->loc, unit_poisoned);
    exit(1);
  }
}

Type_Info infer_type_info_of_decl_unit(Compilation_Unit *unit, Scope *scope) {
  if(unit->type_inference_seen && !unit->type_inferred) {
    print_error_message("I found a circular dependency at this node.", unit->node->loc);
    unit->poisoned = true;
    return POISON_TYPE_INFO;
  }
  unit->type_inference_seen = true;
  Type_Info r = infer_type_info_of_decl(unit->node, scope, &NULL_AST_NODE, &unit->poisoned);
  unit->type_inferred = true;
  return r;
}

// DO NOT HOLD THE RETURNED POINTER!
Scope_Entry *get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Ast_Node *node, bool *scope_is_ordered) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry *e = &scope->entries.data[i];
      if(e->symbol == symbol) {
        *scope_is_ordered = scope->is_ordered;
        return e;
      }
    }

    if(scope->has_parent)
      scope = scope->parent;
    else
      break;
  }
  print_error_message("This variable is undefined in this scope.", node->loc);
  exit(1);
}

Type_Info get_type_of_identifier_in_scope(symbol symbol, Scope *scope, Ast_Node *node, bool *unit_poisoned) {
  bool scope_is_ordered;
  Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, node, &scope_is_ordered);

  if(!scope_is_ordered)
    return infer_type_info_of_decl_unit(e->declaration.unit, scope);
  else {
    Ast_Node *node = e->declaration.node;
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
    } else if(node->type == NODE_FUNCTION_ARGUMENT) {
      Ast_Function_Argument *n = (Ast_Function_Argument *)node;
      assert(n->type_info.type != TYPE_UNKNOWN);
      return n->type_info;
    }
    type_inference_error("Internal compiler error: Node referenced in local scope is not a declaration or argument.", node->loc, unit_poisoned);
    exit(1);
  }
}

Type_Info infer_type_of_expr(Ast_Node *node, Scope *scope, Ast_Function_Definition *fn_def, bool using_result, bool *unit_poisoned) {
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
          Type_Info first = infer_type_of_expr(n->first, scope, fn_def, true, unit_poisoned);
          Type_Info second = infer_type_of_expr(n->second, scope, fn_def, true, unit_poisoned);
          if(first.type == TYPE_POISON || second.type == TYPE_POISON) return POISON_TYPE_INFO;

          if((first.type != TYPE_INT && first.type != TYPE_UNKNOWN_INT) ||
             (second.type != TYPE_INT && second.type != TYPE_UNKNOWN_INT))
            type_inference_error("The operands to an arithmetic operator must be integers.", node->loc, unit_poisoned);

          if(first.type == TYPE_UNKNOWN_INT && second.type == TYPE_UNKNOWN_INT) {
            first.data.unknown_int += second.data.unknown_int;
            n->convert_to = solidify_type(first, *node);
            return first;
          }
          if(can_implicitly_cast(first, second)) {
            n->convert_to = solidify_type(second, *node);
            return second;
          }
          if(can_implicitly_cast(second, first)) {
            n->convert_to = solidify_type(first, *node);
            return first;
          }
          error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
          n->convert_to = POISON_TYPE_INFO;
          return POISON_TYPE_INFO;
        }

        case OPLESS_THAN:
        case OPLESS_THAN_OR_EQUAL_TO:
        case OPGREATER_THAN:
        case OPGREATER_THAN_OR_EQUAL_TO: {
          Type_Info first = infer_type_of_expr(n->first, scope, fn_def, true, unit_poisoned);
          Type_Info second = infer_type_of_expr(n->second, scope, fn_def, true, unit_poisoned);
          if(first.type == TYPE_POISON || second.type == TYPE_POISON) return POISON_TYPE_INFO;
          if((first.type != TYPE_INT && first.type != TYPE_UNKNOWN_INT) ||
             (second.type != TYPE_INT && second.type != TYPE_UNKNOWN_INT))
            type_inference_error("The operands to a comparison operator must be integers.", node->loc, unit_poisoned);
          if(can_implicitly_cast(first, second))
            n->convert_to = solidify_type(second, *node);
          else if(can_implicitly_cast(second, first))
            n->convert_to = solidify_type(first, *node);
          else {
            error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
            n->convert_to = POISON_TYPE_INFO;
            return POISON_TYPE_INFO;
          }
          return BOOL_TYPE_INFO;
        }

        case OPSET_EQUALS: {
          if(!scope->is_ordered) {
            type_inference_error("You cannot use a set statement outside of a block.", node->loc, unit_poisoned);
          }
          Type_Info left = infer_type_of_expr(n->first, scope, fn_def, true, unit_poisoned);
          Type_Info right = infer_type_of_expr(n->second, scope, fn_def, true, unit_poisoned);
          if(left.type == TYPE_POISON || right.type == TYPE_POISON) {
            n->convert_to = POISON_TYPE_INFO;
            return POISON_TYPE_INFO;
          }
          if(can_implicitly_cast(right, left)) {
            n->convert_to = solidify_type(left, *node);
            if(n->convert_to.type == TYPE_POISON) *unit_poisoned = true;
            return left;
          }
          error_cannot_implicitly_cast(right, left, node->loc, false, unit_poisoned);
          n->convert_to = POISON_TYPE_INFO;
          return POISON_TYPE_INFO;
        }

        default:
          type_inference_error("I cannot infer the type of this expression yet.", node->loc, unit_poisoned);
          exit(1);
      }
      break;
    }
    case NODE_SYMBOL: {
      return get_type_of_identifier_in_scope(((Ast_Symbol*)node)->symbol, scope, node, unit_poisoned);
    }
    case NODE_BLOCK: {
      return infer_types_of_block(node, fn_def, using_result, unit_poisoned);
    }
    case NODE_RETURN: {
      Ast_Return *n = node;
      Type_Info value = infer_type_of_expr(n->value, scope, fn_def, using_result, unit_poisoned);
      if(value.type == TYPE_POISON) return POISON_TYPE_INFO;
      if(fn_def->n.type == NODE_NULL) {
        type_inference_error("Cannot return outside of a function body", node->loc, unit_poisoned);
        return POISON_TYPE_INFO;
      }
      if(!can_implicitly_cast(value, fn_def->return_type_info)) {
        error_cannot_implicitly_cast(value, fn_def->return_type_info, node->loc, false, unit_poisoned);
        return POISON_TYPE_INFO;
      }
      return fn_def->return_type_info;
    }
    case NODE_IF: {
      Ast_If *n = node;
      n->result_is_used = using_result;
      Type_Info cond = infer_type_of_expr(n->cond, scope, fn_def, true, unit_poisoned);
      if(cond.type != TYPE_BOOL) type_inference_error("The conditional of an if statement must be a boolean value.", n->cond->loc, unit_poisoned);
      Type_Info first = infer_type_of_expr(n->first, scope, fn_def, using_result, unit_poisoned);
      Type_Info second = infer_type_of_expr(n->second, scope, fn_def, using_result, unit_poisoned);
      if(using_result) {
        if(first.type == TYPE_POISON || second.type == TYPE_POISON) return POISON_TYPE_INFO;
        if(can_implicitly_cast(first, second)) {
          n->result_type_info = second;
          return second;
        }
        if(can_implicitly_cast(second, first)) {
          n->result_type_info = first;
          return first;
        }
        error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
        return POISON_TYPE_INFO;
      }
      return NOTHING_TYPE_INFO;
    }

    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = node;
      Scope_Entry *e;
      {
        Ast_Symbol *sym = n->identifier;
        assert(sym->n.type == NODE_SYMBOL);
        // Ignoring overriding functions and shadowing variables for now.
        bool scope_is_ordered;
        e = get_entry_of_identifier_in_scope(sym->symbol, scope, n, &scope_is_ordered);
        assert(!scope_is_ordered);
      }

      Compilation_Unit *unit = e->declaration.unit;
      assert(unit->type == UNIT_FUNCTION_SIGNATURE);
      n->signature = unit;
      infer_types_of_compilation_unit(unit);

      Ast_Function_Definition *def = unit->node;
      if(n->arguments.length != def->arguments.length) {
        type_inference_error(NULL, node->loc, unit_poisoned);
        printf("I expected %i arguments to this function, but got %i instead.\n", def->arguments.length,n->arguments.length);
      } else {
        for(int i = 0; i < n->arguments.length; i++) {
          Type_Info defined = ((Ast_Function_Argument *)def->arguments.data[i])->type_info;
          Type_Info passed = infer_type_of_expr(n->arguments.data[i], scope, fn_def, true, unit_poisoned);
          if(!can_implicitly_cast(passed, defined))
            error_cannot_implicitly_cast(passed, defined, n->arguments.data[i]->loc, false, unit_poisoned);
        }
      }

      return def->return_type_info;
    }

    case NODE_NULL: {
      return NOTHING_TYPE_INFO;
    }

    default: {
      type_inference_error("I cannot infer the type of this expression yet.", node->loc, unit_poisoned);
      return POISON_TYPE_INFO;
    }
  }
}

Type_Info infer_types_of_block(Ast_Node *node_block, Ast_Function_Definition *fn_def, bool using_result, bool *unit_poisoned) {
  assert(node_block->type == NODE_BLOCK);
  Type_Info last_statement_type = NOTHING_TYPE_INFO;
  Ast_Block *block = (Ast_Block *) node_block;
  for(int i = 0; i < block->statements.length; i++) {
    Ast_Node *node = block->statements.data[i];
    bool using_this_result = using_result && (i == block->statements.length-1);
    switch(node->type) {
      case NODE_LITERAL:
      case NODE_BINARY_OP:
      case NODE_UNARY_OP:
      case NODE_IF:
      case NODE_FUNCTION_CALL:
      case NODE_SYMBOL:
      case NODE_BLOCK:
      case NODE_RETURN: {
        last_statement_type = infer_type_of_expr(node, &block->scope, fn_def, using_this_result, unit_poisoned);
        break;
      }

      case NODE_TYPED_DECL:
      case NODE_UNTYPED_DECL_SET:
      case NODE_TYPED_DECL_SET: {
        last_statement_type = infer_type_info_of_decl(node, &block->scope, fn_def, unit_poisoned);
        break;
      }

      case NODE_NULL: {
        break;
      }

      case NODE_FUNCTION_DEFINITION: {
        type_inference_error("Function definitions are not allowed in a function definition.",
                             node->loc, unit_poisoned);
        exit(1);
      }

      case NODE_PRIMITIVE_TYPE:
      default: {
        type_inference_error("Internal compiler error: I cannot infer the type of this node.",
                             node->loc, unit_poisoned);
        exit(1);
      }
    }
  }
  return using_result ? last_statement_type : NOTHING_TYPE_INFO;
}

Type_Info infer_type_info_of_function_signature(Ast_Function_Definition *node, Scope *scope, bool *unit_poisoned) {
  node->return_type_info = type_info_of_type_expr(node->return_type, scope);
  for(int i = 0; i < node->arguments.length; i++) {
    Ast_Function_Argument *arg = node->arguments.data[i];
    assert(arg->n.type == NODE_FUNCTION_ARGUMENT);
    arg->type_info = type_info_of_type_expr(arg->type, scope);
  }

  return NOTHING_TYPE_INFO; // placeholder
}

void infer_types_of_compilation_unit(Compilation_Unit *unit) {
  if(unit->type_inferred || unit->poisoned) return;
  if(unit->type_inference_seen) {
    print_error_message("I found a circular dependency at this node.", unit->node->loc);
    unit->poisoned = true;
    return;
  }
  unit->type_inference_seen = true;
  switch(unit->type) {
    case UNIT_FUNCTION_SIGNATURE: {
      if(infer_type_info_of_function_signature(unit->node, unit->scope, &unit->poisoned).type == TYPE_POISON) {
        unit->poisoned = true;
      }
      break;
    }
    case UNIT_FUNCTION_BODY: {
      Compilation_Unit *sig = unit->data.signature;
      assert(sig->type == UNIT_FUNCTION_SIGNATURE);
      infer_types_of_compilation_unit(sig);
      if(sig->poisoned) {
        unit->poisoned = true;
      } else {
        Ast_Function_Definition *node = unit->node;
        if(infer_types_of_block(node->body, node, false, &unit->poisoned).type == TYPE_POISON)
          unit->poisoned = true;
      }
      break;
    }
    default:
      print_error_message("Internal compiler error: Unknown compilation unit type.", NULL_LOCATION);
      exit(1);
  }
  unit->type_inferred = true;
}
