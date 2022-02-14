#include "type_inference.h"

#include <stdio.h>

#include "ast.h"
#include "errors.h"


void type_inference_error(char *message, Location l, bool *unit_poisoned) {
  print_error_message(message, l);
  *unit_poisoned = true;
}

void error_cannot_implicitly_cast(Type a, Type b, Location loc, bool cast_either_way, bool *unit_poisoned) {
  type_inference_error(NULL, loc, unit_poisoned);
  printf("I cannot implicitly cast ");
  print_type(a);
  printf(" -> ");
  print_type(b);
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

bool can_implicitly_cast_type_info(Type_Info *before, Type_Info *after) {
  if(before == after) return true;
  else if(after->type == TYPE_POISON) return true;
  else if(before->type == TYPE_INT && after->type == TYPE_INT) {
    if(before->data.integer.width == after->data.integer.width &&
       before->data.integer.is_signed == after->data.integer.is_signed)
      return true;
    else if(before->data.integer.is_signed && !after->data.integer.is_signed)
      return false;
    else if(before->data.integer.width >= after->data.integer.width)
      return false;
    else
      return true;

  } else if(before->type == TYPE_UNKNOWN_INT && after->type == TYPE_INT) {
    s64 literal = before->data.unknown_int;
    if(!after->data.integer.is_signed) {
      if(literal < 0) return false;
      if(after->data.integer.width == 64) return true;
      return literal < power_of_two(after->data.integer.width);
    } else {
      if(after->data.integer.width == 64) return true;
      return s64_abs(literal) < power_of_two(after->data.integer.width-1);
    }

    return true;
  } else if(before->type == TYPE_UNKNOWN_INT && after->type == TYPE_UNKNOWN_INT) {
    return true;
  }
  return false;
}

bool can_implicitly_cast_type(Type before, Type after) {
  if(before.reference_count != after.reference_count) return false;
  if(before.info == after.info) return true;
  return can_implicitly_cast_type_info(before.info, after.info);
}

Type solidify_type(Type x, Ast_Node node) {
  if(x.info->type == TYPE_POISON) return POISON_TYPE;
  else if(x.info->type == TYPE_UNKNOWN_INT) {
    return INT_TYPE;
  } else if(x.info->type == TYPE_INT || x.info->type == TYPE_BOOL || x.info->type == TYPE_STRUCT)
    return x;
  else {
    print_type_info(x);
    print_error_message("I don't know how to make this type concrete.", node.loc);
    return POISON_TYPE;
  }
}






Type type_of_type_expr(Ast_Node *type_node, Scope *scope, bool *unit_poisoned) {
  if(type_node->type == NODE_PRIMITIVE_TYPE) {
    Ast_Primitive_Type *n = (Ast_Primitive_Type *)type_node;
    return n->type;
  } else if(type_node->type == NODE_SYMBOL) {
    Ast_Symbol *n = (Ast_Symbol *)type_node;
    Scope *found_scope;
    Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, type_node->loc, &found_scope);
    if(found_scope->type == FILE_SCOPE && e->declaration.unit->type == UNIT_STRUCT) {
      Compilation_Unit *unit = e->declaration.unit;
      if(!unit->type_inferred) infer_types_of_compilation_unit(unit);
      if(unit->poisoned) unit->data.struct_def.type = POISON_TYPE;
      if(unit->data.struct_def.type.info->type == TYPE_POISON) *unit_poisoned = true;
      return unit->data.struct_def.type;
    } else {
      type_inference_error("I expected this symbol to refer to a type, but it doesn't.", type_node->loc, unit_poisoned);
      exit(1);
    }
  } else if(type_node->type == NODE_UNARY_OP) {
    Ast_Unary_Op *n = (Ast_Unary_Op *)type_node;
    if(n->operator == OPREF_TYPE) {
      Type type = type_of_type_expr(n->operand, scope, unit_poisoned);
      type.reference_count++;
      return type;
    } else {
      type_inference_error("Internal compiler error: I expected a type operator.", type_node->loc, unit_poisoned);
      exit(1);
    }
  } else {
    type_inference_error("Internal compiler error: I expected a primitive type.", type_node->loc, unit_poisoned);
    exit(1);
  }
}

Type infer_type_of_decl(Ast_Node *decl, Scope *scope, Compilation_Unit *unit, bool *unit_poisoned) {
  if(decl->type == NODE_TYPED_DECL) {
    Ast_Typed_Decl *n = (Ast_Typed_Decl *)decl;
    if(n->type.info->type == TYPE_UNKNOWN) n->type = type_of_type_expr(n->type_node, scope, unit_poisoned);
    return n->type;

  } else if(decl->type == NODE_TYPED_DECL_SET) {
    Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)decl;
    if(n->type.info->type != TYPE_UNKNOWN) return n->type;

    Type inferred_type = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
    Type given_type = type_of_type_expr(n->type_node, scope, unit_poisoned);

    if(inferred_type.info->type == TYPE_POISON || given_type.info->type == TYPE_POISON) {
      n->type = POISON_TYPE;
      return POISON_TYPE;
    } else if(can_implicitly_cast_type(inferred_type, given_type)) {
      n->type = given_type;
      return given_type;
    } else {
      error_cannot_implicitly_cast(inferred_type, given_type, n->n.loc, false, unit_poisoned);
      n->type = POISON_TYPE;
      return POISON_TYPE;
    }
    n->type = solidify_type(inferred_type, n->n);
    if(n->type.info->type == TYPE_POISON) *unit_poisoned = true;
    return n->type;

  } else if(decl->type == NODE_UNTYPED_DECL_SET) {
    Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)decl;
    if(n->type.info->type != TYPE_UNKNOWN) return n->type;

    Type inferred_type = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
    n->type = solidify_type(inferred_type, n->n);
    if(n->type.info->type == TYPE_POISON) *unit_poisoned = true;
    return n->type;

  } else {
    type_inference_error("Internal compiler error: This is not a declaration.", decl->loc, unit_poisoned);
    exit(1);
  }
}

Type infer_type_of_decl_unit(Compilation_Unit *unit, Scope *scope) {
  if(unit->type_inference_seen && !unit->type_inferred) {
    print_error_message("I found a circular dependency at this node.", unit->node->loc);
    unit->poisoned = true;
    return POISON_TYPE;
  }
  unit->type_inference_seen = true;
  Type r = infer_type_of_decl(unit->node, scope, unit, &unit->poisoned);
  unit->type_inferred = true;
  return r;
}


Type infer_type_of_struct_definition(Ast_Struct_Definition *def, Scope *scope, bool *unit_poisoned) {
  Type_Info *info = malloc(sizeof(Type_Info));
  info->type = TYPE_STRUCT;
  info->data.struct_.members = init_Struct_Member_Array(2);
  info->data.struct_.definition = def;
  info->data.struct_.llvm_generated = false;
  info->data.struct_.name = def->symbol;
  info->sized = false;
  info->sizing_seen = false;
  info->size = 0;

  for(int i = 0; i < def->members.length; i++) {
    Compilation_Unit *unit = def->members.data[i];
    Ast_Node *node = unit->node;
    Struct_Member member;
    member.unit = unit;
    member.offset = -1;
    if(node->type == NODE_TYPED_DECL) {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      member.symbol = n->symbol;
    } else if(node->type == NODE_TYPED_DECL_SET) {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      member.symbol = n->symbol;
    } else assert(false);

    Struct_Member_Array_push(&info->data.struct_.members, member);
  }

  Type type = {0,info};
  def->type = type;
  return type;
}


// DO NOT HOLD THE RETURNED POINTER!
Scope_Entry *get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Location error_location, Scope **found_scope) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry *e = &scope->entries.data[i];
      if(e->symbol == symbol) {
        *found_scope = scope;
        return e;
      }
    }

    if(scope->has_parent)
      scope = scope->parent;
    else
      break;
  }
  print_error_message("This symbol is undefined in this scope.", error_location);
  exit(1);
}

Type get_type_of_identifier_in_scope(symbol symbol, Scope *scope, Location error_location, bool *unit_poisoned) {
  Scope *found_scope;
  Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, error_location, &found_scope);

  if(found_scope->type == FILE_SCOPE)
    return infer_type_of_decl_unit(e->declaration.unit, found_scope);
  else if(found_scope->type == BLOCK_SCOPE) {
    Ast_Node *node = e->declaration.node;
    if(node->type == NODE_TYPED_DECL) {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      assert(n->type.info->type != TYPE_UNKNOWN);
      return n->type;
    } else if(node->type == NODE_TYPED_DECL_SET) {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      assert(n->type.info->type != TYPE_UNKNOWN);
      return n->type;
    } else if(node->type == NODE_UNTYPED_DECL_SET) {
      Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
      assert(n->type.info->type != TYPE_UNKNOWN);
      return n->type;
    } else if(node->type == NODE_FUNCTION_PARAMETER) {
      Ast_Function_Parameter *n = (Ast_Function_Parameter *)node;
      assert(n->type.info->type != TYPE_UNKNOWN);
      return n->type;
    }
    type_inference_error("Internal compiler error: Node referenced in local scope is not a declaration or argument.", node->loc, unit_poisoned);
    exit(1);
  } else assert(false);
}

Type infer_type_of_expr(Ast_Node *node, Scope *scope, Compilation_Unit *unit, bool using_result, bool *unit_poisoned) {
  switch(node->type) {
    case NODE_LITERAL:
      return ((Ast_Literal*)node)->type;
    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPREFERENCE:
        case OPDEREFERENCE: {
          Type op = infer_type_of_expr(n->operand, scope, unit, true, unit_poisoned);
          if(op.info->type == TYPE_POISON) return POISON_TYPE;
          if(n->operator == OPREFERENCE) op.reference_count++;
          else {
            if(op.reference_count == 0) {
              type_inference_error("I cannot dereference a non-pointer type.", n->n.loc, unit_poisoned);
              return POISON_TYPE;
            }
            op.reference_count--;
          }
          return op;
        }
        default: assert(false);
      }
      break;
    }
    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = (Ast_Binary_Op *)node;
      switch(n->operator) {
        case OPPLUS:
        case OPMINUS:
        case OPMUL:
        case OPDIV: {
          Type first = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type second = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(first.info->type == TYPE_POISON || second.info->type == TYPE_POISON) return POISON_TYPE;

          if((first.info->type != TYPE_INT && first.info->type != TYPE_UNKNOWN_INT) ||
             (second.info->type != TYPE_INT && second.info->type != TYPE_UNKNOWN_INT))
            type_inference_error("The operands to an arithmetic operator must be integers.", node->loc, unit_poisoned);

          if(first.info->type == TYPE_UNKNOWN_INT && second.info->type == TYPE_UNKNOWN_INT) {
            s64 a = first.info->data.unknown_int;
            s64 b = second.info->data.unknown_int;
            s64 c;
            switch(n->operator) {
              case OPPLUS: c = a+b; break;
              case OPMINUS: c = a-b; break;
              case OPMUL: c = a*b; break;
              case OPDIV: c = a/b; break;
              default: assert(false);
            }
            Type to = allocate_unknown_int_type(c);
            n->convert_to = solidify_type(to, *node);
            return to;
          }
          if(can_implicitly_cast_type(first, second)) {
            n->convert_to = solidify_type(second, *node);
            return second;
          }
          if(can_implicitly_cast_type(second, first)) {
            n->convert_to = solidify_type(first, *node);
            return first;
          }
          error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
          n->convert_to = POISON_TYPE;
          return POISON_TYPE;
        }

        case OPEQUALS: // this needs to be handled by its own code later.
        case OPLESS_THAN:
        case OPLESS_THAN_OR_EQUAL_TO:
        case OPGREATER_THAN:
        case OPGREATER_THAN_OR_EQUAL_TO: {
          Type first = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type second = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(first.info->type == TYPE_POISON || second.info->type == TYPE_POISON) return POISON_TYPE;
          if((first.info->type != TYPE_INT && first.info->type != TYPE_UNKNOWN_INT) ||
             (second.info->type != TYPE_INT && second.info->type != TYPE_UNKNOWN_INT))
            type_inference_error("The operands to a comparison operator must be integers.", node->loc, unit_poisoned);
          if(can_implicitly_cast_type(first, second))
            n->convert_to = solidify_type(second, *node);
          else if(can_implicitly_cast_type(second, first))
            n->convert_to = solidify_type(first, *node);
          else {
            error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
            n->convert_to = POISON_TYPE;
            return POISON_TYPE;
          }
          return BOOL_TYPE;
        }

        case OPSTRUCT_MEMBER:
        case OPSTRUCT_MEMBER_REF: {
          Type first = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          if(first.info->type == TYPE_POISON) return POISON_TYPE;
          if(first.info->type != TYPE_STRUCT) {
            type_inference_error("The first operand of a '.' operator must be a struct.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
          if(n->second->type != NODE_SYMBOL) {
            type_inference_error("The second operand of a '.' operator must be a symbol.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
          symbol sym = ((Ast_Symbol *)n->second)->symbol;
          Struct_Member_Array members = first.info->data.struct_.members;
          for(int i = 0; i < members.length; i++) {
            Struct_Member member = members.data[i];
            if(member.symbol == sym) {
              Compilation_Unit *unit = member.unit;
              assert(unit->type == UNIT_STRUCT_MEMBER);
              infer_types_of_compilation_unit(unit);
              if(n->operator == OPSTRUCT_MEMBER_REF) unit->data.struct_member.type.reference_count++;
              return unit->data.struct_member.type;
            }
          }

          type_inference_error(NULL, node->loc, unit_poisoned);
          print_type(first);
          printf(" does not have a member with the name \"");
          print_symbol(sym);
          printf("\".\n");

          return POISON_TYPE;
        }

        case OPSET_EQUALS: {
          if(scope->type != BLOCK_SCOPE) {
            type_inference_error("You cannot use a set statement outside of a block.", node->loc, unit_poisoned);
          }
          Type left = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type right = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(left.info->type == TYPE_POISON || right.info->type == TYPE_POISON) {
            n->convert_to = POISON_TYPE;
            return POISON_TYPE;
          }
          if(left.reference_count >= right.reference_count && can_implicitly_cast_type_info(right.info, left.info)) {
            n->convert_to = solidify_type(left, *node);
            n->convert_to.reference_count = 0;
            if(n->convert_to.info->type == TYPE_POISON) *unit_poisoned = true;
            return left;
          }
          error_cannot_implicitly_cast(right, left, node->loc, false, unit_poisoned);
          n->convert_to = POISON_TYPE;
          return POISON_TYPE;
        }

        default:
          type_inference_error("I cannot infer the type of this expression yet.", node->loc, unit_poisoned);
          exit(1);
      }
      break;
    }
    case NODE_SYMBOL: {
      return get_type_of_identifier_in_scope(((Ast_Symbol*)node)->symbol, scope, node->loc, unit_poisoned);
    }
    case NODE_BLOCK: {
      return infer_types_of_block(node, unit, using_result, unit_poisoned);
    }
    case NODE_RETURN: {
      Ast_Return *n = (Ast_Return *)node;
      Type value = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
      if(value.info->type == TYPE_POISON) return POISON_TYPE;
      if(unit->type != UNIT_FUNCTION_BODY) {
        type_inference_error("Cannot return outside of a function body", node->loc, unit_poisoned);
        return POISON_TYPE;
      }
      Ast_Function_Definition *fn_def = (Ast_Function_Definition *)unit->node;
      if(!can_implicitly_cast_type(value, fn_def->return_type)) {
        error_cannot_implicitly_cast(value, fn_def->return_type, node->loc, false, unit_poisoned);
        return POISON_TYPE;
      }
      return fn_def->return_type;
    }
    case NODE_IF: {
      Ast_If *n = (Ast_If *)node;
      n->result_is_used = using_result;
      Type cond = infer_type_of_expr(n->cond, scope, unit, true, unit_poisoned);
      if(cond.info->type != TYPE_BOOL && cond.info->type != TYPE_POISON) type_inference_error("The conditional of an if statement must be a boolean value.", n->cond->loc, unit_poisoned);
      Type first = infer_type_of_expr(n->first, scope, unit, using_result, unit_poisoned);
      Type second = infer_type_of_expr(n->second, scope, unit, using_result, unit_poisoned);
      if(using_result) {
        if(first.info->type == TYPE_POISON || second.info->type == TYPE_POISON) return POISON_TYPE;
        if(can_implicitly_cast_type(first, second)) {
          n->result_type = second;
          return second;
        }
        if(can_implicitly_cast_type(second, first)) {
          n->result_type = first;
          return first;
        }
        error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
        return POISON_TYPE;
      }
      return NOTHING_TYPE;
    }

    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      Scope_Entry *e;
      Scope *found_scope;
      {
        Ast_Symbol *sym = (Ast_Symbol *)n->identifier;
        assert(sym->n.type == NODE_SYMBOL);
        // Ignoring overriding functions and shadowing variables for now.
        e = get_entry_of_identifier_in_scope(sym->symbol, scope, sym->n.loc, &found_scope);
        assert(found_scope->type == FILE_SCOPE);
      }

      Compilation_Unit *unit = e->declaration.unit;
      assert(unit->type == UNIT_FUNCTION_SIGNATURE);
      n->signature = unit;
      infer_types_of_compilation_unit(unit);

      Ast_Function_Definition *def = (Ast_Function_Definition *)unit->node;
      if(n->arguments.length != def->parameters.length) {
        type_inference_error(NULL, node->loc, unit_poisoned);
        printf("I expected %i arguments to this function, but got %i instead.\n", def->parameters.length,n->arguments.length);
      } else {
        for(int i = 0; i < n->arguments.length; i++) {
          Type defined = ((Ast_Function_Parameter *)def->parameters.data[i])->type;
          Type passed = infer_type_of_expr(n->arguments.data[i], scope, unit, true, unit_poisoned);
          if(!can_implicitly_cast_type(passed, defined))
            error_cannot_implicitly_cast(passed, defined, n->arguments.data[i]->loc, false, unit_poisoned);
        }
      }

      return def->return_type;
    }

    case NODE_NULL: {
      return NOTHING_TYPE;
    }

    default: {
      type_inference_error("I cannot infer the type of this expression yet.", node->loc, unit_poisoned);
      return POISON_TYPE;
    }
  }
}

Type infer_types_of_block(Ast_Node *node_block, Compilation_Unit *unit, bool using_result, bool *unit_poisoned) {
  assert(node_block->type == NODE_BLOCK);
  Type last_statement_type = NOTHING_TYPE;
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
        last_statement_type = infer_type_of_expr(node, &block->scope, unit, using_this_result, unit_poisoned);
        break;
      }

      case NODE_WHILE: {
        last_statement_type = NOTHING_TYPE;
        Ast_While *n = (Ast_While *)node;
        Type cond = infer_type_of_expr(n->cond, &block->scope, unit, true, unit_poisoned);

        if(!can_implicitly_cast_type(cond, BOOL_TYPE))
          error_cannot_implicitly_cast(cond, BOOL_TYPE, n->n.loc, false, unit_poisoned);

        Type body = infer_type_of_expr(n->body, &block->scope, unit, false, unit_poisoned);
        break;
      }

      case NODE_TYPED_DECL:
      case NODE_UNTYPED_DECL_SET:
      case NODE_TYPED_DECL_SET: {
        last_statement_type = infer_type_of_decl(node, &block->scope, unit, unit_poisoned);
        break;
      }

      case NODE_NULL: {
        break;
      }

      case NODE_FUNCTION_DEFINITION: {
        last_statement_type = POISON_TYPE;
        type_inference_error("Function definitions are not allowed in a function definition.",
                             node->loc, unit_poisoned);
        *unit_poisoned = true;
        last_statement_type = POISON_TYPE;
        break;
      }

      case NODE_STRUCT_DEFINITION: {
        last_statement_type = POISON_TYPE;
        type_inference_error("Struct definitions are not allowed in a function definition.",
                             node->loc, unit_poisoned);
        *unit_poisoned = true;
        break;
      }

      case NODE_PRIMITIVE_TYPE:
      default: {
        last_statement_type = POISON_TYPE;
        type_inference_error("Internal compiler error: I cannot infer the type of this node.",
                             node->loc, unit_poisoned);
        *unit_poisoned = true;
        break;
      }
    }
  }
  return using_result ? last_statement_type : NOTHING_TYPE;
}

Type infer_type_of_function_signature(Ast_Function_Definition *node, Scope *scope, bool *unit_poisoned) {
  node->return_type = type_of_type_expr(node->return_type_node, scope, unit_poisoned);
  for(int i = 0; i < node->parameters.length; i++) {
    Ast_Function_Parameter *param = (Ast_Function_Parameter *)node->parameters.data[i];
    assert(param->n.type == NODE_FUNCTION_PARAMETER);
    param->type = type_of_type_expr(param->type_node, scope, unit_poisoned);
  }

  return NOTHING_TYPE; // placeholder
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
      if(infer_type_of_function_signature((Ast_Function_Definition *)unit->node, unit->data.signature.scope, &unit->poisoned).info->type == TYPE_POISON) {
        unit->poisoned = true;
      }
      break;
    }
    case UNIT_FUNCTION_BODY: {
      Compilation_Unit *sig = unit->data.body.signature;
      assert(sig->type == UNIT_FUNCTION_SIGNATURE);
      infer_types_of_compilation_unit(sig);
      if(sig->poisoned) {
        unit->poisoned = true;
      } else {
        Ast_Function_Definition *node = (Ast_Function_Definition *)unit->node;
        if(infer_types_of_block(node->body, unit, false, &unit->poisoned).info->type == TYPE_POISON)
          unit->poisoned = true;
      }
      break;
    }
    case UNIT_STRUCT: {
      Type type = infer_type_of_struct_definition((Ast_Struct_Definition *)unit->node, unit->data.struct_def.scope, &unit->poisoned);
      unit->data.struct_def.type = type;
      break;
    }
    case UNIT_STRUCT_MEMBER: {
      Ast_Node *node = unit->node;
      if(node->type == NODE_TYPED_DECL) {
        Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
        unit->data.struct_member.type = type_of_type_expr(n->type_node, unit->data.struct_member.scope, &unit->poisoned);
      } else if(node->type == NODE_TYPED_DECL_SET) {
        Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
        unit->data.struct_member.type = type_of_type_expr(n->type_node, unit->data.struct_member.scope, &unit->poisoned);
      } else assert(false);
      break;
    }
    default:
      print_error_message("Internal compiler error: Unknown compilation unit type.", NULL_LOCATION);
      exit(1);
  }
  unit->type_inferred = true;
}
