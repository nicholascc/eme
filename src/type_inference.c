#include "type_inference.h"

#include <stdio.h>

#include "c-utils/hash.h"
#include "ast.h"
#include "bytecode.h"
#include "errors.h"
#include "files.h"



void type_inference_error(char *message, Location l, bool *unit_poisoned) {
  print_error_message(message, l);
  *unit_poisoned = true;
}

void error_cannot_implicitly_cast(Type a, Type b, Location loc, bool cast_either_way, bool *unit_poisoned) {
  if(a.info->type == TYPE_POISON || b.info->type == TYPE_POISON) {
    *unit_poisoned = true;
    return;
  }
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

Type solidify_type(Type x) {
  switch(x.info->type) {
    case TYPE_UNKNOWN_INT:
      return INT_TYPE;

    case TYPE_INT:
    case TYPE_BOOL:
    case TYPE_STRUCT:
    case TYPE_POLY_INSTANCE:
    case TYPE_UNIQUE:
      return x;

    default:
      print_type_info(x);
      print_error_message("I don't know how to make this type concrete.", NULL_LOCATION);
      return POISON_TYPE;
  }
}





// Used to call try_function_call
typedef enum Argument_Type {
  ARGUMENT_NON_TYPE,
  ARGUMENT_TYPE
} Argument_Type;

typedef struct Argument {
  Argument_Type type;
  union {
    Type non_type; // TYPE of passed
    Type type; // VALUE matching
  } data;
} Argument;

// return_type is set if the function returns true.
bool try_function_call(Argument *arguments, int argument_count, bool is_operator, bool is_export, Compilation_Unit *unit, Type *return_type, Compilation_Unit **to_call, bool *unit_poisoned);

bool call_exported_function(symbol identifier, Argument *arguments, int argument_count, bool is_operator, Compilation_Unit *unit, Type *return_type, Compilation_Unit **to_call, bool *unit_poisoned) {
  assert(unit->type == UNIT_IMPORT);
  type_infer_compilation_unit(unit);

  Compilation_Unit *module = unit->data.import.module;
  assert(module->type == UNIT_MODULE);
  assert(module->data.module.scope.type == UNIT_SCOPE);

  for(int i = 0; i < module->data.module.scope.entries.length; i++) {
    Scope_Entry e = module->data.module.scope.entries.data[i];
    Compilation_Unit *u = e.data.unit.unit;
    if(e.symbol == identifier) {
      bool matches = try_function_call(arguments, argument_count, is_operator, true, u, return_type, to_call, NULL);
      if(matches)
        return true;
    }
  }
  return false;
}

// doesn't free arguments
bool call_function(symbol identifier, Argument *arguments, int argument_count, bool is_operator, Type *return_type, Compilation_Unit **body, Scope *scope) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry e = scope->entries.data[i];
      if(scope->type == UNIT_SCOPE) {
        Compilation_Unit *u = e.data.unit.unit;
        if(e.symbol == identifier) {
          bool matches = try_function_call(arguments, argument_count, is_operator, false, u, return_type, body, NULL);
          if(matches)
            return true;
        } else if(e.symbol == st_get_id_of("*",-1)) {
          bool matches = call_exported_function(identifier, arguments, argument_count, is_operator, u, return_type, body, NULL);
          if(matches)
            return true;
        }
      }
    }

    if(scope->has_parent)
      scope = scope->parent;
    else
      break;
  }
  return false;
}

bool try_get_entry_of_exported_identifier(symbol symbol, Compilation_Unit *unit, Scope **found_scope, Scope_Entry **found_entry) {
  assert(unit->type == UNIT_IMPORT);
  type_infer_compilation_unit(unit);

  Compilation_Unit *module = unit->data.import.module;
  assert(module->type == UNIT_MODULE);
  assert(module->data.module.scope.type == UNIT_SCOPE);

  for(int i = 0; i < module->data.module.scope.entries.length; i++) {
    Scope_Entry *e = &module->data.module.scope.entries.data[i];
    if(e->symbol == symbol) {
      *found_scope = &module->data.module.scope;
      *found_entry = e;
      return true;
    }
  }
  return false;
}

bool try_get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Scope **found_scope, Scope_Entry **found_entry) {
  while(true) {
    for(int i = 0; i < scope->entries.length; i++) {
      Scope_Entry *e = &scope->entries.data[i];
      if(scope->type == UNIT_SCOPE) {
        if(e->symbol == symbol) {
          *found_scope = scope;
          *found_entry = e;
          return true;
        } else if(e->symbol == st_get_id_of("*", -1)) {
          bool matches = try_get_entry_of_exported_identifier(symbol, e->data.unit.unit, found_scope, found_entry);
          if(matches) return true;
        }
      } else if(e->symbol == symbol) {
        *found_scope = scope;
        *found_entry = e;
        return true;
      }
    }

    if(scope->has_parent)
      scope = scope->parent;
    else
      break;
  }
  return false;
}

// DO NOT HOLD THE RETURNED POINTER!
Scope_Entry *get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Location error_location, Scope **found_scope) {
  Scope_Entry *e;
  if(try_get_entry_of_identifier_in_scope(symbol, scope, found_scope, &e)) {
    return e;
  } else {
    print_error_message("This symbol is undefined in this scope.", error_location);
    exit(1);
  }
}

Type evaluate_type_expr(Ast_Node *node, Scope *scope, Compilation_Unit *unit, bool *unit_poisoned) {
  switch(node->type) {
    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPREFERENCE: {
          Type t = evaluate_type_expr(n->operand, scope, unit, unit_poisoned);
          t.reference_count++;
          return t;
        }
        default:
          assert(false);
      }
    }
    case NODE_SYMBOL: {
      Ast_Symbol *n = (Ast_Symbol *)node;
      Scope *found_scope;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, n->n.loc, &found_scope);
      if(found_scope->type == UNIT_SCOPE) {
        Compilation_Unit *unit = e->data.unit.unit;
        Type unit_type = infer_type_of_compilation_unit(unit);
        if(unit_type.info->type == TYPE_POISON) return POISON_TYPE;
        else if(unit_type.info->type != TYPE_FTYPE) {
          type_inference_error("I expected this to be a type; instead it something else.", n->n.loc, unit_poisoned);
          return POISON_TYPE;
        }
        if(unit->type == UNIT_STRUCT) {
          return unit->data.struct_def.type;
        } else if(unit->type == UNIT_UNIQUE) {
          return unit->data.unique.type;
        } else assert(false);
      } else if(found_scope->type == REGISTER_SCOPE) {
        type_inference_error("I cannot use a type declared in a block scope.", n->n.loc, unit_poisoned);
        return POISON_TYPE;
      } else if(found_scope->type == TYPE_SCOPE) {
        return e->data.type.value;
      } else assert(false);
    }
    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      symbol identifier;
      {
        Ast_Symbol *sym = (Ast_Symbol *)n->identifier;
        assert(sym->n.type == NODE_SYMBOL);
        identifier = sym->symbol;
      }
      // check for built-ins like type_of
      if(identifier == st_get_id_of("type_of", -1)) {
        if(n->arguments.length != 1) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I expected 1 argument to this function, but got %i instead.\n", n->arguments.length);
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        return infer_type_of_expr(n->arguments.data[0], scope, unit, true, unit_poisoned);
      }

      Compilation_Unit *definition = n->body;
      assert(definition->type == UNIT_POLY_STRUCT);
      type_infer_compilation_unit(definition);

      Ast_Poly_Struct_Definition *def = (Ast_Poly_Struct_Definition *)definition->node;
      assert(n->arguments.length == def->parameters.length);

      // hash the instance
      u64 hash;
      for(int i = 0; i < n->arguments.length; i++) {
        Ast_Node *p = n->arguments.data[i];
        Type t = evaluate_type_expr(p, scope, unit, unit_poisoned);
        if(i == 0) hash = hash_type(t);
        else hash = hash_combine(hash, hash_type(t));
      }

      // ...and check if it's already in the table of already made instances.
      {
        Type r;
        if(get_Type_Table(&definition->data.poly_struct_def.instances, hash, &r)) {
          return r;
        }
      }

      // otherwise we make it ourselves.

      Type_Info *info = malloc(sizeof(Type_Info));
      info->type = TYPE_POLY_INSTANCE;
      info->sized = false;
      info->sizing_seen = false;
      info->size = 0;
      info->data.poly_instance.definition_unit = definition;
      info->data.poly_instance.definition = def;
      {
        Scope *s = malloc(sizeof(Scope));
        s->type = TYPE_SCOPE;
        s->has_parent = true;
        s->parent = scope;
        s->entries = init_Scope_Entry_Array(2);
        info->data.poly_instance.scope = s;
      }
      info->data.poly_instance.members = init_Compilation_Unit_Ptr_Array(def->members.length);
      info->data.poly_instance.llvm_generated = false;

      for(int i = 0; i < n->arguments.length; i++) {
        Scope_Entry param_entry = def->param_scope.entries.data[i];
        {
          Ast_Passed_Parameter *param = (Ast_Passed_Parameter *)def->parameters.data[i];
          assert(param->n.type == NODE_PASSED_PARAMETER);
          assert(param->symbol == param_entry.symbol);
        }
        Scope_Entry e;
        e.symbol = param_entry.symbol;
        if(param_entry.data.basic.type.info->type == TYPE_FTYPE) {
          e.data.type.value = evaluate_type_expr(n->arguments.data[i], scope, unit, unit_poisoned);
        } else if(param_entry.data.basic.type.info->type == TYPE_POISON) {
          *unit_poisoned = true;
        } else assert(false);
        Scope_Entry_Array_push(&info->data.poly_instance.scope->entries, e);
      }

      for(int i = 0; i < def->members.length; i++) {
        Compilation_Unit *unit = allocate_null_compilation_unit();
        unit->type = UNIT_STRUCT_MEMBER;
        unit->type_inferred = false;
        unit->type_inference_seen = false;
        unit->bytecode_generated = false;
        unit->bytecode_generating = false;
        unit->poisoned = false;
        unit->node = def->members.data[i];
        unit->scope = info->data.poly_instance.scope;
        unit->data.struct_member.offset = -1;
        unit->data.struct_member.type = UNKNOWN_TYPE;

        if(unit->node->type == NODE_TYPED_DECL) {
          Ast_Typed_Decl *n = (Ast_Typed_Decl *)unit->node;
          unit->data.struct_member.symbol = n->symbol;
        } else if(unit->node->type == NODE_TYPED_DECL_SET) {
          Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)unit->node;
          unit->data.struct_member.symbol = n->symbol;
        } else assert(false);

        Compilation_Unit_Ptr_Array_push(&info->data.poly_instance.members, unit);
      }

      Type t = {0, info};
      assert(set_Type_Table(&definition->data.poly_struct_def.instances, hash, t) && "the type was not already in the table");
      return t;
    }
    case NODE_BIND_SYMBOL: {
      Ast_Bind_Symbol *n = (Ast_Bind_Symbol *)node;
      Scope *found_scope;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, n->n.loc, &found_scope);
      return e->data.type.value;
    }
    case NODE_PRIMITIVE_TYPE: {
      Ast_Primitive_Type *n = (Ast_Primitive_Type *) node;
      return n->type;
    }
    default:
      type_inference_error("I cannot evaluate this as a type expression.", node->loc, unit_poisoned);
      exit(1);
  }
}

Type type_of_type_expr(Ast_Node *node, Scope *scope, Compilation_Unit *unit, Location loc,  bool *unit_poisoned) {
  Type type = infer_type_of_expr(node, scope, unit, true, unit_poisoned);
  if(type.reference_count != 0 || type.info->type != TYPE_FTYPE) {
    error_cannot_implicitly_cast(type, FTYPE_TYPE, loc, false, unit_poisoned);
    return POISON_TYPE;
  }
  return evaluate_type_expr(node, scope, unit, unit_poisoned);
}

Type infer_type_of_decl(Ast_Node *decl, Scope *scope, Compilation_Unit *unit, bool *unit_poisoned) {
  if(decl->type == NODE_TYPED_DECL) {
    Ast_Typed_Decl *n = (Ast_Typed_Decl *)decl;
    Scope *found_scope;
    Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, decl->loc, &found_scope);
    assert(found_scope->type == REGISTER_SCOPE);

    if(e->data.reg.type.info->type == TYPE_UNKNOWN) e->data.reg.type = type_of_type_expr(n->type_node, scope, unit, decl->loc, unit_poisoned);
    return e->data.reg.type;

  } else if(decl->type == NODE_TYPED_DECL_SET) {
    Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)decl;
    Scope *found_scope;
    Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, decl->loc, &found_scope);
    assert(found_scope->type == REGISTER_SCOPE);

    if(e->data.reg.type.info->type != TYPE_UNKNOWN) return e->data.reg.type;

    Type inferred_type = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
    Type given_type = type_of_type_expr(n->type_node, scope, unit, decl->loc, unit_poisoned);

    if(inferred_type.info->type == TYPE_POISON || given_type.info->type == TYPE_POISON) {
      e->data.reg.type = POISON_TYPE;
      return POISON_TYPE;
    } else if(can_implicitly_cast_type(inferred_type, given_type)) {
      e->data.reg.type = given_type;
      return given_type;
    } else {
      error_cannot_implicitly_cast(inferred_type, given_type, n->n.loc, false, unit_poisoned);
      e->data.reg.type = POISON_TYPE;
      return POISON_TYPE;
    }

  } else if(decl->type == NODE_UNTYPED_DECL_SET) {
    Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)decl;
    Scope *found_scope;
    Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, decl->loc, &found_scope);
    assert(found_scope->type == REGISTER_SCOPE);
    if(e->data.reg.type.info->type != TYPE_UNKNOWN) return e->data.reg.type;

    Type inferred_type = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
    e->data.reg.type = solidify_type(inferred_type);
    if(e->data.reg.type.info->type == TYPE_POISON) *unit_poisoned = true;
    return e->data.reg.type;

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
  info->data.struct_.members = init_Compilation_Unit_Ptr_Array(2);
  info->data.struct_.definition = def;
  info->data.struct_.llvm_generated = false;
  info->data.struct_.name = def->symbol;
  info->sized = false;
  info->sizing_seen = false;
  info->size = 0;

  for(int i = 0; i < def->members.length; i++) {
    Compilation_Unit *unit = def->members.data[i];
    Ast_Node *node = unit->node;
    unit->data.struct_member.offset = -1;
    if(node->type == NODE_TYPED_DECL) {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      unit->data.struct_member.symbol = n->symbol;
    } else if(node->type == NODE_TYPED_DECL_SET) {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      unit->data.struct_member.symbol = n->symbol;
    } else assert(false);

    Compilation_Unit_Ptr_Array_push(&info->data.struct_.members, unit);
  }

  Type type = {0,info};
  def->type = type;
  return type;
}

void infer_types_of_function_parameters(Ast_Function_Definition *def, Compilation_Unit *unit, bool *unit_poisoned) {
  assert(def->parameter_scope.type == REGISTER_SCOPE);
  int passed_i = 0;
  int matched_i = 0;
  for(int i = 0; i < def->parameters.length; i++) {
    Ast_Node *node = def->parameters.data[i];
    if(node->type == NODE_PASSED_PARAMETER) {
      Ast_Passed_Parameter *p = (Ast_Passed_Parameter *)node;
      assert(p->symbol == def->parameter_scope.entries.data[passed_i].symbol);
      Type t = type_of_type_expr(p->type_node, &def->parameter_scope, unit, p->type_node->loc, unit_poisoned);
      p->type = t;
      def->parameter_scope.entries.data[passed_i].data.reg.type = t;
      passed_i++;
    } else if(node->type == NODE_MATCHED_PARAMETER) {
      Ast_Matched_Parameter *p = (Ast_Matched_Parameter *)node;
      p->value = type_of_type_expr(p->node, &def->parameter_scope, unit, node->loc, unit_poisoned);
      matched_i++;
    } else assert(false);
  }
  def->passed_param_count = passed_i;
  def->matched_param_count = matched_i;
  assert(passed_i + matched_i == def->parameters.length);
}


void infer_type_of_poly_struct_definition(Ast_Poly_Struct_Definition *def, Compilation_Unit *unit, bool *unit_poisoned) {
  Scope *param_scope = &def->param_scope;
  Ast_Node_Ptr_Array parameters = def->parameters;
  assert(param_scope->entries.length == parameters.length);
  assert(param_scope->type == BASIC_SCOPE);

  for(int i = 0; i < parameters.length; i++) {
    Ast_Passed_Parameter *param = (Ast_Passed_Parameter *)parameters.data[i];
    assert(param->n.type == NODE_PASSED_PARAMETER);
    assert(param->symbol == param_scope->entries.data[i].symbol);

    Type t = type_of_type_expr(param->type_node, param_scope, unit, param->type_node->loc, unit_poisoned);
    if(t.info->type != TYPE_FTYPE) {
      type_inference_error("The parameters of a polymorphic struct must be of type 'type'.", param->n.loc, unit_poisoned);
      t = POISON_TYPE;
    }
    param_scope->entries.data[i].data.basic.type = t;
  }
}

Type get_type_of_identifier_in_scope(symbol symbol, Scope *scope, Location error_location, bool *unit_poisoned) {
  Scope *found_scope;
  Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, error_location, &found_scope);

  if(found_scope->type == UNIT_SCOPE)
    return infer_type_of_compilation_unit(e->data.unit.unit);
  else if(found_scope->type == REGISTER_SCOPE) {
    assert(e->data.reg.type.info->type != TYPE_UNKNOWN);
    return e->data.reg.type;
  } else if(found_scope->type == BASIC_SCOPE) {
    return e->data.basic.type;
  } else if(found_scope->type == TYPE_SCOPE) {
    return FTYPE_TYPE;
  } else if(found_scope->type == SYMBOL_SCOPE) {
    return FTYPE_TYPE;
  } else {
    print_scope(*found_scope, true);
    assert(false);
  }
}

// result is whether or not it matches
bool assign_bound_type_to_poly_function_arguments(Scope *scope, Scope *instance_scope, Ast_Node *node, Type passed_type) {
  switch(node->type) {
    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPREFERENCE: {
          if(passed_type.reference_count > 0) {
            passed_type.reference_count--;
            return assign_bound_type_to_poly_function_arguments(scope, instance_scope, n->operand, passed_type);
          } else return false;
        }
        default: {
          assert(false);
        }
      }
    }
    case NODE_BIND_SYMBOL: {
      Ast_Bind_Symbol *n = (Ast_Bind_Symbol *)node;
      symbol sym = n->symbol;
      assert(scope->type == SYMBOL_SCOPE);
      assert(instance_scope->type == TYPE_SCOPE);
      assert(scope->entries.length == instance_scope->entries.length);
      for(int i = 0; i < scope->entries.length; i++) {
        assert(scope->entries.data[i].symbol == instance_scope->entries.data[i].symbol);
        if(scope->entries.data[i].symbol == sym) {
          instance_scope->entries.data[i].data.type.value = passed_type;
          return true;
        }
      }
      assert(false);
    }
    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      if(passed_type.reference_count > 0) return false;
      if(passed_type.info->type != TYPE_POLY_INSTANCE) return false;

      assert(passed_type.info->data.poly_instance.scope->type == TYPE_SCOPE);
      assert(passed_type.info->data.poly_instance.scope->entries.length == n->arguments.length);

      Argument *arguments = malloc(n->arguments.length * sizeof(Argument));
      for(int i = 0; i < n->arguments.length; i++) {
        Scope_Entry e = passed_type.info->data.poly_instance.scope->entries.data[i];
        if(!assign_bound_type_to_poly_function_arguments(scope, instance_scope, n->arguments.data[i], e.data.type.value)) {
          free(arguments);
          return false;
        }
        Argument a;
        a.type = ARGUMENT_TYPE;
        a.data.type = e.data.type.value;
        arguments[i] = a;
      }

      symbol identifier;
      {
        Ast_Symbol *sym = (Ast_Symbol *)n->identifier;
        assert(sym->n.type == NODE_SYMBOL);
        identifier = sym->symbol;
      }

      {
        Type return_type;
        Compilation_Unit *body;
        if(call_function(identifier, arguments, n->arguments.length, false, &return_type, &body, scope)) {
          free(arguments);
          n->return_type = return_type;
          n->body = body;
          return body == passed_type.info->data.poly_instance.definition_unit;
        } else {
          free(arguments);
          return false;
        }
      }
    }
    case NODE_SYMBOL: {
      Ast_Symbol *n = (Ast_Symbol *)node;
      Scope *found_scope;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, n->n.loc, &found_scope);
      if(found_scope->type == UNIT_SCOPE) {
        Compilation_Unit *unit = e->data.unit.unit;
        Type unit_type = infer_type_of_compilation_unit(unit);
        assert(unit_type.info->type == TYPE_FTYPE);
        assert(unit->type == UNIT_STRUCT);
        return type_equals(unit->data.struct_def.type, passed_type);
      } else if(found_scope->type == REGISTER_SCOPE) {
        assert(false);
      } else if(found_scope->type == SYMBOL_SCOPE) {
        return true;
      } else assert(false);
    }
    case NODE_PRIMITIVE_TYPE: {
      Ast_Primitive_Type *n = (Ast_Primitive_Type *)node;
      return type_equals(passed_type, n->type);
    }
    default: {
      assert(false);
    }
  }
}

bool try_function_call(Argument *arguments, int argument_count, bool is_operator, bool is_export, Compilation_Unit *unit, Type *return_type, Compilation_Unit **to_call, bool *unit_poisoned) {
  if(unit->type == UNIT_FUNCTION_SIGNATURE) {
    type_infer_compilation_unit(unit);

    Ast_Function_Definition *def = (Ast_Function_Definition *)unit->node;
    if(argument_count != def->parameters.length || (is_operator && !def->is_operator) || (is_export && !def->is_export))
      return false;
    else {
      for(int i = 0; i < argument_count; i++) {
        Ast_Node *param = def->parameters.data[i];
        if(param->type == NODE_PASSED_PARAMETER) {
          if(arguments[i].type != ARGUMENT_NON_TYPE) return false;
          Ast_Passed_Parameter *p = (Ast_Passed_Parameter *)param;
          if(!can_implicitly_cast_type(arguments[i].data.non_type, p->type))
            return false;
        } else if(param->type == NODE_MATCHED_PARAMETER) {
          if(arguments[i].type != ARGUMENT_TYPE) return false;
          Ast_Matched_Parameter *p = (Ast_Matched_Parameter *)param;
          if(!type_equals(arguments[i].data.type, p->value))
            return false;
        } else assert(false);
      }
    }
    *return_type = def->return_type;
    *to_call = unit->data.signature.body;
    return true;
  } else if(unit->node->type == NODE_FOREIGN_DEFINITION) {
    type_infer_compilation_unit(unit);
    Ast_Foreign_Definition *def = (Ast_Foreign_Definition *)unit->node;
    if(argument_count != def->parameter_types.length || is_operator || (is_export && !def->is_export))
      return false;
    else {
      for(int i = 0; i < argument_count; i++) {
        if(arguments[i].type != ARGUMENT_NON_TYPE)
          return false;
        Type defined = def->parameter_types.data[i];
        Type passed = arguments[i].data.non_type;
        if(!can_implicitly_cast_type(passed, defined))
          return false;
      }
    }
    *return_type = def->return_type;
    *to_call = unit;
    return true;
  } else if(unit->type == UNIT_POLY_FUNCTION) {
    type_infer_compilation_unit(unit);

    Ast_Function_Definition *def = (Ast_Function_Definition *)unit->node;
    if(argument_count != def->parameters.length || (is_operator && !def->is_operator) || (is_export && !def->is_export))
      return false;
    else {
      // we establish a 1:1 mapping from scope entries in the BOUND_TYPE_SCOPE scope to their actual types, placed in the BOUND_TYPE_INSTANCE_SCOPE
      Scope instance_scope;
      instance_scope.type = TYPE_SCOPE;
      instance_scope.has_parent = true;
      assert(def->bound_type_scope.has_parent);
      instance_scope.parent = def->bound_type_scope.parent;
      instance_scope.entries = init_Scope_Entry_Array(2);
      for(int i = 0; i < def->bound_type_scope.entries.length; i++) {
        Scope_Entry e;
        e.symbol = def->bound_type_scope.entries.data[i].symbol;
        e.data.type.value = UNKNOWN_TYPE;
        Scope_Entry_Array_push(&instance_scope.entries, e);
      }
      // then do the binding itself...
      for(int i = 0; i < def->parameters.length; i++) {
        bool this_argument_matches;

        if(arguments[i].type == ARGUMENT_NON_TYPE) {
          Ast_Passed_Parameter *param = (Ast_Passed_Parameter *)def->parameters.data[i];
          this_argument_matches = assign_bound_type_to_poly_function_arguments(&def->bound_type_scope, &instance_scope, param->type_node, solidify_type(arguments[i].data.non_type));
        } else if(arguments[i].type == ARGUMENT_TYPE) {
          Ast_Matched_Parameter *param = (Ast_Matched_Parameter *)def->parameters.data[i];
          this_argument_matches = assign_bound_type_to_poly_function_arguments(&def->bound_type_scope, &instance_scope, param->node, solidify_type(arguments[i].data.type));
        } else assert(false);

        if(!this_argument_matches) {
          free(instance_scope.entries.data);
          return false;
        }
      }

      // then recheck to ensure that everything matches, now that we know the types of all the $X expressions.
      for(int i = 0; i < argument_count; i++) {
        if(arguments[i].type == ARGUMENT_NON_TYPE) {
          Ast_Passed_Parameter *param = (Ast_Passed_Parameter *)def->parameters.data[i];
          Type defined = type_of_type_expr(param->type_node, &instance_scope, unit, def->parameters.data[i]->loc, unit_poisoned);
          Type passed = solidify_type(arguments[i].data.non_type);
          if(!can_implicitly_cast_type(passed, defined)) {
            free(instance_scope.entries.data);
            return false;
          }
        } else if(arguments[i].type == ARGUMENT_TYPE) {
          Ast_Matched_Parameter *param = (Ast_Matched_Parameter *)def->parameters.data[i];
          Type defined = type_of_type_expr(param->node, &instance_scope, unit, def->parameters.data[i]->loc, unit_poisoned);
          Type passed = solidify_type(arguments[i].data.type);
          if(!type_equals(passed, defined))
            return false;
        } else assert(false);
      }

      // if we've gotten this far, everything is good for actually calling this function.

      u64 hash;
      for(int i = 0; i < argument_count; i++) {
        Type t;
        if(arguments[i].type == ARGUMENT_NON_TYPE) {
          t = arguments[i].data.non_type;
        } else if(arguments[i].type == ARGUMENT_TYPE) {
          t = arguments[i].data.type;
        } else assert(false);
        if(i == 0) hash = hash_type(t);
        else hash = hash_combine(hash, hash_type(t));
      }

      // now we get the instance compilation unit, perhaps we have to make it ourselves.
      Compilation_Unit *instance;
      if(get_Compilation_Unit_Ptr_Table(&unit->data.poly_function_def.instances, hash, &instance)) {
        free(instance_scope.entries.data);
      } else {
        Ast_Function_Definition *new_def = (Ast_Function_Definition *)allocate_ast_node((Ast_Node *)def, sizeof(Ast_Function_Definition));

        instance = allocate_null_compilation_unit();
        instance->type = UNIT_FUNCTION_BODY;
        instance->type_inferred = false;
        instance->type_inference_seen = false;
        instance->bytecode_generated = false;
        instance->bytecode_generating = false;
        instance->poisoned = false;
        instance->node = (Ast_Node *)new_def;
        instance->scope = unit->scope;
        instance->data.body.signature = unit;
        instance->data.body.bytecode = allocate_bytecode_unit_type(BYTECODE_FUNCTION, sizeof(Bytecode_Function));

        set_Compilation_Unit_Ptr_Table(&unit->data.poly_function_def.instances, hash, instance);
        new_def->def_type = FN_POLY_INSTANCE;
        new_def->is_inline = def->is_inline;
        new_def->bound_type_scope = instance_scope;
        new_def->parameter_scope = copy_scope(def->parameter_scope, &new_def->bound_type_scope);

        new_def->parameters = copy_ast_node_ptr_array(def->parameters, &new_def->parameter_scope);
        infer_types_of_function_parameters(new_def, instance, &instance->poisoned);

        new_def->return_type_node = copy_ast_node(def->return_type_node, &new_def->parameter_scope);
        new_def->return_type = type_of_type_expr(new_def->return_type_node, &new_def->parameter_scope, instance, new_def->return_type_node->loc, &instance->poisoned);

        new_def->body = copy_ast_node(def->body, &new_def->parameter_scope);
      }

      Ast_Function_Definition *instance_def = (Ast_Function_Definition *)instance->node;
      *to_call = instance;
      *return_type = instance_def->return_type;
      return true;
    }
  } else if(unit->type == UNIT_POLY_STRUCT) {
    type_infer_compilation_unit(unit);

    Ast_Poly_Struct_Definition *def = (Ast_Poly_Struct_Definition *)unit->node;
    if(argument_count != def->parameters.length || is_operator || (is_export && !def->is_export))
      return false;
    else {
      for(int i = 0; i < argument_count; i++) {
        if(arguments[i].type != ARGUMENT_TYPE)
          return false;
      }
    }
    *return_type = FTYPE_TYPE;
    *to_call = unit;
    return true;
  } else assert(false);
}

Type infer_type_of_expr(Ast_Node *node, Scope *scope, Compilation_Unit *unit, bool using_result, bool *unit_poisoned) {
  switch(node->type) {
    case NODE_LITERAL:
      return ((Ast_Literal*)node)->type;
    case NODE_LITERAL_STRING: {
      Ast_Literal_String *n = (Ast_Literal_String *)node;

      Argument *arguments = malloc(2*sizeof(Argument));
      arguments[0].type = ARGUMENT_NON_TYPE;
      arguments[0].data.non_type = integer_type_with(false, 8);
      arguments[0].data.non_type.reference_count++;

      arguments[1].type = ARGUMENT_NON_TYPE;
      arguments[1].data.non_type = INT_TYPE;

      if(call_function(st_get_id_of("make_string_literal",-1), arguments, 2, true, &n->return_type, &n->make_body, scope)) {
        free(arguments);
        return n->return_type;
      } else {
        free(arguments);
        type_inference_error("I cannot find a function which implements the make_string_literal operator with these argument types.", node->loc, unit_poisoned);
        return POISON_TYPE;
      }
    }
    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPREFERENCE:
        case OPDEREFERENCE: {
          Type op = infer_type_of_expr(n->operand, scope, unit, true, unit_poisoned);
          if(op.info->type == TYPE_POISON) return POISON_TYPE;
          else if(op.info->type == TYPE_FTYPE) {
            if(n->operator == OPDEREFERENCE) {
              type_inference_error("I cannot use the dereference operator on a type.", n->n.loc, unit_poisoned);
              return POISON_TYPE;
            }
            return FTYPE_TYPE;
          }
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
            n->convert_to = solidify_type(to);
            return to;
          }
          if(can_implicitly_cast_type(first, second)) {
            n->convert_to = solidify_type(second);
            return second;
          }
          if(can_implicitly_cast_type(second, first)) {
            n->convert_to = solidify_type(first);
            return first;
          }
          error_cannot_implicitly_cast(second, first, node->loc, true, unit_poisoned);
          n->convert_to = POISON_TYPE;
          return POISON_TYPE;
        }

        case OPOR:
        case OPAND: {
          Type first = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type second = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(first.info->type == TYPE_POISON || second.info->type == TYPE_POISON) return POISON_TYPE;
          if(!can_implicitly_cast_type(first, BOOL_TYPE)) {
            error_cannot_implicitly_cast(first, BOOL_TYPE, node->loc, false, unit_poisoned);
            return POISON_TYPE;
          } else if(!can_implicitly_cast_type(second, BOOL_TYPE)) {
            error_cannot_implicitly_cast(second, BOOL_TYPE, node->loc, false, unit_poisoned);
            return POISON_TYPE;
          }
          return BOOL_TYPE;
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
            n->convert_to = solidify_type(second);
          else if(can_implicitly_cast_type(second, first))
            n->convert_to = solidify_type(first);
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
          if(n->second->type != NODE_SYMBOL) {
            type_inference_error("The second operand of a '.' operator must be a symbol.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
          symbol sym = ((Ast_Symbol *)n->second)->symbol;
          if(first.info->type == TYPE_STRUCT) {
            Compilation_Unit_Ptr_Array members = first.info->data.struct_.members;
            for(int i = 0; i < members.length; i++) {
              Compilation_Unit *unit = members.data[i];
              assert(unit->type == UNIT_STRUCT_MEMBER);
              if(unit->data.struct_member.symbol == sym) {
                type_infer_compilation_unit(unit);
                if(n->operator == OPSTRUCT_MEMBER_REF) unit->data.struct_member.type.reference_count++;
                return unit->data.struct_member.type;
              }
            }
          } else if(first.info->type == TYPE_POLY_INSTANCE) {
            Compilation_Unit_Ptr_Array members = first.info->data.poly_instance.members;
            for(int i = 0; i < members.length; i++) {
              Compilation_Unit *unit = members.data[i];
              assert(unit->type == UNIT_STRUCT_MEMBER);
              if(unit->data.struct_member.symbol == sym) {
                type_infer_compilation_unit(unit);
                Type r = unit->data.struct_member.type;
                if(n->operator == OPSTRUCT_MEMBER_REF) r.reference_count++;
                return r;
              }
            }
          } else {
            type_inference_error("The first operand of a '.' operator must be a struct.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }

          type_inference_error(NULL, node->loc, unit_poisoned);
          print_type(first);
          printf(" does not have a member with the name \"");
          print_symbol(sym);
          printf("\".\n");

          return POISON_TYPE;
        }

        case OPSET_EQUALS: {
          if(scope->type != REGISTER_SCOPE) {
            type_inference_error("You cannot use a set statement outside of a block.", node->loc, unit_poisoned);
          }
          Type left = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type right = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(left.info->type == TYPE_POISON || right.info->type == TYPE_POISON) {
            n->convert_to = POISON_TYPE;
            return POISON_TYPE;
          }
          if(left.reference_count >= right.reference_count && can_implicitly_cast_type_info(right.info, left.info)) {
            n->convert_to = solidify_type(left);
            n->convert_to.reference_count = 0;
            if(n->convert_to.info->type == TYPE_POISON) *unit_poisoned = true;
            return left;
          }
          error_cannot_implicitly_cast(right, left, node->loc, false, unit_poisoned);
          n->convert_to = POISON_TYPE;
          return POISON_TYPE;
        }

        case OPSUBSCRIPT_REF:
        case OPSUBSCRIPT: {
          Type first = infer_type_of_expr(n->first, scope, unit, true, unit_poisoned);
          Type second = infer_type_of_expr(n->second, scope, unit, true, unit_poisoned);
          if(first.info->type == TYPE_POISON || second.info->type == TYPE_POISON) return POISON_TYPE;

          Argument *arguments = malloc(2*sizeof(Argument));
          if(first.info->type == TYPE_FTYPE) {
            arguments[0].type = ARGUMENT_TYPE;
            arguments[0].data.type = evaluate_type_expr(n->first, scope, unit, unit_poisoned);
          } else {
            arguments[0].type = ARGUMENT_NON_TYPE;
            arguments[0].data.non_type = first;
          }

          if(second.info->type == TYPE_FTYPE) {
            arguments[1].type = ARGUMENT_TYPE;
            arguments[1].data.type = evaluate_type_expr(n->second, scope, unit, unit_poisoned);
          } else {
            arguments[1].type = ARGUMENT_NON_TYPE;
            arguments[1].data.non_type = second;
          }

          {
            Type return_type;
            Compilation_Unit *body;
            if(call_function(st_get_id_of("subscript",-1), arguments, 2, true, &return_type, &body, scope)) {
              free(arguments);
              if(return_type.reference_count == 0) {
                type_inference_error("A subscript operator function must return a pointer.", body->node->loc, unit_poisoned);
                return POISON_TYPE;
              }

              if(n->operator == OPSUBSCRIPT) return_type.reference_count--;
              n->convert_to = return_type;
              n->data.overload.body = body;
              return return_type;
            } else {
              free(arguments);
              type_inference_error("I cannot find a function which implements the subscript operator with these argument types.", node->loc, unit_poisoned);
              return POISON_TYPE;
            }
          }
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
      if(n->is_return_nothing) {
        Ast_Function_Definition *fn_def = (Ast_Function_Definition *)unit->node;
        if(fn_def->return_type.info->type != TYPE_NOTHING) {
          type_inference_error("You must provide a return value for this function.", node->loc, unit_poisoned);
          return POISON_TYPE;
        }
        return NOTHING_TYPE;
      } else {
        Type value = infer_type_of_expr(n->value, scope, unit, true, unit_poisoned);
        if(value.info->type == TYPE_POISON) return POISON_TYPE;
        if(unit->type != UNIT_FUNCTION_BODY) {
          type_inference_error("You cannot return outside of a function body.", node->loc, unit_poisoned);
          return POISON_TYPE;
        }
        Ast_Function_Definition *fn_def = (Ast_Function_Definition *)unit->node;
        if(!can_implicitly_cast_type(value, fn_def->return_type)) {
          error_cannot_implicitly_cast(value, fn_def->return_type, node->loc, false, unit_poisoned);
          return POISON_TYPE;
        }
        return fn_def->return_type;
      }
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
      symbol identifier;
      {
        Ast_Symbol *sym = (Ast_Symbol *)n->identifier;
        assert(sym->n.type == NODE_SYMBOL);
        identifier = sym->symbol;
      }

      // check for built-ins like size_of
      if(identifier == st_get_id_of("size_of", -1)) {
        if(n->arguments.length != 1) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I expected 1 argument to this function, but got %i instead.\n", n->arguments.length);
          n->return_type = POISON_TYPE;
          return n->return_type;
        }

        Type passed = infer_type_of_expr(n->arguments.data[0], scope, unit, true, unit_poisoned);
        if(!can_implicitly_cast_type(passed, FTYPE_TYPE)) {
          error_cannot_implicitly_cast(passed, FTYPE_TYPE, n->arguments.data[0]->loc, false, unit_poisoned);
        }
        Type value_passed = type_of_type_expr(n->arguments.data[0], scope, unit, n->arguments.data[0]->loc, unit_poisoned);
        n->return_type = allocate_unknown_int_type(size_of_type(value_passed));
        return INT_TYPE;
      } else if(identifier == st_get_id_of("type_of", -1)) {
        if(n->arguments.length != 1) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I expected 1 argument to this function, but got %i instead.\n", n->arguments.length);
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        return FTYPE_TYPE;
      } else if(identifier == st_get_id_of("bit_cast", -1)) {
        if(n->arguments.length != 2) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I expected 2 arguments to this function, but got %i instead.\n", n->arguments.length);
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        Type to = solidify_type(type_of_type_expr(n->arguments.data[0], scope, unit, n->arguments.data[0]->loc, unit_poisoned));
        Type from = solidify_type(infer_type_of_expr(n->arguments.data[1], scope, unit, true, unit_poisoned));
        if(size_of_type(to) != size_of_type(from)) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I cannot bit_cast from ");
          print_type(from);
          printf(" (%i bytes wide) -> ", size_of_type(from));
          print_type(to);
          printf(" (%i bytes wide) because they are of different sizes.\n", size_of_type(to));
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        n->return_type = to;
        return to;
      } else if(identifier == st_get_id_of("int_cast", -1)) {
        if(n->arguments.length != 2) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I expected 2 arguments to this function, but got %i instead.\n", n->arguments.length);
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        Type to = solidify_type(type_of_type_expr(n->arguments.data[0], scope, unit, n->arguments.data[0]->loc, unit_poisoned));
        Type from = solidify_type(infer_type_of_expr(n->arguments.data[1], scope, unit, true, unit_poisoned));
        if(to.reference_count > 0 || from.reference_count > 0 || to.info->type != TYPE_INT || from.info->type != TYPE_INT) {
          type_inference_error(NULL, node->loc, unit_poisoned);
          printf("I cannot int_cast from ");
          print_type(from);
          printf(" -> ", size_of_type(from));
          print_type(to);
          printf(" because they must both be integer types.\n", size_of_type(to));
          n->return_type = POISON_TYPE;
          return n->return_type;
        }
        n->return_type = to;
        return to;
      } else {
        // if we're calling a user-defined function of any type (e.g. polymorphic struct, foreign function, normal function, etc.)
        Argument *arguments = malloc(n->arguments.length * sizeof(Argument));
        for(int i = 0; i < n->arguments.length; i++) {
          Type passed = infer_type_of_expr(n->arguments.data[i], scope, unit, true, unit_poisoned);
          if(passed.info->type == TYPE_POISON) {
            free(arguments);
            return POISON_TYPE;
          } else if(passed.info->type == TYPE_FTYPE) {
            Argument a;
            a.type = ARGUMENT_TYPE;
            a.data.type = evaluate_type_expr(n->arguments.data[i], scope, unit, unit_poisoned);
            arguments[i] = a;
          } else {
            Argument a;
            a.type = ARGUMENT_NON_TYPE;
            a.data.non_type = passed;
            arguments[i] = a;
          }
        }
        {
          Type return_type;
          Compilation_Unit *body;
          if(call_function(identifier, arguments, n->arguments.length, false, &return_type, &body, scope)) {
            free(arguments);
            n->return_type = return_type;
            n->body = body;
            return return_type;
          } else {
            free(arguments);
            type_inference_error(NULL, node->loc, unit_poisoned);
            printf("I cannot find a function %s with these argument types.\n", st_get_str_of(identifier));
            return POISON_TYPE;
          }
        }
      }
      assert(false);
    }

    case NODE_BIND_SYMBOL: {
      return FTYPE_TYPE;
    }

    case NODE_PRIMITIVE_TYPE: {
      Ast_Primitive_Type *n = (Ast_Primitive_Type *)node;
      return FTYPE_TYPE;
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

      case NODE_EACH: {
        last_statement_type = NOTHING_TYPE;
        Ast_Each *n = (Ast_Each *)node;
        Type collection = infer_type_of_expr(n->collection, &block->scope, unit, true, unit_poisoned);

        {
          Argument a;
          if(collection.info->type == TYPE_FTYPE) {
            a.type = ARGUMENT_TYPE;
            a.data.type = evaluate_type_expr(n->collection, &block->scope, unit, unit_poisoned);
          } else {
            a.type = ARGUMENT_NON_TYPE;
            a.data.non_type = collection;
          }

          if(!call_function(st_get_id_of("iterator_make",-1), &a, 1, true, &n->iterator_type, &n->make_body, &block->scope)) {
            type_inference_error("I cannot find a function which implements the iterator_make operator with these argument types.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
        }

        {
          Argument a;
          a.type = ARGUMENT_NON_TYPE;
          a.data.non_type = n->iterator_type;
          if(!call_function(st_get_id_of("iterator_element",-1), &a, 1, true, &n->element_type, &n->element_body, &block->scope)) {
            type_inference_error("I cannot find a function which implements the iterator_element operator with these argument types.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
        }

        {
          Argument a;
          a.type = ARGUMENT_NON_TYPE;
          a.data.non_type = n->iterator_type;
          if(!call_function(st_get_id_of("iterator_index",-1), &a, 1, true, &n->index_type, &n->index_body, &block->scope)) {
            type_inference_error("I cannot find a function which implements the iterator_index operator with these argument types.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
        }

        {
          Argument a;
          a.type = ARGUMENT_NON_TYPE;
          a.data.non_type = n->iterator_type;
          Type return_type;
          if(!call_function(st_get_id_of("iterator_done",-1), &a, 1, true, &return_type, &n->done_body, &block->scope)) {
            type_inference_error("I cannot find a function which implements the iterator_done operator with these argument types.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
          if(!type_equals(return_type, BOOL_TYPE)) {
            type_inference_error("The function I found implementing the iterator_done operator with these argument types does not return a boolean result.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
        }

        {
          Argument a;
          a.type = ARGUMENT_NON_TYPE;
          a.data.non_type = n->iterator_type;
          a.data.non_type.reference_count++;
          Type return_type;
          if(!call_function(st_get_id_of("iterator_next",-1), &a, 1, true, &return_type, &n->next_body, &block->scope)) {
            type_inference_error("I cannot find a function which implements the iterator_next operator with these argument types.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
          if(!type_equals(return_type, NOTHING_TYPE)) {
            type_inference_error("The function I found implementing the iterator_next operator with these argument types returns a result; it should return nothing.", node->loc, unit_poisoned);
            return POISON_TYPE;
          }
        }

        assert(n->scope.type == REGISTER_SCOPE);
        assert(n->scope.entries.length == 2);
        assert(n->scope.entries.data[0].symbol == n->element);
        n->scope.entries.data[0].data.reg.type = n->element_type;
        assert(n->scope.entries.data[1].symbol == n->index);
        n->scope.entries.data[1].data.reg.type = n->index_type;

        Type body = infer_type_of_expr(n->body, &n->scope, unit, false, unit_poisoned);
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

void type_infer_foreign_definition(Ast_Foreign_Definition *node, Scope *scope, Compilation_Unit *unit, bool *unit_poisoned) {
  assert(node->parameters.length == node->parameter_types.length);
  for(int i = 0; i < node->parameters.length; i++) {
    Ast_Node *param = node->parameters.data[i];
    node->parameter_types.data[i] = type_of_type_expr(param, scope, unit, param->loc, unit_poisoned);
  }
  node->return_type = type_of_type_expr(node->return_type_node, scope, unit, node->return_type_node->loc, unit_poisoned);
}

void type_infer_function_definition(Ast_Function_Definition *node, Scope *scope, Compilation_Unit *unit, bool *unit_poisoned) {
  assert(node->def_type == FN_SIMPLE);

  infer_types_of_function_parameters(node, unit, unit_poisoned);

  node->return_type = type_of_type_expr(node->return_type_node, scope, unit, node->return_type_node->loc, unit_poisoned);
}

void type_infer_compilation_unit(Compilation_Unit *unit) {
  if(unit->type_inferred || unit->poisoned) return;
  if(unit->type_inference_seen) {
    print_error_message("I found a circular dependency at this node.", unit->node->loc);
    unit->poisoned = true;
    exit(1); // Currently, continuing from this point could lead to invalid error messages.
    return;
  }
  unit->type_inference_seen = true;
  switch(unit->type) {
    case UNIT_FUNCTION_SIGNATURE: {
      assert(unit->node->type == NODE_FUNCTION_DEFINITION);
      type_infer_function_definition((Ast_Function_Definition *)unit->node, unit->scope, unit, &unit->poisoned);
      break;
    }
    case UNIT_FOREIGN_FUNCTION: {
      assert(unit->node->type == NODE_FOREIGN_DEFINITION);
      type_infer_foreign_definition((Ast_Foreign_Definition *)unit->node, unit->scope, unit, &unit->poisoned);
      break;
    }
    case UNIT_FUNCTION_BODY: {
      Compilation_Unit *sig = unit->data.body.signature;
      assert(sig->type == UNIT_FUNCTION_SIGNATURE || sig->type == UNIT_POLY_FUNCTION);
      type_infer_compilation_unit(sig);
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
      Type type = infer_type_of_struct_definition((Ast_Struct_Definition *)unit->node, unit->scope, &unit->poisoned);
      unit->data.struct_def.type = type;
      break;
    }
    case UNIT_POLY_STRUCT: {
      infer_type_of_poly_struct_definition((Ast_Poly_Struct_Definition *)unit->node, unit, &unit->poisoned);
      break;
    }
    case UNIT_STRUCT_MEMBER: {
      Ast_Node *node = unit->node;
      if(node->type == NODE_TYPED_DECL) {
        Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
        unit->data.struct_member.type = type_of_type_expr(n->type_node, unit->scope, unit, node->loc, &unit->poisoned);
      } else if(node->type == NODE_TYPED_DECL_SET) {
        Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
        unit->data.struct_member.type = type_of_type_expr(n->type_node, unit->scope, unit, node->loc, &unit->poisoned);
      } else assert(false);
      break;
    }
    case UNIT_POLY_FUNCTION: {
      // we just can't do anything yet
      break;
    }
    case UNIT_IMPORT: {
      assert(unit->node->type == NODE_IMPORT);

      Ast_Import *import = (Ast_Import *)unit->node;
      int file_id = add_file(import->filename);
      parse_file(file_id);
      File_Data *file_data = files.data+file_id;
      assert(file_data->parsed);
      unit->data.import.module = file_data->module;
      break;
    }
    case UNIT_CONSTANT: {
      Ast_Node *node = unit->node;
      if(node->type == NODE_TYPED_DECL_SET) {
        Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
        Type defined = type_of_type_expr(n->type_node, unit->scope, unit, node->loc, &unit->poisoned);
        Type inferred = infer_type_of_expr(n->value, unit->scope, unit, true, &unit->poisoned);
        if(inferred.reference_count != 0 || inferred.info->type != TYPE_UNKNOWN_INT) {
          type_inference_error("A global constant can only be an integer literal.", node->loc, &unit->poisoned);
          break;
        }
        if(!can_implicitly_cast_type(inferred, defined)) {
          error_cannot_implicitly_cast(inferred, defined, node->loc, false, &unit->poisoned);
          unit->data.constant.type = POISON_TYPE;
          break;
        }

        unit->data.constant.type = defined;
        unit->data.constant.value = inferred.info->data.unknown_int;
      } else if(node->type == NODE_UNTYPED_DECL_SET) {
        Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
        Type inferred = infer_type_of_expr(n->value, unit->scope, unit, true, &unit->poisoned);
        if(inferred.reference_count != 0 || inferred.info->type != TYPE_UNKNOWN_INT) {
          type_inference_error("A global constant can only be an integer literal.", node->loc, &unit->poisoned);
          break;
        }

        unit->data.constant.type = solidify_type(inferred);
        unit->data.constant.value = inferred.info->data.unknown_int;
      } else assert(false);
      break;
    }
    case UNIT_UNIQUE: {
      Ast_Node *node = unit->node;
      assert(node->type == NODE_UNIQUE_DEFINITION);
      Ast_Unique_Definition *n = (Ast_Unique_Definition *)node;
      unit->data.unique.symbol = n->symbol;
      Type original = type_of_type_expr(n->node, unit->scope, unit, node->loc, &unit->poisoned);
      unit->data.unique.type = make_unique_type(original, n->symbol);
      break;
    }
    case UNIT_MODULE: {
      for(int i = 0; i < unit->data.module.scope.entries.length; i++) {
        Scope_Entry entry = unit->data.module.scope.entries.data[i];
        type_infer_compilation_unit(entry.data.unit.unit);
      }
      break;
    }
    default:
      print_error_message("Internal compiler error: Unknown compilation unit type.", NULL_LOCATION);
      exit(1);
  }
  unit->type_inferred = true;
}

Type infer_type_of_compilation_unit(Compilation_Unit *unit) {
  type_infer_compilation_unit(unit);
  if(unit->poisoned) return POISON_TYPE;

  switch(unit->type) {
    case UNIT_STRUCT:
    case UNIT_UNIQUE: {
      return FTYPE_TYPE;
    }
    case UNIT_STRUCT_MEMBER: {
      return unit->data.struct_member.type;
    }
    case UNIT_CONSTANT: {
      return unit->data.constant.type;
    }

    case UNIT_FUNCTION_SIGNATURE:
    case UNIT_FUNCTION_BODY:
    case UNIT_POLY_STRUCT:
    case UNIT_POLY_FUNCTION:
    default: {
      print_error_message("Internal compiler error: Tried to find the type of this compilation unit, which has no type.", unit->node->loc);
      exit(1);
    }
  }
}
