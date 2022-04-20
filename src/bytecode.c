#include "bytecode.h"

#include <stdio.h>

#include "c-utils/integer.h"
#include "c-utils/alloca.h"

#include "ast.h"
#include "type_inference.h"
#include "errors.h"


// Refers to the translation of an ast block to bytecode, where entry is the
// generated entry block and exit is the generated exit block.
// Entry can equal exit.
typedef struct Bytecode_Ast_Block {
  u32 entry;
  u32 exit;
  u32 result_reg;
} Bytecode_Ast_Block;

Bytecode_Unit *allocate_bytecode_unit_type(Bytecode_Unit_Type type, u32 size) {
  Bytecode_Unit *result = malloc(size);
  result->type = type;
  return result;
}

GENERATE_DARRAY_CODE(Bytecode_Instruction, Bytecode_Instruction_Array);
GENERATE_DARRAY_CODE(Bytecode_Block, Bytecode_Block_Array);
GENERATE_DARRAY_CODE(Bytecode_Unit *, Bytecode_Unit_Ptr_Array);

Scope_Entry *get_entry_of_identifier_register_scope(symbol symbol, Scope *scope, Location error_location) {
  Scope *found_scope;
  Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, error_location, &found_scope);
  assert(found_scope->type == REGISTER_SCOPE);
  return e;
}

void print_bytecode_instruction(Bytecode_Instruction inst) {
  switch(inst.type) {
    case BC_ADD: {
      printf("add r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_SUB: {
      printf("sub r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_MUL: {
      printf("mul r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_DIV: {
      printf("div r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_AND: {
      printf("and r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_OR: {
      printf("or r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_EQUALS: {
      printf("eq r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_LESS_THAN: {
      printf("less_than r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_LESS_THAN_EQUALS: {
      printf("less_than_equals r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_SET_LITERAL: {
      printf("set r%i <- %lli\n", inst.data.set_literal.reg_a, inst.data.set_literal.lit_b);
      break;
    }
    case BC_SET_PTR_LITERAL: {
      printf("set r%i <- %lli bytes ptr\n", inst.data.set_ptr_literal.reg_a, inst.data.set_ptr_literal.length);
      return;
    }
    case BC_SET: {
      printf("set r%i <- r%i\n", inst.data.set.reg_a, inst.data.set.reg_b);
      break;
    }
    case BC_BIT_CAST: {
      printf("bit_cast r%i <- r%i\n", inst.data.bit_cast.reg_a, inst.data.bit_cast.reg_b);
      break;
    }
    case BC_REF_TO: {
      printf("ref_to r%i <- r%i\n", inst.data.unary_op.reg_a, inst.data.unary_op.reg_b);
      break;
    }
    case BC_GET_MEMBER_PTR: {
      printf("get_member_ptr r%i <- r%i at %i\n", inst.data.get_member_ptr.reg_a, inst.data.get_member_ptr.reg_b, inst.data.get_member_ptr.member);
      break;
    }
    case BC_LOAD: {
      printf("load r%i <- r%i\n", inst.data.unary_op.reg_a, inst.data.unary_op.reg_b);
      break;
    }
    case BC_STORE: {
      printf("store r%i <- r%i\n", inst.data.unary_op.reg_a, inst.data.unary_op.reg_b);
      break;
    }
    case BC_CALL: {
      if(inst.data.call.keep_return_value)
        printf("call %s -> r%i\n", st_get_str_of(inst.data.call.to->name), inst.data.call.reg);
      else
        printf("call %s\n", st_get_str_of(inst.data.call.to->name));
      break;
    }
    case BC_ARG: {
      printf("  arg r%i\n", inst.data.arg.reg);
      break;
    }
    case BC_RETURN: {
      printf("ret r%i\n", inst.data.ret.reg);
      break;
    }
    case BC_RETURN_NOTHING: {
      printf("ret_nothing\n");
      break;
    }
    case BC_BRANCH: {
      printf("branch %i\n", inst.data.branch.block);
      break;
    }
    case BC_COND_BRANCH: {
      printf("cond_branch r%i -> [%i, %i]\n", inst.data.cond_branch.reg_cond, inst.data.cond_branch.block_true, inst.data.cond_branch.block_false);
      break;
    }
    default: {
      printf("<cannot print this instruction>\n");
    }
  }
}

void print_bytecode_block(Bytecode_Block block) {
  for(int i = 0; i < block.instructions.length; i++) {
    printf("      ");
    print_bytecode_instruction(block.instructions.data[i]);
  }
}

void print_bytecode_function(Bytecode_Function fn) {
  printf("%s :: bytecode_fn ", st_get_str_of(fn.u.name));
  if(fn.is_inline) printf("inline ");
  printf("(%i) -> ", fn.passed_param_count);
  print_type(fn.return_type);
  printf(" {\n");
  printf("  .registers :: {\n");
  for(int i = 0; i < fn.register_types.length; i++) {
    printf("    %i: ", i);
    print_type(fn.register_types.data[i]);
    printf("\n");
  }
  printf("  }\n");
  printf("  .blocks :: {\n");
  for(int i = 0; i < fn.blocks.length; i++) {
    printf("    %i", i);
    if(i == fn.entry_block) printf(" (entry)");
    printf(":\n");
    print_bytecode_block(fn.blocks.data[i]);
  }
  printf("  }\n}\n");
}

void print_bytecode_foreign_function(Bytecode_Foreign_Function fn) {
  printf("%s :: foreign (", st_get_str_of(fn.u.name));
  for(int i = 0; i < fn.parameter_types.length; i++) {
    if(i != 0) printf(", ");
    print_type(fn.parameter_types.data[i]);
  }
  printf(") -> ");
  print_type(fn.return_type);
  printf(";");
}

void print_bytecode_unit(Bytecode_Unit *unit) {
  switch(unit->type) {
    case BYTECODE_FUNCTION: {
      print_bytecode_function(*((Bytecode_Function *)unit));
      break;
    }
    case BYTECODE_FOREIGN_FUNCTION: {
      print_bytecode_foreign_function(*((Bytecode_Foreign_Function *)unit));
      break;
    }
    default: assert(false);
  }
}

void print_bytecode_compilation_unit(Compilation_Unit *unit) {
  if(unit->bytecode_generated) {
    if(unit->type == UNIT_FUNCTION_BODY) print_bytecode_unit(unit->data.body.bytecode);
    else if(unit->type == UNIT_FOREIGN_FUNCTION) print_bytecode_unit(unit->data.foreign.bytecode);
    else assert(false);
  } else printf("<bytecode not generated yet>\n");
}

u32 add_register(Bytecode_Function *fn, Type type) {
  Type_Array_push(&fn->register_types, type);
  return fn->register_types.length-1;
}

void add_instruction(Bytecode_Block *block, Bytecode_Instruction inst) {
  assert(!block->is_concluded);
  if(inst.type == BC_RETURN || inst.type == BC_RETURN_NOTHING || inst.type == BC_BRANCH || inst.type == BC_COND_BRANCH)
    block->is_concluded = true;
  Bytecode_Instruction_Array_push(&block->instructions, inst);
}

Bytecode_Block init_bytecode_block() {
  return (Bytecode_Block) {init_Bytecode_Instruction_Array(2), false};
}







u32 add_block_to_block(Ast_Block *block_node, Bytecode_Function *fn, u32 *block);
Bytecode_Ast_Block generate_bytecode_block(Ast_Node *node, Bytecode_Function *fn, Scope *scope);
u32 generate_bytecode_expr(Ast_Node *node, u32 *block, Bytecode_Function *fn, Scope *scope);

u32 add_unary_op_instruction(Bytecode_Block *block, Bytecode_Instruction_Type t, u32 a, u32 b) {
  Bytecode_Instruction inst;
  inst.type = t;
  inst.data.unary_op.reg_a = a;
  inst.data.unary_op.reg_b = b;
  add_instruction(block, inst);
  return a;
}

u32 add_bin_op_instruction(Bytecode_Block *block, Bytecode_Instruction_Type t, u32 a, u32 b, u32 c) {
  Bytecode_Instruction inst;
  inst.type = t;
  inst.data.bin_op.reg_a = a;
  inst.data.bin_op.reg_b = b;
  inst.data.bin_op.reg_c = c;
  add_instruction(block, inst);
  return a;
}

u32 add_set_literal_instruction(Bytecode_Function *fn, u32 *block, int value, Type type) {
  Bytecode_Instruction inst;
  inst.type = BC_SET_LITERAL;
  inst.data.set_literal.lit_b = value;
  inst.data.set_literal.reg_a = add_register(fn, solidify_type(type));
  add_instruction(&fn->blocks.data[*block], inst);
  return inst.data.set_literal.reg_a;
}

u32 add_set_ptr_literal_instruction(Bytecode_Function *fn, u32 *block, u8 *ptr, s64 length, Type type) {
  Bytecode_Instruction inst;
  inst.type = BC_SET_PTR_LITERAL;
  inst.data.set_ptr_literal.reg_a = add_register(fn, solidify_type(type));
  inst.data.set_ptr_literal.ptr = ptr;
  inst.data.set_ptr_literal.length = length;
  add_instruction(&fn->blocks.data[*block], inst);
  return inst.data.set_ptr_literal.reg_a;
}

void add_arg(Bytecode_Function *fn, u32 block, u32 reg) {
  Bytecode_Instruction inst;
  inst.type = BC_ARG;
  inst.data.arg.reg = reg;
  add_instruction(&fn->blocks.data[block], inst);
}

void add_call(Bytecode_Function *fn, u32 block, Compilation_Unit *body, Type return_type, u32 result_reg) {
  generate_bytecode_compilation_unit(body);
  Bytecode_Instruction inst;
  inst.type = BC_CALL;
  if(body->type == UNIT_FUNCTION_BODY)
    inst.data.call.to = body->data.body.bytecode;
  else if(body->type == UNIT_FOREIGN_FUNCTION)
    inst.data.call.to = body->data.foreign.bytecode;
  else assert(false);
  inst.data.call.keep_return_value = return_type.info->type != TYPE_NOTHING;
  inst.data.call.reg = result_reg;
  add_instruction(&fn->blocks.data[block], inst);
}

u32 add_call_new_reg(Bytecode_Function *fn, u32 block, Compilation_Unit *body, Type return_type) {
  u32 result_reg;
  if(return_type.info->type == TYPE_NOTHING)
    result_reg = -1;
  else
    result_reg = add_register(fn, return_type);

  add_call(fn, block, body, return_type, result_reg);
  return result_reg;
}

u32 add_subscript_ref(Ast_Binary_Op *n, u32 *block, Bytecode_Function *fn, Scope *scope) {
  u32 first_reg = generate_bytecode_expr(n->first, block, fn, scope);
  u32 second_reg = generate_bytecode_expr(n->second, block, fn, scope);

  Compilation_Unit *body = n->data.overload.body;
  generate_bytecode_compilation_unit(body);

  Type call_return_type = n->convert_to;
  if(n->operator == OPSUBSCRIPT)
    call_return_type.reference_count++;

  u32 result_reg = add_call_new_reg(fn, *block, body, call_return_type);
  add_arg(fn, *block, first_reg);
  add_arg(fn, *block, second_reg);
  return result_reg;
}

// Takes a register containing type ^^^X, ^^X, ^X or X, and converts it to ^X
// either by referencing or dereferencing it.
u32 ref_or_deref_to_single_ptr(u32 ptr_reg, u32 *block, Bytecode_Function *fn) {
  Type ptr_type = fn->register_types.data[ptr_reg];
  if(ptr_type.reference_count > 0) {
    while(ptr_type.reference_count > 1) {
      ptr_type.reference_count--;
      u32 new_reg = add_register(fn, ptr_type);
      Bytecode_Instruction inst;
      inst.type = BC_LOAD;
      inst.data.unary_op.reg_a = new_reg;
      inst.data.unary_op.reg_b = ptr_reg;
      add_instruction(&fn->blocks.data[*block], inst);
      ptr_reg = new_reg;
    }
    return ptr_reg;
  } else {
    ptr_type.reference_count++;
    u32 new_reg = add_register(fn, ptr_type);
    {
      Bytecode_Instruction inst;
      inst.type = BC_REF_TO;
      inst.data.get_member_ptr.reg_a = new_reg;
      inst.data.get_member_ptr.reg_b = ptr_reg;
      add_instruction(&fn->blocks.data[*block], inst);
    }
    return new_reg;
  }
}

u32 generate_set_expression_ptr(Ast_Node *node, u32 *block, Bytecode_Function *fn, Scope *scope) {
  if(node->type == NODE_BINARY_OP) {
    Ast_Binary_Op *n = (Ast_Binary_Op *)node;
    if(n->operator == OPSTRUCT_MEMBER) {
      u32 first_reg; // this should be a single pointer to a struct.
      if(n->first->type == NODE_SYMBOL) {
        Ast_Symbol *sn = (Ast_Symbol *)n->first;
        Scope_Entry *e = get_entry_of_identifier_register_scope(sn->symbol, scope, sn->n.loc);
        u32 struct_reg = e->data.reg.register_id;
        first_reg = ref_or_deref_to_single_ptr(struct_reg, block, fn);
      } else {
        u32 struct_reg = generate_set_expression_ptr(n->first, block, fn, scope);
        first_reg = ref_or_deref_to_single_ptr(struct_reg, block, fn);
      }

      Ast_Symbol *s = (Ast_Symbol *)n->second;

      Type result_type;
      u32 member;
      {
        Type first_type = fn->register_types.data[first_reg];
        Compilation_Unit_Ptr_Array members;
        if(first_type.info->type == TYPE_STRUCT)
          members = first_type.info->data.struct_.members;
        else if(first_type.info->type == TYPE_POLY_INSTANCE)
          members = first_type.info->data.poly_instance.members;
        else assert(false);

        bool found = false;
        for(int i = 0; i < members.length; i++) {
          Compilation_Unit *unit = members.data[i];
          assert(unit->type == UNIT_STRUCT_MEMBER);
          if(unit->data.struct_member.symbol == s->symbol) {
            found = true;
            assert(unit->type_inferred);
            result_type = unit->data.struct_member.type;
            member = i;
          }
        }
        assert(found);
      }

      Type ptr_type = result_type;
      ptr_type.reference_count++;

      u32 ptr_reg = add_register(fn, ptr_type);
      {
        Bytecode_Instruction inst;
        inst.type = BC_GET_MEMBER_PTR;
        inst.data.get_member_ptr.reg_a = ptr_reg;
        inst.data.get_member_ptr.reg_b = first_reg;
        inst.data.get_member_ptr.member = member;
        add_instruction(&fn->blocks.data[*block], inst);
      }
      return ptr_reg;
    } else if(n->operator == OPSUBSCRIPT || n->operator == OPSUBSCRIPT_REF) {
      return add_subscript_ref(n, block, fn, scope);
    } else assert(false);
  } else assert(false);
}

// returns the register storing the result
u32 generate_bytecode_expr(Ast_Node *node, u32 *block, Bytecode_Function *fn, Scope *scope) {
  switch(node->type) {
    case NODE_LITERAL: {
      Ast_Literal *n = (Ast_Literal *)node;
      return add_set_literal_instruction(fn, block, n->value, n->type);
    }
    case NODE_LITERAL_STRING: {
      Ast_Literal_String *n = (Ast_Literal_String *)node;
      Type ptr_type = integer_type_with(false, 8);
      ptr_type.reference_count++;
      u32 ptr_reg = add_set_ptr_literal_instruction(fn, block, n->string, n->length, ptr_type);
      u32 length_reg = add_set_literal_instruction(fn, block, n->length, INT_TYPE);
      u32 result_reg = add_call_new_reg(fn, *block, n->make_body, n->return_type);
      add_arg(fn, *block, ptr_reg);
      add_arg(fn, *block, length_reg);
      return result_reg;
    }
    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = (Ast_Binary_Op *)node;
      switch(n->operator) {
        #define MACRO_ADD_BINARY_BYTECODE(op_type, inst_type, reverse, result_reg)\
          case op_type: {\
            u32 reg_b = generate_bytecode_expr(reverse ? n->second : n->first, block, fn, scope);\
            u32 reg_c = generate_bytecode_expr(reverse ? n->first : n->second, block, fn, scope);\
            return add_bin_op_instruction(&fn->blocks.data[*block], inst_type,\
                                          result_reg,\
                                          reg_b,\
                                          reg_c);\
        }
        MACRO_ADD_BINARY_BYTECODE(OPPLUS, BC_ADD, false, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPMINUS, BC_SUB, false, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPMUL, BC_MUL, false, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPDIV, BC_DIV, false, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPAND, BC_AND, false, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPOR, BC_OR, false, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPEQUALS, BC_EQUALS, false, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPLESS_THAN, BC_LESS_THAN, false, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPGREATER_THAN, BC_LESS_THAN, true, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPLESS_THAN_OR_EQUAL_TO, BC_LESS_THAN_EQUALS, false, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPGREATER_THAN_OR_EQUAL_TO, BC_LESS_THAN_EQUALS, true, add_register(fn, BOOL_TYPE))
        // lookup table maybe?
        case OPSET_EQUALS: {
          u32 value_reg = generate_bytecode_expr(n->second, block, fn, scope);
          Type value_type = fn->register_types.data[value_reg];
          if(n->first->type == NODE_SYMBOL) {
            Ast_Symbol *sn = (Ast_Symbol *)n->first;
            symbol symbol = sn->symbol;

            Scope_Entry *e = get_entry_of_identifier_register_scope(symbol, scope, sn->n.loc);

            u32 ptr_reg = e->data.reg.register_id;
            Type ptr_type = fn->register_types.data[ptr_reg];
            if(ptr_type.reference_count == value_type.reference_count) {
              Bytecode_Instruction inst;
              inst.type = BC_SET;
              inst.data.set.reg_a = ptr_reg;
              inst.data.set.reg_b = value_reg;
              add_instruction(&fn->blocks.data[*block], inst);
              return value_reg;
            }

            while(ptr_type.reference_count > value_type.reference_count+1) {
              Bytecode_Instruction inst;
              inst.type = BC_LOAD;
              ptr_type.reference_count--;
              u32 new_reg = add_register(fn, ptr_type);
              inst.data.unary_op.reg_a = new_reg;
              inst.data.unary_op.reg_b = ptr_reg;
              add_instruction(&fn->blocks.data[*block], inst);
              ptr_reg = new_reg;
            }
            Bytecode_Instruction inst;
            inst.type = BC_STORE;
            inst.data.unary_op.reg_a = ptr_reg;
            inst.data.unary_op.reg_b = value_reg;
            add_instruction(&fn->blocks.data[*block], inst);
            return value_reg;
          } else {
            u32 ptr_reg = generate_set_expression_ptr(n->first, block, fn, scope);
            Type ptr_type = fn->register_types.data[ptr_reg];
            while(ptr_type.reference_count > value_type.reference_count+1) {
              Bytecode_Instruction inst;
              inst.type = BC_LOAD;
              ptr_type.reference_count--;
              u32 new_reg = add_register(fn, ptr_type);
              inst.data.unary_op.reg_a = new_reg;
              inst.data.unary_op.reg_b = ptr_reg;
              add_instruction(&fn->blocks.data[*block], inst);
              ptr_reg = new_reg;
            }
            Bytecode_Instruction inst;
            inst.type = BC_STORE;
            inst.data.unary_op.reg_a = ptr_reg;
            inst.data.unary_op.reg_b = value_reg;
            add_instruction(&fn->blocks.data[*block], inst);
            return value_reg;
          }
        }
        case OPSTRUCT_MEMBER:
        case OPSTRUCT_MEMBER_REF: {
          Ast_Symbol *s = (Ast_Symbol *)n->second;
          assert(s->n.type == NODE_SYMBOL);

          u32 struct_reg = generate_bytecode_expr(n->first, block, fn, scope);
          Type struct_type = fn->register_types.data[struct_reg];
          u32 struct_ptr_reg = ref_or_deref_to_single_ptr(struct_reg, block, fn);

          Type result_type;
          u32 member;
          {
            Compilation_Unit_Ptr_Array members;
            if(struct_type.info->type == TYPE_STRUCT)
              members = struct_type.info->data.struct_.members;
            else if(struct_type.info->type == TYPE_POLY_INSTANCE)
              members = struct_type.info->data.poly_instance.members;

            bool found = false;
            for(int i = 0; i < members.length; i++) {
              Compilation_Unit *unit = members.data[i];
              assert(unit->type == UNIT_STRUCT_MEMBER);
              if(unit->data.struct_member.symbol == s->symbol) {
                found = true;
                type_infer_compilation_unit(unit);
                result_type = unit->data.struct_member.type;
                member = i;
              }
            }
            assert(found);
          }

          Type member_ptr_type = result_type;
          member_ptr_type.reference_count++;

          u32 member_ptr_reg = add_register(fn, member_ptr_type);
          {
            Bytecode_Instruction inst;
            inst.type = BC_GET_MEMBER_PTR;
            inst.data.get_member_ptr.reg_a = member_ptr_reg;
            inst.data.get_member_ptr.reg_b = struct_ptr_reg;
            inst.data.get_member_ptr.member = member;
            add_instruction(&fn->blocks.data[*block], inst);
          }
          if(n->operator == OPSTRUCT_MEMBER_REF) {
            return member_ptr_reg;
          } else {
            u32 result_reg = add_register(fn, result_type);
            {
              Bytecode_Instruction inst;
              inst.type = BC_LOAD;
              inst.data.unary_op.reg_a = result_reg;
              inst.data.unary_op.reg_b = member_ptr_reg;
              add_instruction(&fn->blocks.data[*block], inst);
            }
            return result_reg;
          }
        }
        case OPSUBSCRIPT:
        case OPSUBSCRIPT_REF: {
          u32 element_ptr_reg = add_subscript_ref(n, block, fn, scope);

          if(n->operator == OPSUBSCRIPT) {
            u32 result_reg = add_register(fn, n->convert_to);
            return add_unary_op_instruction(&fn->blocks.data[*block], BC_LOAD,
                                            result_reg,
                                            element_ptr_reg);
          } else if(n->operator == OPSUBSCRIPT_REF) {
            return element_ptr_reg;
          } else assert(false);
        }
        default: {
          print_error_message("Internal compiler error: I cannot generate bytecode for this node.", node->loc);
          exit(1);
        }
      }
      assert(false);
    }
    case NODE_UNARY_OP: {
      Ast_Unary_Op *n = (Ast_Unary_Op *)node;
      switch(n->operator) {
        case OPREFERENCE: {
          u32 reg_b = generate_bytecode_expr(n->operand, block, fn, scope);
          Type result_type = fn->register_types.data[reg_b];
          result_type.reference_count++;
          u32 result_reg = add_register(fn, result_type);
          return add_unary_op_instruction(&fn->blocks.data[*block], BC_REF_TO,
                                          result_reg,
                                          reg_b);
        }
        case OPDEREFERENCE: {
          u32 reg_b = generate_bytecode_expr(n->operand, block, fn, scope);
          Type result_type = fn->register_types.data[reg_b];
          assert(result_type.reference_count > 0);
          result_type.reference_count--;
          u32 result_reg = add_register(fn, result_type);
          return add_unary_op_instruction(&fn->blocks.data[*block], BC_LOAD,
                                          result_reg,
                                          reg_b);
        }
        default: {
          print_error_message("Internal compiler error: I cannot generate bytecode for this node.", node->loc);
          exit(1);
        }
      }
    }
    case NODE_SYMBOL: {
      Ast_Symbol *n = (Ast_Symbol *)node;
      Scope *found_scope;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, node->loc, &found_scope);
      if(found_scope->type == UNIT_SCOPE) {
        Compilation_Unit *u = e->data.unit.unit;
        assert(u->type == UNIT_CONSTANT);
        return add_set_literal_instruction(fn, block, u->data.constant.value, u->data.constant.type);
      } else if(found_scope->type == REGISTER_SCOPE) {
        return e->data.reg.register_id;
      } else assert(false);
    }
    case NODE_BLOCK: {
      return add_block_to_block((Ast_Block *)node, fn, block);
    }
    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      Scope_Entry *e = get_entry_of_identifier_register_scope(n->symbol, scope, node->loc);
      e->data.reg.register_id = add_register(fn, e->data.reg.type);
      return -1;
    }
    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
      Scope_Entry *e = get_entry_of_identifier_register_scope(n->symbol, scope, node->loc);
      u32 reg = add_register(fn, e->data.reg.type);
      e->data.reg.register_id = reg;
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      add_instruction(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_TYPED_DECL_SET: {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      Scope_Entry *e = get_entry_of_identifier_register_scope(n->symbol, scope, node->loc);
      u32 reg = fn->register_types.length;
      e->data.reg.register_id = add_register(fn, e->data.reg.type);
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      add_instruction(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_RETURN: {
      Ast_Return *n = (Ast_Return *)node;
      if(fn->blocks.data[*block].is_concluded) return -1;
      if(n->is_return_nothing) {
        Bytecode_Instruction inst;
        inst.type = BC_RETURN_NOTHING;
        add_instruction(&fn->blocks.data[*block], inst);
        return -1;
      } else {
        Bytecode_Instruction inst;
        inst.type = BC_RETURN;
        inst.data.ret.reg = generate_bytecode_expr(n->value, block, fn, scope);
        add_instruction(&fn->blocks.data[*block], inst);
        return inst.data.ret.reg;
      }
    }
    case NODE_IF: {
      Ast_If *n = (Ast_If *)node;
      Bytecode_Instruction cond;
      cond.type = BC_COND_BRANCH;
      cond.data.cond_branch.reg_cond = generate_bytecode_expr(n->cond, block, fn, scope);
      u32 prev_block = *block;
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      *block = fn->blocks.length - 1;

      u32 if_result_reg;
      if(n->result_is_used) {
        if_result_reg = add_register(fn, n->result_type);
      }

      Bytecode_Ast_Block block_true = generate_bytecode_block(n->first, fn, scope);
      cond.data.cond_branch.block_true = block_true.entry;
      if(!fn->blocks.data[block_true.exit].is_concluded) {
        if(n->result_is_used) {
          Bytecode_Instruction inst;
          inst.type = BC_SET;
          inst.data.set.reg_a = if_result_reg;
          inst.data.set.reg_b = block_true.result_reg;
          add_instruction(&fn->blocks.data[block_true.exit], inst);
        }
        Bytecode_Instruction inst;
        inst.type = BC_BRANCH;
        inst.data.branch.block = *block;
        add_instruction(&fn->blocks.data[block_true.exit], inst);
      }

      if(n->second->type == NODE_NULL) {
        cond.data.cond_branch.block_false = *block;
      } else {
        Bytecode_Ast_Block block_false = generate_bytecode_block(n->second, fn, scope);
        cond.data.cond_branch.block_false = block_false.entry;
        if(!fn->blocks.data[block_false.exit].is_concluded) {
          if(n->result_is_used) {
            Bytecode_Instruction inst;
            inst.type = BC_SET;
            inst.data.set.reg_a = if_result_reg;
            inst.data.set.reg_b = block_false.result_reg;
            add_instruction(&fn->blocks.data[block_false.exit], inst);
          }
          Bytecode_Instruction inst;
          inst.type = BC_BRANCH;
          inst.data.branch.block = *block;
          add_instruction(&fn->blocks.data[block_false.exit], inst);
        }
      }

      add_instruction(&fn->blocks.data[prev_block], cond);
      return n->result_is_used ? if_result_reg : -1;
    }
    case NODE_WHILE: {
      Ast_While *n = (Ast_While *)node;

      u32 prev_block = *block;
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      *block = fn->blocks.length - 1;

      Bytecode_Ast_Block block_cond = generate_bytecode_block(n->cond, fn, scope);
      Bytecode_Ast_Block block_body = generate_bytecode_block(n->body, fn, scope);
      {
        Bytecode_Instruction cond;
        cond.type = BC_COND_BRANCH;
        cond.data.cond_branch.reg_cond = block_cond.result_reg;
        cond.data.cond_branch.block_true = block_body.entry;
        cond.data.cond_branch.block_false = *block;
        add_instruction(&fn->blocks.data[block_cond.exit], cond);
      }
      {
        Bytecode_Instruction back;
        back.type = BC_BRANCH;
        back.data.branch.block = block_cond.entry;
        add_instruction(&fn->blocks.data[block_body.exit], back);
      }
      {
        Bytecode_Instruction intro;
        intro.type = BC_BRANCH;
        intro.data.branch.block = block_cond.entry;
        add_instruction(&fn->blocks.data[prev_block], intro);
      }

      return -1;
    }
    case NODE_EACH: {
      Ast_Each *n = (Ast_Each *)node;

      // Add make_iterator portion to the beginning
      u32 collection_reg = generate_bytecode_expr(n->collection, block, fn, scope);

      generate_bytecode_compilation_unit(n->make_body);
      u32 iterator_reg = add_call_new_reg(fn, *block, n->make_body, n->iterator_type);
      add_arg(fn, *block, collection_reg);

      // Add new block after the loop
      u32 prev_block = *block;
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      *block = fn->blocks.length - 1;


      u32 element_reg = add_register(fn, n->element_type);
      u32 index_reg = add_register(fn, n->index_type);

      assert(n->scope.type == REGISTER_SCOPE);
      assert(n->scope.entries.length == 2);
      assert(n->scope.entries.data[0].symbol == n->element);
      n->scope.entries.data[0].data.reg.register_id = element_reg;
      assert(n->scope.entries.data[1].symbol == n->index);
      n->scope.entries.data[1].data.reg.register_id = index_reg;


      // Add conditional portion to a new block block_cond.
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      u32 block_cond = fn->blocks.length - 1;

      generate_bytecode_compilation_unit(n->done_body);
      u32 done_reg = add_call_new_reg(fn, block_cond, n->done_body, BOOL_TYPE);
      add_arg(fn, block_cond, iterator_reg);


      // Add go-to-next portion to a new block block_next
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      u32 block_next = fn->blocks.length - 1;

      Type iterator_ptr_type = n->iterator_type;
      iterator_ptr_type.reference_count++;
      u32 iterator_ptr_reg = add_register(fn, iterator_ptr_type);
      add_unary_op_instruction(&fn->blocks.data[block_next], BC_REF_TO, iterator_ptr_reg, iterator_reg);

      generate_bytecode_compilation_unit(n->next_body);
      add_call(fn, block_next, n->next_body, NOTHING_TYPE, -1);
      add_arg(fn, block_next, iterator_ptr_reg);


      // Add updating/variable-initialization portion to a new block block_body.
      Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block(2));
      u32 block_body = fn->blocks.length - 1;

      generate_bytecode_compilation_unit(n->element_body);
      add_call(fn, block_body, n->element_body, n->element_type, element_reg);
      add_arg(fn, block_body, iterator_reg);

      generate_bytecode_compilation_unit(n->index_body);
      add_call(fn, block_body, n->index_body, n->index_type, index_reg);
      add_arg(fn, block_body, iterator_reg);

      // Populate the body with the actual body.
      u32 block_body_entry = block_body;
      generate_bytecode_expr(n->body, &block_body, fn, &n->scope);


      // Add all the branches
      {
        Bytecode_Instruction intro;
        intro.type = BC_BRANCH;
        intro.data.branch.block = block_cond;
        add_instruction(&fn->blocks.data[prev_block], intro);
        add_instruction(&fn->blocks.data[block_next], intro);
      }
      {
        Bytecode_Instruction to_next;
        to_next.type = BC_BRANCH;
        to_next.data.branch.block = block_next;
        add_instruction(&fn->blocks.data[block_body], to_next);
      }
      {
        Bytecode_Instruction cond;
        cond.type = BC_COND_BRANCH;
        cond.data.cond_branch.reg_cond = done_reg;
        cond.data.cond_branch.block_true = *block;
        cond.data.cond_branch.block_false = block_body_entry;
        add_instruction(&fn->blocks.data[block_cond], cond);
      }
      return -1;
    }
    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      { // check for built-ins like size_of
        symbol symbol = ((Ast_Symbol *)n->identifier)->symbol;
        if(symbol == st_get_id_of("size_of", -1)) {
          assert(n->arguments.length == 1);
          Type size_type = n->return_type;
          assert(size_type.info->type == TYPE_UNKNOWN_INT);
          s64 size = size_type.info->data.unknown_int;
          return add_set_literal_instruction(fn, block, size, size_type);
        } else if(symbol == st_get_id_of("bit_cast", -1)) {
          assert(n->arguments.length == 2);
          u32 from_reg = generate_bytecode_expr(n->arguments.data[1], block, fn, scope);
          u32 to_reg = add_register(fn, n->return_type);

          Bytecode_Instruction inst;
          inst.type = BC_BIT_CAST;
          inst.data.set.reg_a = to_reg;
          inst.data.set.reg_b = from_reg;
          add_instruction(&fn->blocks.data[*block], inst);
          return to_reg;
        } else if(symbol == st_get_id_of("int_cast", -1)) {
          assert(n->arguments.length == 2);
          u32 from_reg = generate_bytecode_expr(n->arguments.data[1], block, fn, scope);
          return from_reg;
        }
      }
      Compilation_Unit *body = n->body;
      generate_bytecode_compilation_unit(body);

      u32 *arg_registers;
      int arg_count;
      if(body->node->type == NODE_FUNCTION_DEFINITION) {
        Ast_Function_Definition *def = (Ast_Function_Definition *)body->node;
        arg_count = def->passed_param_count;
        arg_registers = alloca(def->passed_param_count * sizeof(u32));
        int passed_i = 0;
        for(int i = 0; i < n->arguments.length; i++) {
          Ast_Node *param = def->parameters.data[i];
          if(param->type == NODE_PASSED_PARAMETER) {
            arg_registers[passed_i] = generate_bytecode_expr(n->arguments.data[i], block, fn, scope);
            passed_i++;
          }
        }
        assert(passed_i == def->passed_param_count);
      } else if(body->node->type == NODE_FOREIGN_DEFINITION) {
        arg_count = n->arguments.length;
        arg_registers = alloca(arg_count * sizeof(u32));
        for(int i = 0; i < n->arguments.length; i++) {
          arg_registers[i] = generate_bytecode_expr(n->arguments.data[i], block, fn, scope);
        }
      } else assert(false);


      u32 result_reg = add_call_new_reg(fn, *block, body, n->return_type);

      for(int i = 0; i < arg_count; i++) {
        add_arg(fn, *block, arg_registers[i]);
      }
      return result_reg;
    }
    case NODE_NULL: {
      return -1;
    }
    case NODE_FUNCTION_DEFINITION:
    case NODE_PRIMITIVE_TYPE:
    default: {
      print_error_message("Internal compiler error: I cannot generate bytecode for this node.", node->loc);
      exit(1);
    }
  }
  assert(false);
}

// This adds instructions from one block to the back of another.
// Result is the register storing the result of the block.
u32 add_block_to_block(Ast_Block *block_node, Bytecode_Function *fn, u32 *block) {
  u32 last_register = 0;
  for(int i = 0; i < block_node->statements.length; i++) {
    Ast_Node *node = block_node->statements.data[i];
    last_register = generate_bytecode_expr(node, block, fn, &block_node->scope);
    if(fn->blocks.data[*block].is_concluded) break;
  }
  return last_register;
}

Bytecode_Ast_Block generate_bytecode_block(Ast_Node *node, Bytecode_Function *fn, Scope *scope) {
  Bytecode_Block_Array_push(&fn->blocks, init_bytecode_block());
  u32 starting_block = fn->blocks.length - 1;
  u32 block = starting_block;
  u32 result_reg;

  if(node->type == NODE_BLOCK) {
    result_reg = add_block_to_block((Ast_Block *)node, fn, &block);
  } else {
    result_reg = generate_bytecode_expr(node, &block, fn, scope);
  }

  return (Bytecode_Ast_Block){starting_block, block, result_reg};
}


void generate_bytecode_function(Bytecode_Function *r, Ast_Function_Definition *defn, symbol unique_name, Scope *scope) {
  r->u.name = unique_name;
  r->register_types = init_Type_Array(4);
  {
    int passed_i = 0;
    for(int i = 0; i < defn->parameters.length; i++) {
      Ast_Node *node = defn->parameters.data[i];
      if(node->type == NODE_PASSED_PARAMETER) {
        Ast_Passed_Parameter *n = (Ast_Passed_Parameter *)defn->parameters.data[i];
        Scope_Entry *e = &defn->parameter_scope.entries.data[passed_i];
        e->data.reg.register_id = add_register(r, e->data.reg.type);
        passed_i++;
      }
    }
    assert(passed_i == defn->passed_param_count);
    r->passed_param_count = passed_i;
  }

  r->is_inline = defn->is_inline;

  r->blocks = init_Bytecode_Block_Array(2);
  assert(defn->body->type == NODE_BLOCK);
  Ast_Block *body = (Ast_Block *)defn->body;
  u32 entry_result_reg;
  Bytecode_Ast_Block generated = generate_bytecode_block(defn->body, r, scope);
  r->entry_block = generated.entry;
  r->return_type = defn->return_type;


  // add a default return if the block isn't concluded, since every block must be concluded
  if(!r->blocks.data[generated.exit].is_concluded) {
    Bytecode_Instruction ret;
    ret.type = BC_RETURN_NOTHING;
    add_instruction(&r->blocks.data[generated.exit], ret);
  }
}

void generate_bytecode_compilation_unit(Compilation_Unit *unit) {
  type_infer_compilation_unit(unit);
  if(unit->bytecode_generated || unit->bytecode_generating || unit->poisoned) return;
  if(unit->type == UNIT_FUNCTION_BODY) {
    assert(unit->node->type == NODE_FUNCTION_DEFINITION);
    unit->bytecode_generating = true;
    Ast_Function_Definition *fn = (Ast_Function_Definition *)unit->node;

    symbol unique_name;
    Compilation_Unit *signature = unit->data.body.signature;
    if(signature->type == UNIT_FUNCTION_SIGNATURE) {
      unique_name = fn->symbol;
    } else if(signature->type == UNIT_POLY_FUNCTION) {
      char *name = st_get_str_of(fn->symbol);
      int name_len = strlen(name);
      u32 my_id = signature->data.poly_function_def.current_instance_id++;
      int necessary_characters = name_len + 1 + (int_log10(my_id) + 1) + 1;

      char *new_name = malloc(sizeof(char) * necessary_characters);
      memcpy(new_name, name, name_len);
      new_name[name_len] = '#';
      sprintf(new_name+name_len+1, "%i", my_id);
      new_name[necessary_characters-1] = 0;
      unique_name = st_get_id_of(new_name, -1);
      free(new_name);
    }

    generate_bytecode_function((Bytecode_Function *)unit->data.body.bytecode, fn, unique_name, unit->scope);
    Bytecode_Unit_Ptr_Array_push(&bytecode_units, unit->data.body.bytecode);
    unit->bytecode_generating = false;
    unit->bytecode_generated = true;
  } else if(unit->type == UNIT_FOREIGN_FUNCTION) {
    assert(unit->node->type == NODE_FOREIGN_DEFINITION);
    unit->bytecode_generating = true;
    Ast_Foreign_Definition *fn = (Ast_Foreign_Definition *)unit->node;

    Bytecode_Unit *bc_unit = unit->data.foreign.bytecode;
    assert(bc_unit->type == BYTECODE_FOREIGN_FUNCTION);

    bc_unit->name = fn->symbol;

    Bytecode_Foreign_Function *u = (Bytecode_Foreign_Function *)bc_unit;
    u->parameter_types = fn->parameter_types;
    u->return_type = fn->return_type;

    Bytecode_Unit_Ptr_Array_push(&bytecode_units, bc_unit);
    unit->bytecode_generating = false;
    unit->bytecode_generated = true;
  } else {
    print_compilation_unit(unit);
    assert(false);
  }

}
