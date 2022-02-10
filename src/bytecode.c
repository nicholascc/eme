#include "bytecode.h"

#include <stdio.h>

#include "c-utils/integer.h"
#include "c-utils/alloca.h"

#include "ast.h"
#include "type_inference.h"
#include "errors.h"

GENERATE_DARRAY_CODE(Bytecode_Instruction, Bytecode_Instruction_Array);
GENERATE_DARRAY_CODE(Bytecode_Block, Bytecode_Block_Array);
GENERATE_DARRAY_CODE(Type, Type_Array);
GENERATE_DARRAY_CODE(Bytecode_Function, Bytecode_Function_Array);

Scope_Entry * get_entry_of_identifier_block_scope(symbol symbol, Scope *scope, Location error_location) {
  Scope *found_scope;
  Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, error_location, &found_scope);
  assert(found_scope->type == BLOCK_SCOPE);
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
    case BC_EQUALS: {
      printf("eq r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_LESS_THAN: {
      printf("less_than r%i <- r%i r%i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_SET_LITERAL: {
      printf("set r%i <- %lli\n", inst.data.set_literal.reg_a, inst.data.set_literal.lit_b);
      break;
    }
    case BC_SET: {
      printf("set r%i <- r%i\n", inst.data.set.reg_a, inst.data.set.reg_b);
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
      printf("call ... -> r%i\n", inst.data.call.reg);
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
  printf("function :: (%i) -> ", fn.param_count);
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

void print_bytecode_compilation_unit(Compilation_Unit *unit) {
  if(unit->bytecode_generated)
    print_bytecode_function(*unit->data.body.bytecode);
  else printf("<bytecode not generated yet>\n");
}

u32 add_register(Bytecode_Function *fn, Type type) {
  Type_Array_push(&fn->register_types, type);
  return fn->register_types.length-1;
}

void add_instruction(Bytecode_Block *block, Bytecode_Instruction inst) {
  assert(!block->is_concluded);
  if(inst.type == BC_RETURN || inst.type == BC_BRANCH || inst.type == BC_COND_BRANCH)
    block->is_concluded = true;
  Bytecode_Instruction_Array_push(&block->instructions, inst);
}

Bytecode_Block init_bytecode_block() {
  return (Bytecode_Block) {init_Bytecode_Instruction_Array(2), false};
}







u32 add_block_to_block(Ast_Block *block_node, Bytecode_Function *fn, u32 *block);
Bytecode_Ast_Block generate_bytecode_block(Ast_Node *node, Bytecode_Function *fn, Scope *scope);

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
    assert(n->operator == OPSTRUCT_MEMBER);
    u32 first_reg; // this should be a single pointer to a struct.
    if(n->first->type == NODE_SYMBOL) {
      Ast_Symbol *sn = (Ast_Symbol *)n->first;
      Scope_Entry *e = get_entry_of_identifier_block_scope(sn->symbol, scope, sn->n.loc);
      u32 struct_reg = e->register_id;
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
      assert(first_type.info->type == TYPE_STRUCT);
      Struct_Member_Array members = first_type.info->data.struct_.members;

      bool found = false;
      for(int i = 0; i < members.length; i++) {
        if(members.data[i].symbol == s->symbol) {
          found = true;
          Compilation_Unit *unit = members.data[i].unit;
          assert(unit->type == UNIT_STRUCT_MEMBER);
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
  } else assert(false);
}

// returns the register storing the result
u32 generate_bytecode_expr(Ast_Node *node, u32 *block, Bytecode_Function *fn, Scope *scope) {
  switch(node->type) {
    case NODE_LITERAL: {
      Ast_Literal *n = (Ast_Literal *)node;
      Bytecode_Instruction inst;
      inst.type = BC_SET_LITERAL;
      inst.data.set_literal.lit_b = n->value;
      inst.data.set_literal.reg_a = add_register(fn, solidify_type(n->type, *node));
      add_instruction(&fn->blocks.data[*block], inst);
      return inst.data.set_literal.reg_a;
    }
    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = (Ast_Binary_Op *)node;
      switch(n->operator) {
        #define MACRO_ADD_BINARY_BYTECODE(op_type, inst_type, result_reg)\
          case op_type: {\
            u32 reg_b = generate_bytecode_expr(n->first, block, fn, scope);\
            u32 reg_c = generate_bytecode_expr(n->second, block, fn, scope);\
            return add_bin_op_instruction(&fn->blocks.data[*block], inst_type,\
                                          result_reg,\
                                          reg_b,\
                                          reg_c);\
        }
        MACRO_ADD_BINARY_BYTECODE(OPPLUS, BC_ADD, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPMINUS, BC_SUB, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPMUL, BC_MUL, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPDIV, BC_DIV, add_register(fn, n->convert_to))
        MACRO_ADD_BINARY_BYTECODE(OPEQUALS, BC_EQUALS, add_register(fn, BOOL_TYPE))
        MACRO_ADD_BINARY_BYTECODE(OPLESS_THAN, BC_LESS_THAN, add_register(fn, BOOL_TYPE))
        // lookup table maybe?
        case OPSET_EQUALS: {
          u32 value_reg = generate_bytecode_expr(n->second, block, fn, scope);
          Type value_type = fn->register_types.data[value_reg];
          if(n->first->type == NODE_SYMBOL) {
            Ast_Symbol *sn = (Ast_Symbol *)n->first;
            symbol symbol = sn->symbol;

            Scope_Entry *e = get_entry_of_identifier_block_scope(symbol, scope, sn->n.loc);

            u32 ptr_reg = e->register_id;
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
          assert(struct_type.info->type == TYPE_STRUCT);

          u32 struct_ptr_reg = ref_or_deref_to_single_ptr(struct_reg, block, fn);

          Type result_type;
          u32 member;
          {
            Struct_Member_Array members = struct_type.info->data.struct_.members;

            bool found = false;
            for(int i = 0; i < members.length; i++) {
              if(members.data[i].symbol == s->symbol) {
                found = true;
                Compilation_Unit *unit = members.data[i].unit;
                assert(unit->type == UNIT_STRUCT_MEMBER);
                infer_types_of_compilation_unit(unit);
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
        default: {
          print_error_message("Internal compiler error: I cannot generate bytecode for this node.", node->loc);
          exit(1);
        }
      }

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
      Scope_Entry *e = get_entry_of_identifier_block_scope(n->symbol, scope, node->loc);
      return e->register_id;
    }
    case NODE_BLOCK: {
      return add_block_to_block((Ast_Block *)node, fn, block);
    }
    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)node;
      Scope_Entry *e = get_entry_of_identifier_block_scope(n->symbol, scope, node->loc);
      e->register_id = add_register(fn, n->type);
      return -1;
    }
    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)node;
      Scope_Entry *e = get_entry_of_identifier_block_scope(n->symbol, scope, node->loc);
      u32 reg = add_register(fn, n->type);
      e->register_id = reg;
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      add_instruction(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_TYPED_DECL_SET: {
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)node;
      Scope_Entry *e = get_entry_of_identifier_block_scope(n->symbol, scope, node->loc);
      u32 reg = fn->register_types.length;
      e->register_id = add_register(fn, n->type);
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      add_instruction(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_RETURN: {
      Ast_Return *n = (Ast_Return *)node;
      Bytecode_Instruction inst;
      inst.type = BC_RETURN;
      inst.data.ret.reg = generate_bytecode_expr(n->value, block, fn, scope);
      add_instruction(&fn->blocks.data[*block], inst);
      return -1;
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
    case NODE_FUNCTION_CALL: {
      Ast_Function_Call *n = (Ast_Function_Call *)node;
      Compilation_Unit *signature = n->signature;
      Compilation_Unit *body = signature->data.signature.body;
      infer_types_of_compilation_unit(body);
      generate_bytecode_compilation_unit(body);

      u32 *arg_registers = alloca(n->arguments.length * sizeof(u32));
      for(int i = 0; i < n->arguments.length; i++) {
        arg_registers[i] = generate_bytecode_expr(n->arguments.data[i], block, fn, scope);
      }

      u32 result_reg = add_register(fn, ((Ast_Function_Definition *)body->node)->return_type);
      {
        Bytecode_Instruction inst;
        inst.type = BC_CALL;
        inst.data.call.to = body->data.body.bytecode;
        inst.data.call.reg = result_reg;
        add_instruction(&fn->blocks.data[*block], inst);
      }

      for(int i = 0; i < n->arguments.length; i++) {
        Bytecode_Instruction inst;
        inst.type = BC_ARG;
        inst.data.arg.reg = arg_registers[i];
        add_instruction(&fn->blocks.data[*block], inst);
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


void generate_bytecode_function(Bytecode_Function *r, Ast_Function_Definition *defn, Scope *scope) {
  r->register_types = init_Type_Array(4);

  r->param_count = 0;
  for(int i = 0; i < defn->scope.entries.length; i++) {
    Scope_Entry *e = &defn->scope.entries.data[i];
    if(e->declaration.node->type == NODE_FUNCTION_PARAMETER) {
      Ast_Function_Parameter *param = (Ast_Function_Parameter *)e->declaration.node;
      e->register_id = add_register(r, param->type);
      r->param_count++;
    }
  }
  assert(r->param_count == defn->parameters.length);

  r->blocks = init_Bytecode_Block_Array(2);
  assert(defn->body->type == NODE_BLOCK);
  Ast_Block *body = (Ast_Block *)defn->body;
  u32 entry_result_reg;
  Bytecode_Ast_Block generated = generate_bytecode_block(defn->body, r, scope);
  r->entry_block = generated.entry;
  r->return_type = defn->return_type;


  // add a default return if the block isn't concluded, since every block must be concluded
  if(!r->blocks.data[generated.exit].is_concluded) {
    Bytecode_Instruction set_literal;
    set_literal.type = BC_SET_LITERAL;
    u32 reg = add_register(r, r->return_type);
    set_literal.data.set_literal.reg_a = reg;
    set_literal.data.set_literal.lit_b = 0;
    add_instruction(&r->blocks.data[generated.exit], set_literal);

    Bytecode_Instruction ret;
    ret.type = BC_RETURN;
    ret.data.ret.reg = reg;
    add_instruction(&r->blocks.data[generated.exit], ret);
  }
}

void generate_bytecode_compilation_unit(Compilation_Unit *unit) {
  if(unit->bytecode_generated || unit->bytecode_generating || unit->poisoned) return;

  infer_types_of_compilation_unit(unit);
  if(unit->type == UNIT_FUNCTION_BODY) {
    assert(unit->node->type == NODE_FUNCTION_DEFINITION);
    unit->bytecode_generating = true;
    Ast_Function_Definition *fn = (Ast_Function_Definition *)unit->node;
    generate_bytecode_function(unit->data.body.bytecode, fn, unit->data.body.scope);
    unit->bytecode_generating = false;
    unit->bytecode_generated = true;
  }
}
