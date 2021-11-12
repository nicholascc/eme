#include "bytecode.h"

#include <stdio.h>

#include "c-utils/integer.h"

#include "ast.h"
#include "type_inference.h"
#include "errors.h"

GENERATE_DARRAY_CODE(Bytecode_Instruction, Bytecode_Block);
GENERATE_DARRAY_CODE(Bytecode_Block, Bytecode_Block_Array);
GENERATE_DARRAY_CODE(Type_Info, Type_Info_Array);
GENERATE_DARRAY_CODE(Bytecode_Function, Bytecode_Function_Array);

void print_bytecode_instruction(Bytecode_Instruction inst) {
  switch(inst.type) {
    case BC_ADD: {
      printf("ADD %i <- %i %i\n", inst.data.bin_op.reg_a, inst.data.bin_op.reg_b, inst.data.bin_op.reg_c);
      break;
    }
    case BC_LESS_THAN: {
      printf("LESS_THAN %i <- %i %i\n", inst.data.bin_conv_op.reg_a, inst.data.bin_conv_op.reg_b, inst.data.bin_conv_op.reg_c);
      break;
    }
    case BC_SET_LITERAL: {
      printf("SET_LITERAL %i <- {%lli}\n", inst.data.set_literal.reg_a, inst.data.set_literal.lit_b);
      break;
    }
    case BC_SET: {
      printf("SET %i <- %i\n", inst.data.set.reg_a, inst.data.set.reg_b);
      break;
    }
    case BC_RETURN: {
      printf("RETURN %i\n", inst.data.ret.reg);
      break;
    }
    case BC_BRANCH: {
      printf("BRANCH %i\n", inst.data.branch.block);
      break;
    }
    case BC_COND_BRANCH: {
      printf("COND_BRANCH %i ? [%i, %i]\n", inst.data.cond_branch.reg_cond, inst.data.cond_branch.block_true, inst.data.cond_branch.block_false);
      break;
    }
    default: {
      printf("<cannot print this instruction>\n");
    }
  }
}

void print_bytecode_block(Bytecode_Block block) {
  for(int i = 0; i < block.length; i++) {
    printf("      ");
    print_bytecode_instruction(block.data[i]);
  }
}

void print_bytecode_function(Bytecode_Function fn) {
  printf("function :: {\n");
  printf("  .registers :: {\n");
  for(int i = 0; i < fn.register_types.length; i++) {
    printf("    %i: ", i);
    print_type_info(fn.register_types.data[i]);
    printf("\n");
  }
  printf("  }\n");
  printf("  .blocks :: {\n");
  for(int i = 0; i < fn.blocks.length; i++) {
    printf("    %i:\n", i);
    print_bytecode_block(fn.blocks.data[i]);
  }
  printf("  }\n}\n");
}

void print_bytecode_compilation_unit(Compilation_Unit *unit) {
  if(unit->bytecode_generated)
    print_bytecode_function(*unit->bytecode.function);
  else printf("<bytecode not generated yet>\n");
}








u32 add_block_to_block(Ast_Block *block_node, Bytecode_Function *fn, u32 *block);
u32 generate_bytecode_block(Ast_Node *node, Bytecode_Function *fn, Scope *scope);

// returns the register storing the result
u32 generate_bytecode_expr(Ast_Node *node, u32 *block, Bytecode_Function *fn, Scope *scope) {
  switch(node->type) {
    case NODE_LITERAL: {
      Ast_Literal *n = node;
      Bytecode_Instruction inst;
      inst.type = BC_SET_LITERAL;
      inst.data.set_literal.lit_b = n->value;
      inst.data.set_literal.reg_a = fn->register_types.length;
      Type_Info_Array_push(&fn->register_types, solidify_type(n->type, *node));
      Bytecode_Block_push(&fn->blocks.data[*block], inst);
      return inst.data.set_literal.reg_a;
    }
    case NODE_BINARY_OP: {
      Ast_Binary_Op *n = node;
      switch(n->operator) {
        case OPPLUS: {
          Bytecode_Instruction inst;
          inst.type = BC_ADD;
          inst.data.bin_op.reg_b = generate_bytecode_expr(n->first, block, fn, scope);
          inst.data.bin_op.reg_c = generate_bytecode_expr(n->second, block, fn, scope);
          inst.data.bin_op.reg_a = fn->register_types.length;
          Type_Info_Array_push(&fn->register_types, n->convert_to);
          Bytecode_Block_push(&fn->blocks.data[*block], inst);
          return inst.data.bin_op.reg_a;
        }
        case OPLESS_THAN: {
          Bytecode_Instruction inst;
          inst.type = BC_LESS_THAN;
          inst.data.bin_conv_op.reg_b = generate_bytecode_expr(n->first, block, fn, scope);
          inst.data.bin_conv_op.reg_c = generate_bytecode_expr(n->second, block, fn, scope);
          inst.data.bin_conv_op.reg_a = fn->register_types.length;
          assert(n->convert_to.type == TYPE_INT);
          inst.data.bin_conv_op.conv_type = n->convert_to;
          Type_Info_Array_push(&fn->register_types, BOOL_TYPE_INFO);
          Bytecode_Block_push(&fn->blocks.data[*block], inst);
          return inst.data.bin_conv_op.reg_a;
        } // should combine these two cases above into single simplified code
        case OPSET_EQUALS: {
          u64 symbol;
          {
            Ast_Symbol *sn = n->first;
            assert(sn->n.type == NODE_SYMBOL);
            symbol = sn->symbol;
          }
          Scope_Entry *e = get_entry_of_identifier_in_scope(symbol, scope, node);
          u32 reg = e->register_id;
          Bytecode_Instruction inst;
          inst.type = BC_SET;
          inst.data.set.reg_a = reg;
          inst.data.set.reg_b = generate_bytecode_expr(n->second, block, fn, scope);
          Bytecode_Block_push(&fn->blocks.data[*block], inst);
          return reg;
        }
        default: {
          print_error_message("Internal compiler error: I cannot generate bytecode for this node.", node->loc);
          exit(1);
        }
      }

    }
    case NODE_SYMBOL: {
      Ast_Symbol *n = node;
      return get_entry_of_identifier_in_scope(n->symbol, scope, node)->register_id;
    }
    case NODE_BLOCK: {
      return add_block_to_block(node, fn, block);
    }
    case NODE_TYPED_DECL: {
      Ast_Typed_Decl *n = node;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, node);
      e->register_id = fn->register_types.length;
      Type_Info_Array_push(&fn->register_types, n->type_info);
      return -1;
    }
    case NODE_UNTYPED_DECL_SET: {
      Ast_Untyped_Decl_Set *n = node;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, node);
      u32 reg = fn->register_types.length;
      e->register_id = reg;
      Type_Info_Array_push(&fn->register_types, n->type_info);
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      Bytecode_Block_push(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_TYPED_DECL_SET: {
      Ast_Typed_Decl_Set *n = node;
      Scope_Entry *e = get_entry_of_identifier_in_scope(n->symbol, scope, node);
      u32 reg = fn->register_types.length;
      e->register_id = reg;
      Type_Info_Array_push(&fn->register_types, n->type_info);
      Bytecode_Instruction inst;
      inst.type = BC_SET;
      inst.data.set.reg_a = reg;
      inst.data.set.reg_b = generate_bytecode_expr(n->value, block, fn, scope);
      Bytecode_Block_push(&fn->blocks.data[*block], inst);
      return reg;
    }
    case NODE_RETURN: {
      Ast_Return *n = node;
      Bytecode_Instruction inst;
      inst.type = BC_RETURN;
      inst.data.ret.reg = generate_bytecode_expr(n->value, block, fn, scope);
      Bytecode_Block_push(&fn->blocks.data[*block], inst);
      return -1;
    }
    case NODE_IF: {
      Ast_If *n = node;
      Bytecode_Instruction cond;
      cond.type = BC_COND_BRANCH;
      cond.data.cond_branch.reg_cond = generate_bytecode_expr(n->cond, block, fn, scope);
      u32 prev_block = *block;
      Bytecode_Block_Array_push(&fn->blocks, init_Bytecode_Block(2));
      *block = fn->blocks.length - 1;

      u32 block_true = generate_bytecode_block(n->first, fn, scope);
      cond.data.cond_branch.block_true = block_true;
      {
        Bytecode_Instruction inst;
        inst.type = BC_BRANCH;
        inst.data.branch.block = *block;
        Bytecode_Block_push(&fn->blocks.data[block_true], inst);
      }


      u32 if_false_result;
      u32 block_false = generate_bytecode_block(n->second, fn, scope);
      cond.data.cond_branch.block_false = block_false;
      {
        Bytecode_Instruction inst;
        inst.type = BC_BRANCH;
        inst.data.branch.block = *block;
        Bytecode_Block_push(&fn->blocks.data[cond.data.cond_branch.block_false], inst);
      }

      assert(!n->result_is_used);

      Bytecode_Block_push(&fn->blocks.data[prev_block], cond);
      return -1;
    }
    case NODE_NULL: {
      return -1;
    }
    case NODE_UNARY_OP:
    case NODE_FUNCTION_CALL:
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
  }
  return last_register;
}

// returns the id of the generated block, and places in the pointer the id of the
// register storing the result of the block (the result of the last statement)
u32 generate_bytecode_block(Ast_Node *node, Bytecode_Function *fn, Scope *scope) {
  Bytecode_Block_Array_push(&fn->blocks, init_Bytecode_Block(2));
  u32 starting_block = fn->blocks.length - 1;
  u32 block = starting_block;

  if(node->type == NODE_BLOCK) {
    add_block_to_block(node, fn, &block);
  } else {
    generate_bytecode_expr(node, &block, fn, scope);
  }

  return starting_block;
}


Bytecode_Function *generate_bytecode_function(Ast_Function_Definition *defn, Scope *scope) {
  Bytecode_Function *r = malloc(sizeof(Bytecode_Function));
  r->register_types = init_Type_Info_Array(4);
  r->blocks = init_Bytecode_Block_Array(2);
  assert(defn->body->type == NODE_BLOCK);
  Ast_Block *body = defn->body;
  u32 entry_result_reg;
  r->entry_block = generate_bytecode_block(defn->body, r, scope);
  return r;
}

void generate_bytecode_compilation_unit(Compilation_Unit *unit, Scope *scope) {
  if(unit->bytecode_generated) return;
  assert(!unit->bytecode_generation_seen && "circular dependency");
  assert(!unit->poisoned && "unit poisoned");

  infer_types_of_compilation_unit(unit, scope);
  if(unit->type == UNIT_FUNCTION_BODY) {
    assert(unit->node->type == NODE_FUNCTION_DEFINITION);
    Ast_Function_Definition *fn = unit->node;
    unit->bytecode.function = generate_bytecode_function(fn, scope);
    unit->bytecode_generated = true;
  }
}
