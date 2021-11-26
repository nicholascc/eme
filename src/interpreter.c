#include "interpreter.h"

#include "c-utils/integer.h"
#include "bytecode.h"

u64 *init_interpreted_function_environment(Bytecode_Function fn) {
  return malloc(sizeof(u64) * fn.register_types.length);
}

u64 interpret_bytecode_function(Bytecode_Function fn, u64 *r) {
  Bytecode_Block block = fn.blocks.data[fn.entry_block];
  int inst_i = 0;
  while(true) {
    Bytecode_Instruction inst = block.instructions.data[inst_i];
    switch(inst.type) {
      case BC_ADD:
      case BC_SUB: {
        Type_Info result_type_info = fn.register_types.data[inst.data.bin_op.reg_a];
        assert(result_type_info.type == TYPE_INT);

        // The following works for all integers, signed or unsigned, and of any width.
        u64 result;
        if(inst.type == BC_ADD) {
          result = r[inst.data.bin_op.reg_b] + r[inst.data.bin_op.reg_c];
        } else {
          result = r[inst.data.bin_op.reg_b] - r[inst.data.bin_op.reg_c];
        }
        // Clear higher bits which may have junk data.
        u8 width = result_type_info.data.integer.width;
        if(width == 8) {
          result &= 0xff;
        } else if(width == 16) {
          result &= 0xffff;
        } else if(width == 32) {
          result &= 0xffffffff;
        }
        r[inst.data.bin_op.reg_a] = result;
        break;
      }
      case BC_LESS_THAN: {
        u32 a = inst.data.bin_op.reg_a;
        u32 b = inst.data.bin_op.reg_b;
        u32 c = inst.data.bin_op.reg_c;
        Type_Info b_type = fn.register_types.data[b];
        Type_Info c_type = fn.register_types.data[c];
        assert(b_type.type == TYPE_INT && c_type.type == TYPE_INT);
        // better way to do this comparison?
        if(b_type.data.integer.is_signed || c_type.data.integer.is_signed) {
          s64 *bv = &r[b];
          s64 *cv = &r[c];
          r[a] = *bv < *cv;
        } else {
          u64 *bv = &r[b];
          u64 *cv = &r[c];
          r[a] = *bv < *cv;
        }
        break;
      }
      case BC_SET: {
        r[inst.data.set.reg_a] = r[inst.data.set.reg_b];
        break;
      }
      case BC_SET_LITERAL: {
        r[inst.data.set_literal.reg_a] = inst.data.set_literal.lit_b;
        break;
      }
      case BC_CALL: {
        Bytecode_Function to_call = *inst.data.call.to;
        u32 result_reg = inst.data.call.reg;
        u64 *env = init_interpreted_function_environment(to_call);

        for(int k = 0; k < to_call.param_count; k++) {
          inst_i++;
          inst = block.instructions.data[inst_i];
          assert(inst.type == BC_ARG);
          env[k] = r[inst.data.arg.reg];
        }
        r[result_reg] = interpret_bytecode_function(to_call, env);
        break;
      }
      case BC_ARG: {
        printf("Internal compiler error: Encountered an arg bytecode instruction not following a call instruction.\n");
        print_bytecode_instruction(inst);
        exit(1);
      }
      case BC_RETURN: {
        u64 result = r[inst.data.ret.reg];
        free(r);
        return result;
      }
      case BC_BRANCH: {
        block = fn.blocks.data[inst.data.branch.block];
        inst_i = -1;
        break;
      }
      case BC_COND_BRANCH: {
        if(r[inst.data.cond_branch.reg_cond])
          block = fn.blocks.data[inst.data.cond_branch.block_true];
        else
          block = fn.blocks.data[inst.data.cond_branch.block_false];
        inst_i = -1;
        break;
      }
      default: {
        printf("Encountered unknown bytecode instruction:\n");
        print_bytecode_instruction(inst);
        exit(1);
      }
    }

    inst_i++;
    if(inst_i >= block.instructions.length) {
      break;
    }
  }
}
