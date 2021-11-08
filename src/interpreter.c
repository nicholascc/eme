#include "interpreter.h"

#include "c-utils/integer.h"
#include "bytecode.h"

void interpret_bytecode_function(Bytecode_Function fn) {
  int register_count = fn.register_types.length;
  u64 *r = malloc(sizeof(u64) * register_count);
  Bytecode_Block block = fn.blocks.data[fn.entry_block];
  int inst_i = 0;
  while(true) {
    Bytecode_Instruction inst = block.data[inst_i];
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
        u32 a = inst.data.bin_conv_op.reg_a;
        u32 b = inst.data.bin_conv_op.reg_b;
        u32 c = inst.data.bin_conv_op.reg_c;
        Type_Info b_type = fn.register_types.data[b];
        Type_Info c_type = fn.register_types.data[c];
        Type_Info conv_type = inst.data.bin_conv_op.conv_type;
        assert(b_type.type == TYPE_INT &&
               c_type.type == TYPE_INT &&
               conv_type.type == TYPE_INT);
        // better way to do this comparison?
        if(conv_type.data.integer.is_signed) {
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
      case BC_RETURN: {
        printf("RETURNED: %lli\n", r[inst.data.ret.reg]);
        break;
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
    if(inst_i >= block.length) {
      break;
    }
  }

  free(r);
}
