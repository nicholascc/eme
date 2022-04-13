#include "interpreter.h"

#include "c-utils/integer.h"
#include "bytecode.h"

void print_hex(u8 *arr, u32 size) {
  for(int i = 0; i < size; i++) {
    printf("%02hhX ", arr[i]);
  }
  printf("\n");
}

void interpreter_cast(u8 *a, u8 *b, Type a_type, Type b_type) {
  u32 a_size = size_of_type(a_type);
  u32 b_size = size_of_type(b_type);
  if(a_type.info->type == TYPE_INT && a_size < b_size && a_type.info->data.integer.is_signed) {
    assert(b_type.info->type == TYPE_INT);
    s64 x;
    switch(a_size) {
      case 1: x = *((s8 *)a); break;
      case 2: x = *((s16 *)a); break;
      case 4: x = *((s32 *)a); break;
      case 8: x = *((s64 *)a); break;
      default: assert(false);
    }
    switch(b_size) {
      case 1: *((s8 *)b) = (s8)x; break;
      case 2: *((s16 *)b) = (s16)x; break;
      case 4: *((s32 *)b) = (s32)x; break;
      case 8: *((s64 *)b) = (s64)x; break;
      default: assert(false);
    }
  } else if(a_size < b_size) {
    memcpy(b, a, a_size);
    memset((u8 *)(a_size + (u64)b), 0, b_size - a_size);
  } else {
    memcpy(b, a, b_size);
  }
}

u8 *interpret_bytecode_function(Bytecode_Function fn, u8 **params) {
  u32 *r_to_id = malloc(sizeof(u32) * fn.register_types.length);
  u32 local_scope_size = 0;
  for(int i = 0; i < fn.register_types.length; i++) {
    u32 size = size_of_type(fn.register_types.data[i]);
    u32 alignment = alignment_of_size(size);
    if(local_scope_size % alignment != 0) local_scope_size += alignment - local_scope_size % alignment;
    assert(local_scope_size % alignment == 0);

    r_to_id[i] = local_scope_size;
    local_scope_size += size;
  }

  u8 *local = malloc(sizeof(u8) * local_scope_size);
  memset(local, 0, local_scope_size);

  for(int i = 0; i < fn.passed_param_count; i++) {
    u32 size = size_of_type(fn.register_types.data[i]);
    memcpy(&local[r_to_id[i]], params[i], size);
  }
  free(params);

  Bytecode_Block block = fn.blocks.data[fn.entry_block];
  int inst_i = 0;
  while(true) {
    Bytecode_Instruction inst = block.instructions.data[inst_i];
    switch(inst.type) {
      case BC_ADD:
      case BC_SUB:
      case BC_MUL:
      case BC_DIV: {
        u32 reg_a = inst.data.bin_op.reg_a;
        u32 reg_b = inst.data.bin_op.reg_b;
        u32 reg_c = inst.data.bin_op.reg_c;
        Type result_type = fn.register_types.data[reg_a];
        assert(result_type.info->type == TYPE_INT);

        u64 b, c;
        memcpy(&b, &local[r_to_id[reg_b]], size_of_type(fn.register_types.data[reg_b]));
        memcpy(&c, &local[r_to_id[reg_c]], size_of_type(fn.register_types.data[reg_c]));

        u64 a;
        if(inst.type == BC_ADD)
          a = b + c;
        else if(inst.type == BC_SUB)
          a = b - c;
        else if(inst.type == BC_MUL)
          a = b * c;
        else if(inst.type == BC_DIV) {
          if(c == 0) assert(false); // TODO: HANDLE INTERPRETER ERRORS PROPERLY
          a = b / c;
        } else assert(false);

        memcpy(&local[r_to_id[reg_a]], &a, size_of_type(fn.register_types.data[reg_a]));
        break;
      }
      case BC_EQUALS:
      case BC_LESS_THAN:
      case BC_LESS_THAN_EQUALS: {
        u32 reg_a = inst.data.bin_op.reg_a;
        u32 reg_b = inst.data.bin_op.reg_b;
        u32 reg_c = inst.data.bin_op.reg_c;
        Type b_type = fn.register_types.data[reg_b];
        Type c_type = fn.register_types.data[reg_c];
        assert(b_type.info->type == TYPE_INT && c_type.info->type == TYPE_INT);

        u64 b = 0;
        u64 c = 0;
        memcpy(&b, &local[r_to_id[reg_b]], size_of_type(fn.register_types.data[reg_b]));
        memcpy(&c, &local[r_to_id[reg_c]], size_of_type(fn.register_types.data[reg_c]));

        bool a;
        if(inst.type == BC_LESS_THAN) {
          if(b_type.info->data.integer.is_signed || c_type.info->data.integer.is_signed) {
            a = *((s64 *)&b) < *((s64 *)&c);
          } else {
            a = b < c;
          }
        } else if(inst.type == BC_LESS_THAN_EQUALS) {
          a = b == c;
          if(b_type.info->data.integer.is_signed || c_type.info->data.integer.is_signed) {
            a |= *((s64 *)&b) < *((s64 *)&c);
          } else {
            a |= b < c;
          }
        } else if(inst.type == BC_EQUALS) {
          a = b == c;
        }
        local[r_to_id[reg_a]] = a;
        break;
      }

      case BC_SET: {
        u32 reg_a = inst.data.set.reg_a;
        u32 reg_b = inst.data.set.reg_b;
        interpreter_cast(&local[r_to_id[reg_b]], &local[r_to_id[reg_a]], fn.register_types.data[reg_b], fn.register_types.data[reg_a]);
        break;
      }
      case BC_SET_LITERAL: {
        u32 reg_a = inst.data.set.reg_a;
        u32 a_size = size_of_type(fn.register_types.data[reg_a]);
        assert(a_size <= 8);
        memcpy(&local[r_to_id[reg_a]], &inst.data.set_literal.lit_b, a_size);
        break;
      }
      case BC_BIT_CAST: {
        u32 reg_a = inst.data.bit_cast.reg_a;
        u32 reg_b = inst.data.bit_cast.reg_b;
        u32 size = size_of_type(fn.register_types.data[reg_a]);
        memcpy(&local[r_to_id[reg_a]], &local[r_to_id[reg_b]], size);
        break;
      }
      case BC_REF_TO: {
        u32 reg_a = inst.data.unary_op.reg_a;
        u32 reg_b = inst.data.unary_op.reg_b;
        assert(size_of_type(fn.register_types.data[reg_a]) == 8);
        u64 *a = (u64 *)(&local[r_to_id[reg_a]]);
        *a = (u64)(&local[r_to_id[reg_b]]);
        break;
      }
      case BC_GET_MEMBER_PTR: {
        u32 reg_a = inst.data.get_member_ptr.reg_a;
        u32 reg_b = inst.data.get_member_ptr.reg_b;
        Type b_type = fn.register_types.data[reg_b];

        u32 member = inst.data.get_member_ptr.member;
        u32 offset;
        {
          Compilation_Unit_Ptr_Array members;
          if(b_type.info->type == TYPE_STRUCT)
            members = b_type.info->data.struct_.members;
          else if(b_type.info->type == TYPE_POLY_INSTANCE)
            members = b_type.info->data.poly_instance.members;
          offset = members.data[member]->data.struct_member.offset;
        }

        u64 b_u64 = *((u64 *)&local[r_to_id[reg_b]]);

        u64 *a = (u64 *)(&local[r_to_id[reg_a]]);
        *a = b_u64 + offset;
        break;
      }
      case BC_LOAD: {
        u32 reg_a = inst.data.unary_op.reg_a;
        u32 reg_b = inst.data.unary_op.reg_b;
        u32 a_size = size_of_type(fn.register_types.data[reg_a]);
        Type b_type = fn.register_types.data[reg_b];
        assert(b_type.reference_count > 0);
        b_type.reference_count--;
        u8 *b = *((u8 **)&local[r_to_id[reg_b]]);
        interpreter_cast(b, &local[r_to_id[reg_a]], b_type, fn.register_types.data[reg_a]);
        break;
      }
      case BC_STORE: {
        u32 reg_a = inst.data.unary_op.reg_a;
        u32 reg_b = inst.data.unary_op.reg_b;
        Type a_type = fn.register_types.data[reg_a];
        assert(a_type.reference_count > 0);
        a_type.reference_count--;
        u8 *a = *((u8 **)&local[r_to_id[reg_a]]);
        interpreter_cast(&local[r_to_id[reg_b]], a, fn.register_types.data[reg_b], a_type);
        break;
      }
      case BC_CALL: {
        Bytecode_Unit *to_call = inst.data.call.to;
        u32 result_reg = inst.data.call.reg;
        bool keep_return_value = inst.data.call.keep_return_value;
        Type_Array param_types;

        switch(to_call->type) {
          case BYTECODE_FUNCTION: {
            Bytecode_Function *u = (Bytecode_Function *)to_call;
            param_types = u->register_types;
            param_types.length = u->passed_param_count;
            break;
          }
          case BYTECODE_FOREIGN_FUNCTION: {
            Bytecode_Foreign_Function *u = (Bytecode_Foreign_Function *)to_call;
            param_types = u->parameter_types;
            break;
          }
          default: printf("%i\n", to_call->type); assert(false);
        }

        u8 **params = malloc(param_types.length * sizeof(u8 *));
        for(int k = 0; k < param_types.length; k++) {
          inst_i++;
          inst = block.instructions.data[inst_i];
          assert(inst.type == BC_ARG);

          Type a_type = fn.register_types.data[inst.data.arg.reg];
          params[k] = malloc(size_of_type(a_type));
          interpreter_cast(&local[r_to_id[inst.data.arg.reg]], params[k], a_type, param_types.data[k]);
        }

        u8 *result_ptr = interpret_bytecode_unit(to_call, params);
        if(keep_return_value) {
          int size = size_of_type(fn.register_types.data[result_reg]);
          memcpy(&local[r_to_id[result_reg]], result_ptr, size);
        }
        if(result_ptr) free(result_ptr);
        break;
      }
      case BC_ARG: {
        printf("Internal compiler error: Encountered an arg bytecode instruction not following a call instruction.\n");
        print_bytecode_instruction(inst);
        exit(1);
      }
      case BC_RETURN: {
        u32 reg = inst.data.ret.reg;
        u8 *result = calloc(size_of_type(fn.return_type), sizeof(u8));
        interpreter_cast(&local[r_to_id[reg]], result, fn.register_types.data[reg], fn.return_type);
        free(r_to_id);
        free(local);
        return result;
      }
      case BC_RETURN_NOTHING: {
        assert(fn.return_type.info->type == TYPE_NOTHING);
        free(r_to_id);
        free(local);
        return NULL;
      }
      case BC_BRANCH: {
        block = fn.blocks.data[inst.data.branch.block];
        inst_i = -1;
        break;
      }
      case BC_COND_BRANCH: {
        u32 cond_reg = inst.data.cond_branch.reg_cond;
        if(local[r_to_id[cond_reg]] != 0)
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


u8 *interpret_bytecode_unit(Bytecode_Unit *unit, u8 **params) {
  switch(unit->type) {
    case BYTECODE_FUNCTION: {
      return interpret_bytecode_function(*(Bytecode_Function *)unit, params);
    }
    case BYTECODE_FOREIGN_FUNCTION: {
      Bytecode_Foreign_Function *fn = (Bytecode_Foreign_Function *)unit;
      u8 *result = NULL;

      if(fn->u.name == st_get_id_of("malloc", -1)) {
        result = malloc(sizeof(u64));
        u64 param;
        memcpy(&param, params[0], sizeof(u64));
        u64 *r_u64 = (u64 *)result;
        *r_u64 = (u64)malloc(param);
      } else if(fn->u.name == st_get_id_of("free", -1)) {
        u64 param;
        memcpy(&param, params[0], sizeof(u64));
        free((u8 *)param);
      } else {
        printf("The interpreted code tried to call an undefined foreign function: '%s'.\n", st_get_str_of(fn->u.name));
        exit(1);
      }

      free(params);
      return result;
    }
    default: assert(false);
  }
}
