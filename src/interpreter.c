#include "interpreter.h"

#include "math.h"
#include <stdio.h>

#include "c-utils/integer.h"
#include "bytecode.h"

typedef float f32;
typedef double f64;

void print_hex(u8 *arr, u32 size) {
  for(int i = 0; i < size; i++) {
    printf("%02hhX ", arr[i]);
  }
  printf("\n");
}

void interpreter_cast(u8 *a, u8 *b, Type a_type, Type b_type) {
  u32 a_size = size_of_type(a_type);
  u32 b_size = size_of_type(b_type);

  if(type_equals(a_type, b_type)) {
    memcpy(b, a, a_size);
  }

  if(a_type.info->type == TYPE_INT) {
    if(b_type.info->type == TYPE_INT) {
      if(a_size < b_size && a_type.info->data.integer.is_signed) {
        s64 x;
        switch(a_size) {
          case 1: x = *((s8 *)a); break;
          case 2: x = *((s16 *)a); break;
          case 4: x = *((s32 *)a); break;
          case 8: x = *((s64 *)a); break;
          default: assert(false);
        }
        switch(b_size) {
          case 1: *((s8 *)b)  = (s8)x; break;
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
    } else if(b_type.info->type == TYPE_FLOAT) {
      if(a_type.info->data.integer.is_signed) {
        s64 x;
        switch(a_size) {
          case 1: x = *((s8 *)a); break;
          case 2: x = *((s16 *)a); break;
          case 4: x = *((s32 *)a); break;
          case 8: x = *((s64 *)a); break;
          default: assert(false);
        }
        switch(b_size) {
          case 4: *((f32 *)b) = (f32)x; break;
          case 8: *((f64 *)b) = (f64)x; break;
          default: assert(false);
        }
      }
    } else assert(false);

  } else if(a_type.info->type == TYPE_FLOAT) {
    f64 x;
    switch(a_size) {
      case 4: x = *((f32 *)a); break;
      case 8: x = *((f64 *)a); break;
      default: assert(false);
    }
    if(b_type.info->type == TYPE_FLOAT) {
      switch(b_size) {
        case 4: *((f32 *)b) = (f32)x; break;
        case 8: *((f64 *)b) = x; break;
        default: assert(false);
      }
    } else if(b_type.info->type == TYPE_INT) {
      if(b_type.info->data.integer.is_signed) {
        switch(b_size) {
          case 1: *((s8 *)b)  = (s8)x; break;
          case 2: *((s16 *)b) = (s16)x; break;
          case 4: *((s32 *)b) = (s32)x; break;
          case 8: *((s64 *)b) = (s64)x; break;
          default: assert(false);
        }
      } else {
        switch(b_size) {
          case 1: *((u8 *)b)  = (u8)x; break;
          case 2: *((u16 *)b) = (u16)x; break;
          case 4: *((u32 *)b) = (u32)x; break;
          case 8: *((u64 *)b) = (u64)x; break;
          default: assert(false);
        }
      }
    } else assert(false);
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
        Type b_type = fn.register_types.data[reg_b];
        Type c_type = fn.register_types.data[reg_c];

        assert(result_type.reference_count == 0);

        if(result_type.info->type == TYPE_INT) {
          u64 b, c;
          interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b, b_type, result_type);
          interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c, c_type, result_type);

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

          memcpy(&local[r_to_id[reg_a]], &a, size_of_type(result_type));


        } else if(result_type.info->type == TYPE_FLOAT) {
          f64 b, c;
          if(result_type.info->data.float_.width == 32) {
            f32 b32, c32;
            interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b32, b_type, result_type);
            interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c32, c_type, result_type);
            b = (f64)b32;
            c = (f64)c32;
          } else if(result_type.info->data.float_.width == 64) {
            interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b, b_type, result_type);
            interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c, c_type, result_type);
          } else assert(false);

          f64 a;
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

          if(result_type.info->data.float_.width == 32) {
            f32 a32 = (f32)a;
            memcpy(&local[r_to_id[reg_a]], &a32, size_of_type(result_type));
          } else if(result_type.info->data.float_.width == 64) {
            memcpy(&local[r_to_id[reg_a]], &a, size_of_type(result_type));
          } else assert(false);
        } else assert(false);
        break;
      }
      case BC_OR:
      case BC_AND: {
        u32 reg_a = inst.data.bin_op.reg_a;
        u32 reg_b = inst.data.bin_op.reg_b;
        u32 reg_c = inst.data.bin_op.reg_c;
        Type b_type = fn.register_types.data[reg_b];
        Type c_type = fn.register_types.data[reg_c];
        assert(b_type.reference_count == 0 && b_type.info->type == TYPE_BOOL &&
               c_type.reference_count == 0 && c_type.info->type == TYPE_BOOL);

        u8 b = local[r_to_id[reg_b]];
        u8 c = local[r_to_id[reg_c]];
        u8 a;
        if(inst.type == BC_AND) {
          a = b && c;
        } else if(inst.type == BC_OR) {
          a = b || c;
        } else assert(false);

        local[r_to_id[reg_a]] = a;
        break;
      }
      case BC_NOT: {
        u32 reg_a = inst.data.bin_op.reg_a;
        u32 reg_b = inst.data.bin_op.reg_b;
        Type b_type = fn.register_types.data[reg_b];
        assert(b_type.reference_count == 0 && b_type.info->type == TYPE_BOOL);
        u8 b = local[r_to_id[reg_b]];
        u8 a = !b;
        local[r_to_id[reg_a]] = a;
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

        if(b_type.info->type == TYPE_INT) {
          Type conv_type = integer_type_with(b_type.info->data.integer.is_signed || c_type.info->data.integer.is_signed, max(b_type.info->data.integer.width, c_type.info->data.integer.width));
          u64 b, c;
          interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b, b_type, conv_type);
          interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c, c_type, conv_type);

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


        } else if(b_type.info->type == TYPE_FLOAT) {
          Type conv_type;
          if(can_implicitly_cast_type(b_type, c_type)) {
            conv_type = c_type;
          } else if(can_implicitly_cast_type(c_type, b_type)) {
            conv_type = b_type;
          } else assert(false);

          f64 b, c;
          if(conv_type.info->data.float_.width == 32) {
            f32 b32, c32;
            interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b32, b_type, conv_type);
            interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c32, c_type, conv_type);
            b = (f64)b32;
            c = (f64)c32;
          } else if(conv_type.info->data.float_.width == 64) {
            interpreter_cast(&local[r_to_id[reg_b]], (u8*)&b, b_type, conv_type);
            interpreter_cast(&local[r_to_id[reg_c]], (u8*)&c, c_type, conv_type);
          } else assert(false);

          bool a;
          if(inst.type == BC_LESS_THAN) {
            a = b < c;
          } else if(inst.type == BC_LESS_THAN_EQUALS) {
            a = b <= c;
          } else if(inst.type == BC_EQUALS) {
            a = b == c;
          }
          local[r_to_id[reg_a]] = a;
        } else assert(false);
        break;
      }

      case BC_SET: {
        u32 reg_a = inst.data.set.reg_a;
        u32 reg_b = inst.data.set.reg_b;
        interpreter_cast(&local[r_to_id[reg_b]], &local[r_to_id[reg_a]], fn.register_types.data[reg_b], fn.register_types.data[reg_a]);
        break;
      }
      case BC_SET_LITERAL: {
        u32 reg_a = inst.data.set_literal.reg_a;
        u32 a_size = size_of_type(fn.register_types.data[reg_a]);
        assert(a_size <= 8);
        memcpy(&local[r_to_id[reg_a]], &inst.data.set_literal.lit_b, a_size);
        break;
      }
      case BC_SET_PTR_LITERAL: {
        u32 reg_a = inst.data.set_ptr_literal.reg_a;
        u32 a_size = size_of_type(fn.register_types.data[reg_a]);
        assert(a_size <= 8);
        memcpy(&local[r_to_id[reg_a]], &inst.data.set_ptr_literal.ptr, a_size);
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
      } else if(fn->u.name == st_get_id_of("realloc", -1)) {
        result = malloc(sizeof(u64));
        u64 param0;
        memcpy(&param0, params[0], sizeof(u64));
        u64 param1;
        memcpy(&param1, params[1], sizeof(u64));

        u64 *r_u64 = (u64 *)result;
        *r_u64 = (u64)realloc((void *)param0, param1);
      } else if(fn->u.name == st_get_id_of("free", -1)) {
        u64 param;
        memcpy(&param, params[0], sizeof(u64));
        free((u8 *)param);
      } else if(fn->u.name == st_get_id_of("putchar", -1)) {
        u8 param;
        memcpy(&param, params[0], sizeof(u8));
        printf("%c", param);
      } else if(fn->u.name == st_get_id_of("getchar", -1)) {
        result = malloc(sizeof(s32));
        s32 *r_s32 = (s32 *)result;
        *r_s32 = (s32)getchar();
      } else if(fn->u.name == st_get_id_of("isspace", -1)) {
        result = malloc(sizeof(u8));
        u8 *r_u8 = (u8 *)result;
        char param0;
        memcpy(&param0, params[0], sizeof(char));
        *r_u8 = isspace(param0);
      } else if(fn->u.name == st_get_id_of("isdigit", -1)) {
        result = malloc(sizeof(u8));
        u8 *r_u8 = (u8 *)result;
        char param0;
        memcpy(&param0, params[0], sizeof(char));
        *r_u8 = isdigit(param0);
      } else if(fn->u.name == st_get_id_of("isalpha", -1)) {
        result = malloc(sizeof(u8));
        u8 *r_u8 = (u8 *)result;
        char param0;
        memcpy(&param0, params[0], sizeof(char));
        *r_u8 = isalpha(param0);
      } else if(fn->u.name == st_get_id_of("exit", -1)) {
        u64 param;
        memcpy(&param, params[0], sizeof(u64));
        printf("\nThe interpreted code exited with error code %llu.\n", param);
        exit(param);
      } else if(fn->u.name == st_get_id_of("sqrtl", -1)) {
        result = malloc(sizeof(f64));
        f64 param;
        memcpy(&param, params[0], sizeof(f64));
        f64 *r_u64 = (f64 *)result;
        *r_u64 = sqrtl(param);
      } else if(fn->u.name == st_get_id_of("print_float", -1)) {
        f64 param;
        memcpy(&param, params[0], sizeof(f64));
        printf("%lf", param);
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
