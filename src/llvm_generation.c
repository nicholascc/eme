#include "llvm_generation.h"

#include <stdio.h>

#include "bytecode.h"
#include "errors.h"

#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Transforms/PassManagerBuilder.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/TargetMachine.h>

LLVMTypeRef llvm_type_of(Type type) {
  if(type.reference_count > 0) {
    return LLVMPointerType(llvm_type_of((Type){type.reference_count-1, type.info}), 0);
  } else {
    Type_Info *info = type.info;
    if(info->type == TYPE_INT) {
      return LLVMIntType(info->data.integer.width);
    } else if(info->type == TYPE_BOOL) {
      return LLVMIntType(1);
    } else if(info->type == TYPE_NOTHING) {
      return LLVMVoidType();
    } else if(info->type == TYPE_STRUCT) {
      if(info->data.struct_.llvm_generated) return info->data.struct_.llvm_type;
      info->data.struct_.llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), st_get_str_of(info->data.struct_.name));
      info->data.struct_.llvm_generated = true;
      u32 element_count = info->data.struct_.members.length;
      LLVMTypeRef *element_types = malloc(sizeof(LLVMTypeRef) * element_count);
      for(int i = 0; i < element_count; i++) {
        element_types[i] = llvm_type_of(info->data.struct_.members.data[i]->data.struct_member.type);
      }
      LLVMStructSetBody(info->data.struct_.llvm_type, element_types, element_count, false);
      return info->data.struct_.llvm_type;
    } else if(info->type == TYPE_POLY_INSTANCE) {
      if(info->data.poly_instance.llvm_generated) return info->data.poly_instance.llvm_type;
      info->data.poly_instance.llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), st_get_str_of(info->data.poly_instance.definition->symbol));
      info->data.poly_instance.llvm_generated = true;
      u32 element_count = info->data.poly_instance.members.length;
      LLVMTypeRef *element_types = malloc(sizeof(LLVMTypeRef) * element_count);
      for(int i = 0; i < element_count; i++) {
        element_types[i] = llvm_type_of(info->data.poly_instance.members.data[i]->data.struct_member.type);
      }
      LLVMStructSetBody(info->data.poly_instance.llvm_type, element_types, element_count, false);
      return info->data.poly_instance.llvm_type;
    } else {
      printf("Cannot convert this type to LLVM: ");
      print_type(type);
      printf("\n");
      exit(1);
    }
  }
}

inline LLVMValueRef generate_llvm_cast(LLVMBuilderRef builder, LLVMValueRef a, Type a_type, Type b_type) {
  if(a_type.info->type == TYPE_INT) {
    assert(b_type.info->type == TYPE_INT);
    u8 a_width = a_type.info->data.integer.width;
    u8 b_width = b_type.info->data.integer.width;
    if(a_width < b_width) {
      if(a_type.info->data.integer.is_signed)
        return LLVMBuildSExt(builder, a, LLVMIntType(b_width), "");
      else
        return LLVMBuildZExt(builder, a, LLVMIntType(b_width), "");
    } else if(a_width > b_width)
      return LLVMBuildTrunc(builder, a, LLVMIntType(b_width), "");
    else
      return a;
  } else {
    return a;
  }
}


// the compilation unit supplied here must be a function body.
void generate_llvm_function_signature(LLVMModuleRef mod, Bytecode_Unit *unit) {
  switch(unit->type) {
    case BYTECODE_FUNCTION: {
      Bytecode_Function *u = (Bytecode_Function *)unit;
      LLVMTypeRef *arg_types = malloc(u->passed_param_count * sizeof(LLVMTypeRef));
      for(int i = 0; i < u->passed_param_count; i++) {
        arg_types[i] = llvm_type_of(u->register_types.data[i]);
      }

      char *name = st_get_str_of(u->u.name);

      LLVMValueRef llf = LLVMAddFunction(mod, name, LLVMFunctionType(llvm_type_of(u->return_type), arg_types, u->passed_param_count, false));
      LLVMSetFunctionCallConv(llf, LLVMCCallConv);
      if(u->is_inline) {
        LLVMAddAttributeAtIndex(llf, LLVMAttributeFunctionIndex, LLVMCreateEnumAttribute(LLVMGetGlobalContext(), LLVMGetEnumAttributeKindForName("alwaysinline", 12), 0));
      }

      u->llvm_function = llf;
      break;
    }
    case BYTECODE_FOREIGN_FUNCTION: {
      Bytecode_Foreign_Function *u = (Bytecode_Foreign_Function *)unit;
      LLVMTypeRef *arg_types = malloc(u->parameter_types.length * sizeof(LLVMTypeRef));
      for(int i = 0; i < u->parameter_types.length; i++) {
        arg_types[i] = llvm_type_of(u->parameter_types.data[i]);
      }

      char *name = st_get_str_of(u->u.name);

      LLVMValueRef llf = LLVMAddFunction(mod, name, LLVMFunctionType(llvm_type_of(u->return_type), arg_types, u->parameter_types.length, false));
      LLVMSetFunctionCallConv(llf, LLVMCCallConv);

      u->llvm_function = llf;
      break;
    }
  }

}



void generate_llvm_function(LLVMModuleRef mod, LLVMBuilderRef builder, Bytecode_Function fn) {
  char *error = NULL;

  LLVMValueRef llf = fn.llvm_function;
  LLVMBasicBlockRef entry_block = LLVMAppendBasicBlock(llf, "");

  LLVMBasicBlockRef *llblocks = malloc(sizeof(LLVMBasicBlockRef) * fn.blocks.length);
  for(int i = 0; i < fn.blocks.length; i++) {
    llblocks[i] = LLVMAppendBasicBlock(llf, "");
  }

  LLVMValueRef *r = malloc(fn.register_types.length * sizeof(LLVMValueRef));
  {
    LLVMPositionBuilderAtEnd(builder, entry_block);
    for(int i = 0; i < fn.register_types.length; i++) {
      LLVMTypeRef type = llvm_type_of(fn.register_types.data[i]);
      r[i] = LLVMBuildAlloca(builder, type, "");
      if(i < fn.passed_param_count) {
        LLVMValueRef arg = LLVMGetParam(llf, i);
        LLVMBuildStore(builder, arg, r[i]);
      }
    }
    LLVMBuildBr(builder, llblocks[fn.entry_block]);
  }

  for(int i = 0; i < fn.blocks.length; i++) {
    Bytecode_Block block = fn.blocks.data[i];
    LLVMBasicBlockRef llb = llblocks[i];
    LLVMPositionBuilderAtEnd(builder, llb);
    for(int j = 0; j < block.instructions.length; j++) {
      Bytecode_Instruction inst = block.instructions.data[j];
      switch(inst.type) {
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_b], "");
          LLVMValueRef c = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_c], "");


          Type a_type = fn.register_types.data[inst.data.bin_op.reg_a];
          assert(a_type.info->type == TYPE_INT);
          {
            Type b_type = fn.register_types.data[inst.data.bin_op.reg_b];
            Type c_type = fn.register_types.data[inst.data.bin_op.reg_c];

            b = generate_llvm_cast(builder, b, b_type, a_type);
            c = generate_llvm_cast(builder, c, c_type, a_type);
          }

          LLVMValueRef a;
          if(inst.type == BC_ADD) {
            a = LLVMBuildAdd(builder, b, c, "");
          } else if(inst.type == BC_SUB) {
            a = LLVMBuildSub(builder, b, c, "");
          } else if(inst.type == BC_MUL) {
            a = LLVMBuildMul(builder, b, c, "");
          } else if(inst.type == BC_DIV) {
            if(a_type.info->data.integer.is_signed) a = LLVMBuildSDiv(builder, b, c, "");
            else a = LLVMBuildUDiv(builder, b, c, "");
          } else assert(false);

          LLVMBuildStore(builder, a, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_OR:
        case BC_AND: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_b], "");
          LLVMValueRef c = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_c], "");
          {
            Type b_type = fn.register_types.data[inst.data.bin_op.reg_b];
            Type c_type = fn.register_types.data[inst.data.bin_op.reg_c];

            b = generate_llvm_cast(builder, b, b_type, BOOL_TYPE);
            c = generate_llvm_cast(builder, c, c_type, BOOL_TYPE);
          }

          LLVMValueRef a;
          if(inst.type == BC_AND) {
            a = LLVMBuildAnd(builder, b, c, "");
          } else if(inst.type == BC_OR) {
            a = LLVMBuildOr(builder, b, c, "");
          } else assert(false);

          LLVMBuildStore(builder, a, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_EQUALS:
        case BC_LESS_THAN:
        case BC_LESS_THAN_EQUALS: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_b], "");
          LLVMValueRef c = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_c], "");
          Type b_type = fn.register_types.data[inst.data.bin_op.reg_b];
          Type c_type = fn.register_types.data[inst.data.bin_op.reg_c];
          assert(b_type.info->type == TYPE_INT && c_type.info->type == TYPE_INT);

          Type conv_type = integer_type_with(b_type.info->data.integer.is_signed || c_type.info->data.integer.is_signed, max(b_type.info->data.integer.width, c_type.info->data.integer.width));

          b = generate_llvm_cast(builder, b, b_type, conv_type);
          c = generate_llvm_cast(builder, c, c_type, conv_type);

          LLVMIntPredicate pred;
          if(inst.type == BC_LESS_THAN) {
            pred = conv_type.info->data.integer.is_signed ? LLVMIntSLT : LLVMIntULT;
          } else if(inst.type == BC_LESS_THAN_EQUALS) {
            pred = conv_type.info->data.integer.is_signed ? LLVMIntSLE : LLVMIntULE;
          } else if(inst.type == BC_EQUALS) {
            pred = LLVMIntEQ;
          } else assert(false);
          LLVMValueRef a = LLVMBuildICmp(builder, pred, b, c, "");
          LLVMBuildStore(builder, a, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_SET: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.set.reg_b], "");
          {

            Type a_type = fn.register_types.data[inst.data.set.reg_a];
            Type b_type = fn.register_types.data[inst.data.set.reg_b];
            b = generate_llvm_cast(builder, b, b_type, a_type);
          }

          LLVMBuildStore(builder, b, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_SET_LITERAL: {
          Type type = fn.register_types.data[inst.data.set_literal.reg_a];
          LLVMTypeRef t = llvm_type_of(type);
          LLVMValueRef a = LLVMConstInt(t, inst.data.set_literal.lit_b, 0);
          LLVMBuildStore(builder, a, r[inst.data.set_literal.reg_a]);
          break;
        }

        case BC_SET_PTR_LITERAL: {
          Type ptr_type = fn.register_types.data[inst.data.set_ptr_literal.reg_a];
          assert(ptr_type.reference_count > 0);
          Type base_type = ptr_type;
          base_type.reference_count--;

          LLVMTypeRef llvm_global_type = LLVMArrayType(LLVMInt8Type(), inst.data.set_ptr_literal.length);
          LLVMValueRef global = LLVMAddGlobal(mod, llvm_global_type, "");
          LLVMSetInitializer(global, LLVMConstString(inst.data.set_ptr_literal.ptr, inst.data.set_ptr_literal.length, true));
          LLVMSetGlobalConstant(global, true);
          LLVMSetLinkage(global, LLVMPrivateLinkage);
          LLVMSetUnnamedAddress(global, LLVMGlobalUnnamedAddr);
          LLVMSetAlignment(global, size_of_type(base_type));

          LLVMValueRef zero = LLVMConstInt(LLVMInt64Type(), 0, true);
          LLVMValueRef indices[2] = {zero, zero};
          LLVMValueRef gep = LLVMBuildGEP2(builder, llvm_global_type, global, indices, 2, "");
          LLVMValueRef a = LLVMBuildBitCast(builder, gep, llvm_type_of(ptr_type), "");
          LLVMBuildStore(builder, gep, r[inst.data.set_ptr_literal.reg_a]);
          break;
        }

        case BC_BIT_CAST: {
          Type a_type = fn.register_types.data[inst.data.bit_cast.reg_a];
          Type b_type = fn.register_types.data[inst.data.bit_cast.reg_b];
          // to do this cast, we just cast the stack pointer we're using, and then
          // copy the value at that stack pointer to our new stack 'register'.
          Type new_ptr_type = a_type;
          new_ptr_type.reference_count++;
          LLVMValueRef new_ptr = LLVMBuildBitCast(builder, r[inst.data.bit_cast.reg_b], llvm_type_of(new_ptr_type), "");
          LLVMValueRef a = LLVMBuildLoad(builder, new_ptr, "");
          LLVMBuildStore(builder, a, r[inst.data.bit_cast.reg_a]);
          break;
        }

        case BC_REF_TO: {
          Type a_type = fn.register_types.data[inst.data.unary_op.reg_a];
          Type b_type = fn.register_types.data[inst.data.unary_op.reg_b];
          assert(b_type.reference_count+1 == a_type.reference_count);
          LLVMValueRef b_ptr = r[inst.data.unary_op.reg_b];
          LLVMBuildStore(builder, b_ptr, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_GET_MEMBER_PTR: {
          Type a_type = fn.register_types.data[inst.data.get_member_ptr.reg_a];
          Type b_type = fn.register_types.data[inst.data.get_member_ptr.reg_b];
          assert(b_type.reference_count == 1);
          b_type.reference_count--;

          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.get_member_ptr.reg_b], "");
          LLVMValueRef a = LLVMBuildStructGEP2(builder, llvm_type_of(b_type), b, inst.data.get_member_ptr.member, "");

          LLVMBuildStore(builder, a, r[inst.data.get_member_ptr.reg_a]);
          break;
        }

        case BC_LOAD: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.unary_op.reg_b], "");
          LLVMValueRef a = LLVMBuildLoad(builder, b, "");
          {
            Type a_type = fn.register_types.data[inst.data.unary_op.reg_a];
            Type b_type = fn.register_types.data[inst.data.unary_op.reg_b];
            b_type.reference_count--;
            a = generate_llvm_cast(builder, a, a_type, b_type);
          }
          LLVMBuildStore(builder, a, r[inst.data.unary_op.reg_a]);
          break;
        }
        case BC_STORE: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.unary_op.reg_b], "");
          {
            Type a_type = fn.register_types.data[inst.data.unary_op.reg_a];
            Type b_type = fn.register_types.data[inst.data.unary_op.reg_b];
            a_type.reference_count--;
            b = generate_llvm_cast(builder, b, b_type, a_type);
          }
          LLVMValueRef a_ptr = LLVMBuildLoad(builder, r[inst.data.unary_op.reg_a], "");
          LLVMBuildStore(builder, b, a_ptr);
          break;
        }

        case BC_CALL: {
          u32 result_reg = inst.data.call.reg;
          bool keep_return_value = inst.data.call.keep_return_value;
          LLVMValueRef llto_call;
          Type_Array param_types;
          {
            Bytecode_Unit *unit = inst.data.call.to;
            switch(unit->type) {
              case BYTECODE_FUNCTION: {
                Bytecode_Function *u = (Bytecode_Function *)unit;
                llto_call = u->llvm_function;
                param_types = u->register_types;
                param_types.length = u->passed_param_count;
                break;
              }
              case BYTECODE_FOREIGN_FUNCTION: {
                Bytecode_Foreign_Function *u = (Bytecode_Foreign_Function *)unit;
                llto_call = u->llvm_function;
                param_types = u->parameter_types;
                break;
              }
              default: assert(false);
            }
          }
          LLVMValueRef *args = malloc(param_types.length *sizeof(LLVMValueRef));
          for(int k = 0; k < param_types.length; k++) {
            j++;
            inst = block.instructions.data[j];
            assert(inst.type == BC_ARG);
            u32 reg = inst.data.arg.reg;
            LLVMValueRef loaded = LLVMBuildLoad(builder, r[reg], "");
            args[k] = generate_llvm_cast(builder, loaded, fn.register_types.data[reg], param_types.data[k]);
          }
          LLVMValueRef a = LLVMBuildCall(builder, llto_call, args, param_types.length, "");
          if(keep_return_value) LLVMBuildStore(builder, a, r[result_reg]);
          break;
        }
        case BC_ARG: {
          printf("Internal compiler error: Encountered an arg bytecode instruction not following a call instruction.\n");
          print_bytecode_instruction(inst);
          exit(1);
        }
        case BC_RETURN: {
          LLVMValueRef a = LLVMBuildLoad(builder, r[inst.data.ret.reg], "");
          a = generate_llvm_cast(builder, a, fn.register_types.data[inst.data.ret.reg], fn.return_type);
          LLVMBuildRet(builder, a);
          break;
        }
        case BC_RETURN_NOTHING: {
          if(fn.return_type.info->type == TYPE_NOTHING)
            LLVMBuildRetVoid(builder);
          else
            LLVMBuildUnreachable(builder);
          break;
        }
        case BC_BRANCH: {
          LLVMBuildBr(builder, llblocks[inst.data.branch.block]);
          break;
        }
        case BC_COND_BRANCH: {
          LLVMValueRef cond = LLVMBuildLoad(builder, r[inst.data.cond_branch.reg_cond], "");
          LLVMBuildCondBr(builder, cond, llblocks[inst.data.cond_branch.block_true], llblocks[inst.data.cond_branch.block_false]);
          break;
        }
        default: {
          printf("Encountered unknown bytecode instruction.\n");
          print_bytecode_instruction(inst);
          exit(1);
        }
      }
    }
  }
  free(llblocks);
  free(r);
}

void llvm_generate_module(Bytecode_Unit_Ptr_Array units, char *out_obj, char *out_asm, char *out_ir) {
  char *error = NULL;
  bool is_error = false;

  LLVMModuleRef mod = LLVMModuleCreateWithName("main_module");

  for(int i = 0; i < units.length; i++) {
    generate_llvm_function_signature(mod, units.data[i]);
  }

  LLVMBuilderRef builder = LLVMCreateBuilder();

  for(int i = 0; i < units.length; i++) {
    if(units.data[i]->type == BYTECODE_FUNCTION)
      generate_llvm_function(mod, builder, *((Bytecode_Function *)units.data[i]));
  }

  LLVMDisposeBuilder(builder);


  printf("Verifying...\n");
  LLVMWriteBitcodeToFile(mod, "out/out.bc");
  is_error = LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
  if(is_error) {
    printf("Internal compiler error in LLVM module verification: %s\n", error);
    exit(1);
  }
  LLVMDisposeMessage(error);

  LLVMInitializeAllTargetInfos();
  LLVMInitializeAllTargets();
  LLVMInitializeAllTargetMCs();
  LLVMInitializeAllAsmParsers();
  LLVMInitializeAllAsmPrinters();

  const char *triple = LLVMGetDefaultTargetTriple();
  printf("Compiling for Target with triple: %s\n", triple);

  LLVMTargetRef Target;
  is_error = LLVMGetTargetFromTriple(triple, &Target, &error);
  if(is_error) print_and_exit(error);
  LLVMTargetMachineRef Target_machine =	LLVMCreateTargetMachine(Target, triple, "generic", "", LLVMCodeGenLevelNone, LLVMRelocDefault, LLVMCodeModelDefault);
  LLVMSetModuleDataLayout(mod, LLVMCreateTargetDataLayout(Target_machine));
  LLVMSetTarget(mod, triple);



  LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
  LLVMAddPromoteMemoryToRegisterPass(pass_manager);
  LLVMAddAlwaysInlinerPass(pass_manager);
  {
    LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
    LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, 1);
    LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
    LLVMPassManagerBuilderDispose(pass_manager_builder);
  }
  LLVMRunPassManager(pass_manager, mod);
  LLVMDisposePassManager(pass_manager);

  if(out_obj) {
    is_error = LLVMTargetMachineEmitToFile(Target_machine, mod, out_obj, LLVMObjectFile, &error);
    if(is_error) print_and_exit(error);
  }

  if(out_asm) {
    is_error = LLVMTargetMachineEmitToFile(Target_machine, mod, out_asm, LLVMAssemblyFile, &error);
    if(is_error) print_and_exit(error);
  }

  if(out_ir) {
    LLVMWriteBitcodeToFile(mod, out_ir);
  }

  printf("Done.\n");

  LLVMDisposeTargetMachine(Target_machine);
  LLVMDisposeModule(mod);
}
