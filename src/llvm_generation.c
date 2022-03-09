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
void generate_llvm_function_signature(LLVMModuleRef mod, Compilation_Unit unit) {
  assert(unit.type == UNIT_FUNCTION_BODY);
  Bytecode_Function *fn = unit.data.body.bytecode;
  LLVMTypeRef *arg_types = malloc(fn->param_count * sizeof(LLVMTypeRef));
  for(int i = 0; i < fn->param_count; i++) {
    arg_types[i] = llvm_type_of(fn->register_types.data[i]);
  }

  char *name = st_get_str_of(((Ast_Function_Definition *)unit.node)->symbol);

  LLVMValueRef llf = LLVMAddFunction(mod, name, LLVMFunctionType(llvm_type_of(fn->return_type), arg_types, fn->param_count, false));
  LLVMSetFunctionCallConv(llf, LLVMCCallConv);

  fn->llvm_function =  llf;
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
      if(i < fn.param_count) {
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

        case BC_EQUALS:
        case BC_LESS_THAN: {
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
          Bytecode_Function to_call = *inst.data.call.to;
          u32 result_reg = inst.data.call.reg;
          LLVMValueRef llto_call = to_call.llvm_function;
          LLVMValueRef *args = malloc(to_call.param_count *sizeof(LLVMValueRef));
          for(int k = 0; k < to_call.param_count; k++) {
            j++;
            inst = block.instructions.data[j];
            assert(inst.type == BC_ARG);
            u32 reg = inst.data.arg.reg;
            LLVMValueRef loaded = LLVMBuildLoad(builder, r[reg], "");
            args[k] = generate_llvm_cast(builder, loaded, fn.register_types.data[reg], to_call.register_types.data[k]);
          }
          LLVMValueRef a = LLVMBuildCall(builder, llto_call, args, to_call.param_count, "");
          LLVMBuildStore(builder, a, r[result_reg]);
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

  LLVMVerifyModule(mod, LLVMAbortProcessAction, &error);
  LLVMDisposeMessage(error);
}

void llvm_generate_module(Ast ast, char *out_obj, char *out_asm, char *out_ir) {
  char *error = NULL;
  bool is_error = false;

  LLVMModuleRef mod = LLVMModuleCreateWithName("main_module");

  for(int i = 0; i < ast.compilation_units.length; i++) {
    Compilation_Unit unit = *ast.compilation_units.data[i];
    if(unit.type != UNIT_FUNCTION_BODY) continue;
    assert(unit.bytecode_generated);
    generate_llvm_function_signature(mod, unit);
  }

  LLVMBuilderRef builder = LLVMCreateBuilder();

  for(int i = 0; i < ast.compilation_units.length; i++) {
    Compilation_Unit unit = *ast.compilation_units.data[i];
    if(unit.type != UNIT_FUNCTION_BODY) continue;
    Bytecode_Function fn = *unit.data.body.bytecode;
    generate_llvm_function(mod, builder, fn);
  }

  LLVMDisposeBuilder(builder);


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


  LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
  LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, 0);
  LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
  LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
  LLVMPassManagerBuilderDispose(pass_manager_builder);
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
