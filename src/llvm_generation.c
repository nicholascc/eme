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

LLVMTypeRef llvm_type_of(Type_Info type) {
  if(type.type == TYPE_INT) {
    return LLVMIntType(type.data.integer.width);
  } else if(type.type == TYPE_BOOL) {
    return LLVMIntType(1);
  } else {
    assert(false);
  }
}

LLVMValueRef generate_llvm_cast(LLVMBuilderRef builder, LLVMValueRef a, Type_Info a_type, Type_Info b_type) {
  assert(a_type.type == TYPE_INT &&
         b_type.type == TYPE_INT);
  u8 a_width = a_type.data.integer.width;
  u8 b_width = b_type.data.integer.width;
  if(a_width < b_width) {
    if(a_type.data.integer.is_signed)
      return LLVMBuildSExt(builder, a, LLVMIntType(b_width), "");
    else
      return LLVMBuildZExt(builder, a, LLVMIntType(b_width), "");
  } else if(a_width > b_width)
    return LLVMBuildTrunc(builder, a, LLVMIntType(b_width), "");
  else
    return a;
}


void generate_llvm_function(LLVMModuleRef mod, LLVMBuilderRef builder, Bytecode_Function fn) {
  char *error = NULL;

  LLVMValueRef llf = LLVMAddFunction(mod, "eme", LLVMFunctionType(LLVMInt64Type(), NULL, 0, 0));
  LLVMSetFunctionCallConv(llf, LLVMCCallConv);

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
    }
    LLVMBuildBr(builder, llblocks[fn.entry_block]);
  }

  for(int i = 0; i < fn.blocks.length; i++) {
    Bytecode_Block block = fn.blocks.data[i];
    LLVMBasicBlockRef llb = llblocks[i];
    LLVMPositionBuilderAtEnd(builder, llb);

    for(int j = 0; j < block.length; j++) {
      Bytecode_Instruction inst = block.data[j];
      switch(inst.type) {
        case BC_ADD:
        case BC_SUB: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_b], "");
          LLVMValueRef c = LLVMBuildLoad(builder, r[inst.data.bin_op.reg_c], "");

          {
            Type_Info a_type = fn.register_types.data[inst.data.bin_op.reg_a];
            Type_Info b_type = fn.register_types.data[inst.data.bin_op.reg_b];
            Type_Info c_type = fn.register_types.data[inst.data.bin_op.reg_c];

            b = generate_llvm_cast(builder, b, b_type, a_type);
            c = generate_llvm_cast(builder, c, c_type, a_type);
          }

          LLVMValueRef a;
          if(inst.type == BC_ADD) {
            a = LLVMBuildAdd(builder, b, c, "");
          } else {
            a = LLVMBuildSub(builder, b, c, "");
          }

          LLVMBuildStore(builder, a, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_LESS_THAN: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.bin_conv_op.reg_b], "");
          LLVMValueRef c = LLVMBuildLoad(builder, r[inst.data.bin_conv_op.reg_c], "");

          Type_Info conv_type = inst.data.bin_conv_op.conv_type;
          assert(conv_type.type == TYPE_INT);
          {
            Type_Info b_type = fn.register_types.data[inst.data.bin_conv_op.reg_b];
            Type_Info c_type = fn.register_types.data[inst.data.bin_conv_op.reg_c];

            b = generate_llvm_cast(builder, b, b_type, conv_type);
            c = generate_llvm_cast(builder, c, c_type, conv_type);
          }
          LLVMIntPredicate pred = conv_type.data.integer.is_signed ? LLVMIntSLT : LLVMIntULT;
          LLVMValueRef a = LLVMBuildICmp(builder, pred, b, c, "");

          LLVMBuildStore(builder, a, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_SET: {
          LLVMValueRef b = LLVMBuildLoad(builder, r[inst.data.set.reg_b], "");
          {

            Type_Info a_type = fn.register_types.data[inst.data.set.reg_a];
            Type_Info b_type = fn.register_types.data[inst.data.set.reg_b];
            b = generate_llvm_cast(builder, b, b_type, a_type);
          }

          LLVMBuildStore(builder, b, r[inst.data.bin_op.reg_a]);
          break;
        }

        case BC_SET_LITERAL: {
          Type_Info type = fn.register_types.data[inst.data.set_literal.reg_a];
          assert(type.type == TYPE_INT);
          LLVMValueRef a = LLVMConstInt(LLVMIntType(type.data.integer.width), inst.data.set_literal.lit_b, 0);
          LLVMBuildStore(builder, a, r[inst.data.set_literal.reg_a]);
          break;
        }
        case BC_RETURN: {
          LLVMValueRef a = LLVMBuildLoad(builder, r[inst.data.ret.reg], "");
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


  LLVMBuilderRef builder = LLVMCreateBuilder();

  for(int i = 0; i < ast.compilation_units.length; i++) {
    Compilation_Unit unit = *ast.compilation_units.data[i];
    assert(unit.bytecode_generated);
    Bytecode_Function fn = *unit.bytecode.function;
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
