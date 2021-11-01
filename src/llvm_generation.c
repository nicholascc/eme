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
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>


void generate_llvm_function(LLVMModuleRef mod, LLVMBuilderRef builder, Bytecode_Function fn) {
  char *error = NULL;

  LLVMValueRef llf = LLVMAddFunction(mod, "eme", LLVMFunctionType(LLVMInt64Type(), NULL, 0, 0));
  LLVMSetFunctionCallConv(llf, LLVMCCallConv);

  // THIS WILL NOT WORK FOR BRANCHES - IT IS JUST A HACK TO GET US STARTED!
  LLVMValueRef *r = malloc(fn.register_types.length * sizeof(LLVMValueRef));

  for(int i = 0; i < fn.blocks.length; i++) {
    Bytecode_Block block = fn.blocks.data[i];
    LLVMBasicBlockRef llb = LLVMAppendBasicBlock(llf, "");
    LLVMPositionBuilderAtEnd(builder, llb);

    for(int j = 0; j < block.length; j++) {
      Bytecode_Instruction inst = block.data[j];
      switch(inst.type) {
        case BC_ADD:
        case BC_SUB: {
          LLVMValueRef b = r[inst.data.basic_op.reg_b];
          LLVMValueRef c = r[inst.data.basic_op.reg_c];
          Type_Info a_type = fn.register_types.data[inst.data.basic_op.reg_a];
          Type_Info b_type = fn.register_types.data[inst.data.basic_op.reg_b];
          Type_Info c_type = fn.register_types.data[inst.data.basic_op.reg_c];
          assert(a_type.type == TYPE_INT &&
                 b_type.type == TYPE_INT &&
                 c_type.type == TYPE_INT);
          u8 a_width = a_type.data.integer.width;
          u8 b_width = b_type.data.integer.width;
          u8 c_width = c_type.data.integer.width;

          if(b_width < a_width) {
            if(b_type.data.integer.is_signed)
              b = LLVMBuildSExt(builder, b, LLVMIntType(a_width), "");
            else
              b = LLVMBuildZExt(builder, b, LLVMIntType(a_width), "");
          } else if(b_width > a_width) {
            b = LLVMBuildTrunc(builder, b, LLVMIntType(a_width), "");
          }

          if(c_width < a_width) {
            if(c_type.data.integer.is_signed)
              c = LLVMBuildSExt(builder, c, LLVMIntType(a_width), "");
            else
              c = LLVMBuildZExt(builder, c, LLVMIntType(a_width), "");
          } else if(c_width > a_width) {
            c = LLVMBuildTrunc(builder, c, LLVMIntType(a_width), "");
          }

          LLVMValueRef result;
          if(inst.type == BC_ADD) {
            result = LLVMBuildAdd(builder, b, c, "");
          } else {
            result = LLVMBuildSub(builder, b, c, "");
          }
          r[inst.data.basic_op.reg_a] = result;
          break;
        }

        case BC_SET: {
          LLVMValueRef b = r[inst.data.basic_op.reg_b];
          Type_Info a_type = fn.register_types.data[inst.data.basic_op.reg_a];
          Type_Info b_type = fn.register_types.data[inst.data.basic_op.reg_b];
          assert(a_type.type == TYPE_INT &&
                 b_type.type == TYPE_INT);
          u8 a_width = a_type.data.integer.width;
          u8 b_width = b_type.data.integer.width;

          if(b_width < a_width) {
            if(b_type.data.integer.is_signed)
              r[inst.data.basic_op.reg_a] = LLVMBuildSExt(builder, b, LLVMIntType(a_width), "");
            else
              r[inst.data.basic_op.reg_a] = LLVMBuildZExt(builder, b, LLVMIntType(a_width), "");
          } else if(b_width > a_width) {
            r[inst.data.basic_op.reg_a] = LLVMBuildTrunc(builder, b, LLVMIntType(a_width), "");
          } else {
            r[inst.data.basic_op.reg_a] = b;
          }
          break;
        }

        case BC_SET_LITERAL: {
          Type_Info type = fn.register_types.data[inst.data.set_literal.reg_a];
          assert(type.type == TYPE_INT);
          r[inst.data.set_literal.reg_a] = LLVMConstInt(LLVMIntType(type.data.integer.width), inst.data.set_literal.lit_b, 0);
          break;
        }
        case BC_RETURN: {
          LLVMBuildRet(builder, r[inst.data.ret.reg]);
          break;
        }
        case BC_BRANCH:
        case BC_COND_BRANCH:
        default: {
          printf("Encountered unknown bytecode instruction.\n");
          print_bytecode_instruction(inst);
          exit(1);
        }
      }
    }
  }

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
  printf("Compiling for target with triple: %s\n", triple);

  LLVMTargetRef target;
  is_error = LLVMGetTargetFromTriple(triple, &target, &error);
  if(is_error) print_and_exit(error);
  LLVMTargetMachineRef target_machine =	LLVMCreateTargetMachine(target, triple, "generic", "", LLVMCodeGenLevelNone, LLVMRelocDefault, LLVMCodeModelDefault);
  LLVMSetModuleDataLayout(mod, LLVMCreateTargetDataLayout(target_machine));
  LLVMSetTarget(mod, triple);


  LLVMPassManagerBuilderRef pass_manager_builder = LLVMPassManagerBuilderCreate();
  LLVMPassManagerBuilderSetOptLevel(pass_manager_builder, 0);
  LLVMPassManagerRef pass_manager = LLVMCreatePassManager();
  LLVMPassManagerBuilderPopulateModulePassManager(pass_manager_builder, pass_manager);
  LLVMPassManagerBuilderDispose(pass_manager_builder);
  LLVMRunPassManager(pass_manager, mod);
  LLVMDisposePassManager(pass_manager);

  if(out_obj) {
    is_error = LLVMTargetMachineEmitToFile(target_machine, mod, out_obj, LLVMObjectFile, &error);
    if(is_error) print_and_exit(error);
  }

  if(out_asm) {
    is_error = LLVMTargetMachineEmitToFile(target_machine, mod, out_asm, LLVMAssemblyFile, &error);
    if(is_error) print_and_exit(error);
  }

  if(out_ir) {
    LLVMWriteBitcodeToFile(mod, out_ir);
  }

  printf("Done.\n");

  LLVMDisposeTargetMachine(target_machine);
  LLVMDisposeModule(mod);
}
