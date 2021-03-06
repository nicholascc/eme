#ifndef LLVM_GENERATION_H
#define LLVM_GENERATION_H

#include "bytecode.h"
#include "ast.h"

void llvm_generate_module(Bytecode_Unit_Ptr_Array units, char *out_obj, char *out_asm, char *out_ir);

#endif
