#ifndef BYTECODE_H
#define BYTECODE_H

#include "c-utils/integer.h"
#include "c-utils/darray.h"

#include "ast.h"
#include <llvm-c/Core.h>

typedef enum Bytecode_Instruction_Type {
  BC_ADD,
  BC_SUB,
  BC_MUL,
  BC_DIV,

  BC_EQUALS,
  BC_LESS_THAN,

  BC_SET,
  BC_SET_LITERAL,

  BC_REF_TO, // reference to the register

  BC_GET_MEMBER_PTR, // behaves similar to llvm getelementptr, taking a pointer
                     // to a struct and a memmber id in that struct, and returning
                     // a pointer to the memmber.
  BC_LOAD,
  BC_STORE,

  BC_CALL,
  BC_ARG,

  BC_RETURN,
  BC_BRANCH,
  BC_COND_BRANCH
} Bytecode_Instruction_Type;

typedef struct Bytecode_Instruction {
  Bytecode_Instruction_Type type;
  union {
    struct {
      u32 reg_a;
      u32 reg_b;
    } set;

    struct {
      u32 reg_a;
      s64 lit_b;
    } set_literal;

    struct {
      u32 reg_a;
      u32 reg_b;
      u32 reg_c;
    } bin_op;

    struct {
      u32 reg_a;
      u32 reg_b;
    } unary_op;

    struct {
      u32 reg_a;
      u32 reg_b;
      u32 member;
    } get_member_ptr;

    struct {
      Bytecode_Function *to;
      u32 reg;
    } call;

    struct {
      u32 reg;
    } arg;

    struct {
      u32 reg;
    } ret;

    struct {
      u32 block;
    } branch;

    struct {
      u32 reg_cond;
      u32 block_true;
      u32 block_false;
    } cond_branch;
  } data;
} Bytecode_Instruction;

GENERATE_DARRAY_HEADER(Bytecode_Instruction, Bytecode_Instruction_Array);

typedef struct Bytecode_Block {
  Bytecode_Instruction_Array instructions;
  bool is_concluded; // specifies whether the block is finished, i.e. it ends with a branch
} Bytecode_Block;

GENERATE_DARRAY_HEADER(Bytecode_Block, Bytecode_Block_Array);

typedef struct Bytecode_Function {
  // For a function with n arguments, the arguments are passed in through the first n registers.
  u32 param_count;
  u32 entry_block;
  Type_Array register_types;
  Bytecode_Block_Array blocks;
  Type return_type;
  LLVMValueRef llvm_function;
} Bytecode_Function;

GENERATE_DARRAY_HEADER(Bytecode_Function, Bytecode_Function_Array);

// Refers to the translation of an ast block to bytecode, where entry is the
// generated entry block and exit is the generated exit block.
// Entry can equal exit.
typedef struct Bytecode_Ast_Block {
  u32 entry;
  u32 exit;
  u32 result_reg;
} Bytecode_Ast_Block;

void print_bytecode_instruction(Bytecode_Instruction inst);
void print_bytecode_block(Bytecode_Block block);
void print_bytecode_function(Bytecode_Function fn);
void print_bytecode_compilation_unit(Compilation_Unit *unit);
void generate_bytecode_compilation_unit(Compilation_Unit *unit); // can poison the compilation unit, e.g. if the body of a function it refers to is poisoned.


#endif
