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
  BC_LESS_THAN_EQUALS,

  BC_SET,
  BC_SET_LITERAL,

  BC_BIT_CAST,

  BC_REF_TO, // reference to the register

  BC_GET_MEMBER_PTR, // behaves similar to llvm getelementptr, taking a pointer
                     // to a struct and a memmber id in that struct, and returning
                     // a pointer to the memmber.
  BC_LOAD,
  BC_STORE,

  BC_CALL,
  BC_ARG,

  BC_RETURN,
  BC_RETURN_NOTHING,
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
    } bit_cast;

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
      Bytecode_Unit *to;
      u32 reg;
      bool keep_return_value;
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


typedef enum Bytecode_Unit_Type {
  BYTECODE_FUNCTION,
  BYTECODE_FOREIGN_FUNCTION
} Bytecode_Unit_Type;

// Bytecode_Units are stored in the same way as Ast_Nodes, where the Bytecode_Unit
// struct is always a header which tells which specific unit type is to follow.

typedef struct Bytecode_Unit {
  Bytecode_Unit_Type type;
  symbol name;
} Bytecode_Unit;

typedef struct Bytecode_Function {
  Bytecode_Unit u;
  // For a function with n arguments, the arguments are passed in through the first n registers.
  u32 passed_param_count;
  u32 entry_block;
  Type_Array register_types;
  Bytecode_Block_Array blocks;
  Type return_type;
  bool is_inline;
  LLVMValueRef llvm_function;
} Bytecode_Function;

typedef struct Bytecode_Foreign_Function {
  Bytecode_Unit u;
  Type_Array parameter_types;
  Type return_type;
  LLVMValueRef llvm_function;
} Bytecode_Foreign_Function;

GENERATE_DARRAY_HEADER(Bytecode_Unit *, Bytecode_Unit_Ptr_Array);
Bytecode_Unit_Ptr_Array bytecode_units; // should be initialized as empty in main

Bytecode_Unit *allocate_bytecode_unit_type(Bytecode_Unit_Type type, u32 size);

void print_bytecode_instruction(Bytecode_Instruction inst);
void print_bytecode_block(Bytecode_Block block);
void print_bytecode_function(Bytecode_Function fn);
void print_bytecode_foreign_function(Bytecode_Foreign_Function fn);
void print_bytecode_unit(Bytecode_Unit *unit);
void print_bytecode_compilation_unit(Compilation_Unit *unit);
void generate_bytecode_compilation_unit(Compilation_Unit *unit); // can poison the compilation unit, e.g. if the body of a function it refers to is poisoned.


#endif
