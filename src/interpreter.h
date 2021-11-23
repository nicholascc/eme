#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "c-utils/integer.h"
#include "bytecode.h"

// The returned environment contains the values for all registers.
// A caller should call this function and then pass in the arguments as the first
// n registers.
u64 *init_interpreted_function_environment(Bytecode_Function fn);
void interpret_bytecode_function(Bytecode_Function fn, u64 *environment);

#endif
