#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "c-utils/integer.h"
#include "bytecode.h"


// params should be an array containing a pointer to each parameter.
// Each parameter will be copied into the local scope of the function,
// and then the parameter list will be freed (by the callee).
// The result is a pointer to the function's return value. The caller
// has ownership of this pointer and should free it ASAP.
u8 *interpret_bytecode_unit(Bytecode_Unit *unit, u8 **params);

#endif
