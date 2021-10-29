#ifndef INTERPRETER_H
#define INTERPRETER_H

#include "c-utils/integer.h"
#include "bytecode.h"

void interpret_bytecode_function(Bytecode_Function fn);

#endif
