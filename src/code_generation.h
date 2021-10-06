#ifndef CODE_GENERATION_H
#define CODE_GENERATION_H

#include <stdio.h>

#include "ast.h"

void ast_to_c(Ast ast, FILE *f);

#endif
