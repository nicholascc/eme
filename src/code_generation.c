#include "code_generation.h"

#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "error.h"
#include "symbol_table.h"


const char *file_prelude =
" \
#include <stdio.h> \n\
#include <stdint.h> \n\
\n\
typedef uint8_t  u8; \n\
typedef uint16_t u16; \n\
typedef uint32_t u32; \n\
typedef uint64_t u64; \n\
\n\
typedef int8_t  s8; \n\
typedef int16_t s16; \n\
typedef int32_t s32; \n\
typedef int64_t s64; \n\
\n\
";

void ast_to_c(Ast ast, FILE *f) {
  fputs(file_prelude, f);
}
