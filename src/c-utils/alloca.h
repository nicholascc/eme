// a small utility to get alloca on any compiler.
// needs to be updated for more compilers than just msvc.

#ifndef C_UTILS_ALLOCA
#define C_UTILS_ALLOCA

#include <stdlib.h>

#ifndef alloca

#ifdef _MSC_VER
#include <malloc.h>
#define alloca _alloca
#endif

#endif

#endif
