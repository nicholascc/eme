#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "symbol_table.h"

#include <llvm-c/Core.h>

typedef enum Type_Type {
  TYPE_UNKNOWN, // type has not yet been inferred
  TYPE_NOTHING,
  TYPE_POISON, // used to indicate a type that is unknown due to a compiler
               // error. A poison type should cause no errors and simply
               // remove the possibility of false positive errors.

  TYPE_INT,
  TYPE_UNKNOWN_INT,
  TYPE_BOOL,

  TYPE_STRUCT
} Type_Type;

typedef struct Ast_Node Ast_Node;
typedef struct Ast_Struct_Definition Ast_Struct_Definition;
typedef struct Compilation_Unit Compilation_Unit;

typedef struct Struct_Member {
  symbol symbol;
  s32 offset; // offset in struct in bytes. Set to -1 before the struct is sized.
  Compilation_Unit *unit;
} Struct_Member;

GENERATE_DARRAY_HEADER(Struct_Member, Struct_Member_Array);

typedef struct Type_Info {
  Type_Type type;
  bool sized; // Structs and similar are not sized as soon as they are created;
              // instead they are sized later so as to correctly detect dependency
              // loops. A struct containing itself is bad, but a struct containing
              // a pointer to itself is fine. Because of this relatively subtle
              // distinction, it's better to detect these dependencies later.
  bool sizing_seen; // To detect circular dependencies in sizing.
  u32 size; // Size of type in bytes. Whether this is known is determined by sized.
            // To actually get this info, use the size_of_type(Type) function.
  union {
    struct {u8 _;} _; // allows the data to be unset in a struct literal like {TYPE_UNKNOWN, 0, {0}}
    s64 unknown_int;
    struct {
      bool is_signed;
      u8 width; // including sign bit
    } integer;
    struct {
      Ast_Struct_Definition *definition;
      Struct_Member_Array members;
      bool llvm_generated;
      LLVMTypeRef llvm_type;
      symbol name;
    } struct_;
  } data;
} Type_Info;

// Type_Infos are uniquely stored in memory, so you can just compare .info pointers to tell if two Types are equal.
typedef struct Type {
  int reference_count;
  Type_Info *info;
} Type;

Type UNKNOWN_TYPE;
Type NOTHING_TYPE;
Type UNKNOWN_INT_TYPE;
Type POISON_TYPE;
Type BOOL_TYPE;
Type INT_TYPE;
Type UINT_TYPE;
// ordered unsigned first, least to greatest width (8-64).
Type INTEGER_TYPES[2][4];

void init_primitive_types(); // initializes the above types. MUST BE CALLED UPON STARTUP!
bool type_equals(Type a, Type b);
Type allocate_unknown_int_type(int value);
Type integer_type_with(bool is_signed, u8 width);

void print_type(Type t);
u32 size_of_type(Type t); // Also computes the size of a struct if that size is unknown.

#endif
