#ifndef TYPE_H
#define TYPE_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "c-utils/table.h"
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
  TYPE_UNKNOWN_STRING,

  TYPE_STRUCT,
  TYPE_POLY_INSTANCE,

  TYPE_FTYPE // The type of a type.
} Type_Type;

typedef struct Scope Scope;
typedef struct Ast_Node Ast_Node;
typedef struct Ast_Struct_Definition Ast_Struct_Definition;
typedef struct Ast_Poly_Struct_Definition Ast_Poly_Struct_Definition;
typedef struct Compilation_Unit Compilation_Unit;
GENERATE_DARRAY_HEADER(Compilation_Unit *, Compilation_Unit_Ptr_Array);

typedef struct Type_Info Type_Info;
// Type_Infos are uniquely stored in memory, so you can just compare .info pointers to tell if two Types are equal.
typedef struct Type {
  int reference_count;
  Type_Info *info;
} Type;

GENERATE_DARRAY_HEADER(Type, Type_Array);
GENERATE_TABLE_HEADER(Type, Type_Table);

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
      Compilation_Unit_Ptr_Array members;
      bool llvm_generated;
      LLVMTypeRef llvm_type;
      symbol name;
    } struct_;
    struct {
      Compilation_Unit *definition_unit;
      Ast_Poly_Struct_Definition *definition;
      Scope *scope;
      Compilation_Unit_Ptr_Array members;
      bool llvm_generated;
      LLVMTypeRef llvm_type;
    } poly_instance;
  } data;
} Type_Info;


Type UNKNOWN_TYPE;
Type NOTHING_TYPE;
Type POISON_TYPE;
Type BOOL_TYPE;
Type INT_TYPE;
Type UINT_TYPE;
// ordered unsigned first, least to greatest width (8-64).
Type INTEGER_TYPES[2][4];
Type FTYPE_TYPE;

void init_primitive_types(); // initializes the above types. MUST BE CALLED UPON STARTUP!
bool type_equals(Type a, Type b);
Type allocate_unknown_int_type(int value);
Type integer_type_with(bool is_signed, u8 width);

void print_type(Type t);
u32 size_of_type(Type t); // Also computes the size of a struct if that size is unknown.

// These hash functions rely on the uniqueness property of Type_Info pointers.
u64 hash_type_info_ptr(Type_Info *t);
u64 hash_type(Type t);

#endif
