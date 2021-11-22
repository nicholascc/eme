#ifndef AST_H
#define AST_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "errors.h"

typedef enum Type_Info_Type {
  TYPE_UNKNOWN, // type has not yet been inferred
  TYPE_NOTHING,
  TYPE_POISON, // used to indicate a type that is unknown due to a compiler
               // error. A poison type should cause no errors and simply
               // remove the possibility of false positive errors.

  TYPE_INT,
  TYPE_UNKNOWN_INT,
  TYPE_BOOL,

  TYPE_STRUCT,
  TYPE_ARRAY
} Type_Info_Type;

typedef struct Type_Info Type_Info;

typedef struct Type_Info {
  Type_Info_Type type;
  int reference_count;
  union {
    struct {u8 _;} _; // allows the data to be unset in a struct literal like {TYPE_UNKNOWN, 0, {0}}
    s64 unknown_int;
    struct {
      bool is_signed;
      u8 width; // including sign bit
    } integer;
    // let's not do structs yet.
    struct {
      Type_Info *element_type;
    } array;
  } data;
} Type_Info;

Type_Info UNKNOWN_TYPE_INFO;
Type_Info NOTHING_TYPE_INFO;
Type_Info POISON_TYPE_INFO;
Type_Info BOOL_TYPE_INFO;
#define INT_TYPE_INFO(is_signed,width) ((Type_Info){TYPE_INT,0,{.integer={is_signed, width}}})

typedef enum Ast_Node_Type {
  NODE_NULL,
  NODE_LITERAL,
  NODE_BINARY_OP,
  NODE_UNARY_OP,
  NODE_IF,
  NODE_FUNCTION_CALL,
  NODE_SYMBOL,
  NODE_BLOCK,

  NODE_PRIMITIVE_TYPE,

  NODE_TYPED_DECL,
  NODE_UNTYPED_DECL_SET,
  NODE_TYPED_DECL_SET,
  NODE_FUNCTION_ARGUMENT,
  NODE_FUNCTION_DEFINITION,
  NODE_RETURN
} Ast_Node_Type;

// AST nodes are stored as structs of many different types, defined below, which
// each have the struct Ast_Node as their first member. In Ast_Node, type
// indicates which type of struct the node is. This way, given an Ast_Node *
// you can get the full type information by casting to a specific type of
// node using this type information. For example,
//
// Ast_Node *node = ...;
// if(node->type == NODE_BINARY_OP) {
//   Ast_Binary_Op *binop = (Ast_Binary_Op *)node;
//   ...
// }

typedef struct Ast_Node {
  Ast_Node_Type type;
  Location loc;
} Ast_Node;

GENERATE_DARRAY_HEADER(Ast_Node *, Ast_Node_Ptr_Array);

typedef struct Bytecode_Function Bytecode_Function;

typedef enum Compilation_Unit_Type {
  // A function is represented by a signature and body which must be
  // type-checked separately (since for example the body of a recursive
  // function may reference it's own signature).
  UNIT_FUNCTION_SIGNATURE,
  UNIT_FUNCTION_BODY
} Compilation_Unit_Type;

typedef struct Compilation_Unit Compilation_Unit;

typedef struct Compilation_Unit {
  Compilation_Unit_Type type;
  bool type_inferred;
  bool type_inference_seen;
  bool bytecode_generated;
  bool bytecode_generation_seen;
  bool poisoned;
  Ast_Node *node;
  union {
    Compilation_Unit *signature;
    Compilation_Unit *body;
  } data;
  union {
    Bytecode_Function *function;
  } bytecode;
} Compilation_Unit;

GENERATE_DARRAY_HEADER(Compilation_Unit *, Compilation_Unit_Ptr_Array);

typedef struct Scope_Entry {
  u64 symbol;
  u32 register_id;
  union {
    Ast_Node *node;
    Compilation_Unit *unit;
  } declaration;
} Scope_Entry;

GENERATE_DARRAY_HEADER(Scope_Entry, Scope_Entry_Array);

typedef struct Scope Scope;

typedef struct Scope {
  bool is_ordered;
  bool has_parent;
  Scope *parent;
  Scope_Entry_Array entries;
} Scope;

typedef struct Ast {
  Compilation_Unit_Ptr_Array compilation_units;
  Scope scope;
} Ast;





typedef struct Ast_Literal {
  Ast_Node n;
  Type_Info type;
  s64 value;
} Ast_Literal;

typedef enum Ast_Binary_Op_Type {
  OPPLUS,
  OPMINUS,
  OPMUL,
  OPDIV,
  OPMOD,

  OPSET_EQUALS,
  OPPLUS_EQUALS,
  OPMINUS_EQUALS,

  OPLESS_THAN,
  OPLESS_THAN_OR_EQUAL_TO,
  OPGREATER_THAN,
  OPGREATER_THAN_OR_EQUAL_TO,

  OPSTRUCT_MEMBER,
  OPSUBSCRIPT,

  OPTEST_EQUALS
} Ast_Binary_Op_Type;

typedef struct Ast_Binary_Op {
  Ast_Node n;
  Ast_Binary_Op_Type operator;
  Ast_Node *first;
  Ast_Node *second;
  Type_Info convert_to;
} Ast_Binary_Op;

typedef enum Ast_Unary_Op_Type {
  OPNEGATE,
  OPREFERENCE,
  OPDEREFERENCE,
  OPPLUS_PLUS_FIRST,
  OPPLUS_PLUS_SECOND,
  OPMINUS_MINUS_FIRST,
  OPMINUS_MINUS_SECOND
} Ast_Unary_Op_Type;

typedef struct Ast_Unary_Op {
  Ast_Node n;
  Ast_Unary_Op_Type operator;
  Ast_Node *operand;
  Type_Info type;
} Ast_Unary_Op;

typedef struct Ast_If {
  Ast_Node n;
  bool result_is_used;
  Type_Info result_type_info; // only set if result_is_used is true
  Ast_Node *cond;
  Ast_Node *first;
  Ast_Node *second;
} Ast_If;

typedef struct Ast_Function_Call {
  Ast_Node n;
  Ast_Node *identifier;
  Ast_Node_Ptr_Array arguments;
} Ast_Function_Call;

typedef struct Ast_Primitive_Type {
  Ast_Node n;
  Type_Info type_info;
} Ast_Primitive_Type;

typedef struct Ast_Symbol {
  Ast_Node n;
  u64 symbol;
} Ast_Symbol;

typedef struct Ast_Typed_Decl {
  Ast_Node n;
  u64 symbol;
  Ast_Node *type;
  Type_Info type_info;
} Ast_Typed_Decl;

typedef struct Ast_Typed_Decl_Set {
  Ast_Node n;
  u64 symbol;
  Ast_Node *type;
  Ast_Node *value;
  Type_Info type_info;
} Ast_Typed_Decl_Set;

typedef struct Ast_Untyped_Decl_Set {
  Ast_Node n;
  u64 symbol;
  Ast_Node *value;
  Type_Info type_info;
} Ast_Untyped_Decl_Set;

typedef struct Ast_Block {
  Ast_Node n;
  Scope scope;
  Ast_Node_Ptr_Array statements;
} Ast_Block;

typedef struct Ast_Function_Argument {
  Ast_Node n;
  u64 symbol;
  Ast_Node *type;
  Type_Info type_info;
} Ast_Function_Argument;

typedef struct Ast_Function_Definition {
  Ast_Node n;
  u64 symbol;
  Ast_Node_Ptr_Array arguments;
  Ast_Node *return_type;
  Type_Info return_type_info;
  Scope scope; // the scope entries for this function's arguments must be placed first and in order in this scope.
  Ast_Node *body;
} Ast_Function_Definition;

typedef struct Ast_Return {
  Ast_Node n;
  Ast_Node *value;
} Ast_Return;

Ast_Node *allocate_ast_node(Ast_Node_Type type, u32 size);
Compilation_Unit *allocate_null_compilation_unit();
Compilation_Unit *allocate_compilation_unit(Compilation_Unit unit);

void print_symbol(u64 symbol);
void print_ast(Ast ast);
void print_scope(Scope s);
void print_ast_statement_array(Ast_Node_Ptr_Array nodes);
void print_ast_node(Ast_Node *node);
void print_type_info(Type_Info t);
void print_compilation_unit(Compilation_Unit *unit);

#endif /* end of include guard: AST_H */
