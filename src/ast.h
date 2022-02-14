#ifndef AST_H
#define AST_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "errors.h"
#include "symbol_table.h"
#include "type.h"

typedef enum Ast_Node_Type {
  NODE_NULL,
  NODE_LITERAL,
  NODE_BINARY_OP,
  NODE_UNARY_OP,
  NODE_IF,
  NODE_WHILE,
  NODE_FUNCTION_CALL,
  NODE_SYMBOL,
  NODE_BLOCK,

  NODE_PRIMITIVE_TYPE,

  NODE_TYPED_DECL,
  NODE_UNTYPED_DECL_SET,
  NODE_TYPED_DECL_SET,
  NODE_FUNCTION_PARAMETER,
  NODE_FUNCTION_DEFINITION,
  NODE_STRUCT_DEFINITION,
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

// You should only use this if you don't know anything about the Ast node, not even
// its file.
Ast_Node NULL_AST_NODE;

GENERATE_DARRAY_HEADER(Ast_Node *, Ast_Node_Ptr_Array);

typedef struct Bytecode_Function Bytecode_Function;

typedef enum Compilation_Unit_Type {
  // A function is represented by a signature and body which must be
  // type-checked separately (since for example the body of a recursive
  // function may reference it's own signature).
  UNIT_FUNCTION_SIGNATURE,
  UNIT_FUNCTION_BODY,
  UNIT_STRUCT,
  UNIT_STRUCT_MEMBER
} Compilation_Unit_Type;

typedef struct Compilation_Unit Compilation_Unit;
typedef struct Scope Scope;

typedef struct Compilation_Unit {
  Compilation_Unit_Type type;
  bool type_inferred;
  bool type_inference_seen; // used to detect circular dependencies
  bool bytecode_generated; // meaningless for structs and struct members
  bool bytecode_generating; // used to not fall into an infinite recursion while
                            // generating bytecode for recursive functions.
  bool poisoned;

  Ast_Node *node;
  union {
    struct {
      Compilation_Unit *body;
      Scope *scope;
    } signature;
    struct {
      Bytecode_Function *bytecode;
      Compilation_Unit *signature;
      Scope *scope;
    } body;
    struct {
      Type type;
      Scope *scope;
    } struct_def;
    struct {
      Type type;
      Scope *scope;
    } struct_member;
  } data;
} Compilation_Unit;

GENERATE_DARRAY_HEADER(Compilation_Unit *, Compilation_Unit_Ptr_Array);

typedef struct Scope_Entry {
  symbol symbol;
  u32 register_id; // Only relevant in block scopes
  union { // Determined based on the parent Scope object's type property
    Ast_Node *node;
    Compilation_Unit *unit;
  } declaration;
} Scope_Entry;

GENERATE_DARRAY_HEADER(Scope_Entry, Scope_Entry_Array);

typedef enum Scope_Type {
  BLOCK_SCOPE,
  FILE_SCOPE
} Scope_Type;

typedef struct Scope {
  Scope_Type type;
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
  Type type;
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

  OPEQUALS,
  OPLESS_THAN,
  OPLESS_THAN_OR_EQUAL_TO,
  OPGREATER_THAN,
  OPGREATER_THAN_OR_EQUAL_TO,

  OPSTRUCT_MEMBER,
  OPSTRUCT_MEMBER_REF,
  OPSUBSCRIPT,

  OPTEST_EQUALS
} Ast_Binary_Op_Type;

typedef struct Ast_Binary_Op {
  Ast_Node n;
  Ast_Binary_Op_Type operator;
  Ast_Node *first;
  Ast_Node *second;
  Type convert_to;
} Ast_Binary_Op;

typedef enum Ast_Unary_Op_Type {
  OPNEGATE,
  OPPLUS_PLUS_FIRST,
  OPPLUS_PLUS_SECOND,
  OPMINUS_MINUS_FIRST,
  OPMINUS_MINUS_SECOND,
  OPREFERENCE,
  OPDEREFERENCE,

  OPREF_TYPE
} Ast_Unary_Op_Type;

typedef struct Ast_Unary_Op {
  Ast_Node n;
  Ast_Unary_Op_Type operator;
  Ast_Node *operand;
} Ast_Unary_Op;

typedef struct Ast_If {
  Ast_Node n;
  bool result_is_used;
  Type result_type; // only set if result_is_used is true
  Ast_Node *cond;
  Ast_Node *first;
  Ast_Node *second;
} Ast_If;

typedef struct Ast_While {
  Ast_Node n;
  Ast_Node *cond;
  Ast_Node *body;
} Ast_While;

typedef struct Ast_Function_Call {
  Ast_Node n;
  Ast_Node *identifier;
  Compilation_Unit *signature;
  Ast_Node_Ptr_Array arguments;
} Ast_Function_Call;

typedef struct Ast_Primitive_Type {
  Ast_Node n;
  Type type;
} Ast_Primitive_Type;

typedef struct Ast_Symbol {
  Ast_Node n;
  symbol symbol;
} Ast_Symbol;

typedef struct Ast_Typed_Decl {
  Ast_Node n;
  symbol symbol;
  Ast_Node *type_node;
  Type type;
} Ast_Typed_Decl;

typedef struct Ast_Typed_Decl_Set {
  Ast_Node n;
  symbol symbol;
  Ast_Node *type_node;
  Ast_Node *value;
  Type type;
} Ast_Typed_Decl_Set;

typedef struct Ast_Untyped_Decl_Set {
  Ast_Node n;
  symbol symbol;
  Ast_Node *value;
  Type type;
} Ast_Untyped_Decl_Set;

typedef struct Ast_Block {
  Ast_Node n;
  Scope scope;
  Ast_Node_Ptr_Array statements;
} Ast_Block;

typedef struct Ast_Function_Parameter {
  Ast_Node n;
  symbol symbol;
  Ast_Node *type_node;
  Type type;
} Ast_Function_Parameter;

typedef struct Ast_Function_Definition {
  Ast_Node n;
  symbol symbol;
  Ast_Node_Ptr_Array parameters;
  Ast_Node *return_type_node;
  Type return_type;
  Scope scope; // the scope entries for this function's arguments must be placed first and in order in this scope.
  Ast_Node *body;
} Ast_Function_Definition;

typedef struct Ast_Struct_Definition {
  Ast_Node n;
  symbol symbol;
  Type type;
  Compilation_Unit_Ptr_Array members;
} Ast_Struct_Definition;

typedef struct Ast_Return {
  Ast_Node n;
  Ast_Node *value;
} Ast_Return;

Ast_Node *allocate_ast_node(Ast_Node_Type type, u32 size);
Compilation_Unit *allocate_null_compilation_unit();
Compilation_Unit *allocate_compilation_unit(Compilation_Unit unit);

void print_symbol(symbol symbol);
void print_ast(Ast ast);
void print_ast_statement_array(Ast_Node_Ptr_Array nodes);
void print_ast_node(Ast_Node *node);
void print_compilation_unit(Compilation_Unit *unit);

#endif /* end of include guard: AST_H */
