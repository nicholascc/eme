#ifndef AST_H
#define AST_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"

typedef enum Type_Info_Type {
  TYPE_UNKNOWN, // type has not yet been inferred
  TYPE_NOTHING,

  TYPE_INT,
  TYPE_UNKNOWN_INT,

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

typedef enum Ast_Node_Type {
  NODE_NULL,
  NODE_LITERAL,
  NODE_BINARY_OP,
  NODE_UNARY_OP,
  NODE_TERNARY_IF,
  NODE_FUNCTION_CALL,
  NODE_SYMBOL,
  NODE_BLOCK,

  NODE_PRIMITIVE_TYPE,

  NODE_TYPED_DECL,
  NODE_UNTYPED_DECL_SET,
  NODE_TYPED_DECL_SET,
  NODE_FUNCTION_DEFINITION,
  NODE_RETURN
} Ast_Node_Type;

typedef enum Ast_Binary_Operator {
  OPPLUS,
  OPMINUS,
  OPMUL,
  OPDIV,
  OPMOD,

  OPSET_EQUALS,
  OPPLUS_EQUALS,
  OPMINUS_EQUALS,

  OPSTRUCT_MEMBER,
  OPSUBSCRIPT,

  OPTEST_EQUALS
} Ast_Binary_Operator;

typedef enum Ast_Unary_Operator {
  OPNEGATE,
  OPREFERENCE,
  OPDEREFERENCE,
  OPPLUS_PLUS_FIRST,
  OPPLUS_PLUS_SECOND,
  OPMINUS_MINUS_FIRST,
  OPMINUS_MINUS_SECOND
} Ast_Unary_Operator;

typedef struct Ast_Node Ast_Node;
GENERATE_DARRAY_HEADER(Ast_Node *, Ast_Node_Ptr_Array);

typedef struct Compilation_Unit {
  bool type_inferred;
  bool seen_in_type_inference;
  Ast_Node *node;
} Compilation_Unit;

GENERATE_DARRAY_HEADER(Compilation_Unit *, Compilation_Unit_Ptr_Array);

typedef struct Scope_Entry {
  u64 symbol;
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


typedef struct Ast_Node {
  Ast_Node_Type type;
  int line;
  int character;
  int file_id;

  union {
    struct {
      Type_Info type;
      int value;
    } literal;

    struct {
      Ast_Node *first;
      Ast_Node *second;
      Ast_Binary_Operator op;
    } binary_op;

    struct {
      Ast_Node *operand;
      Ast_Unary_Operator operator;
    } unary_op;

    struct {
      Ast_Node *cond;
      Ast_Node *first;
      Ast_Node *second;
    } ternary_if;

    struct {
      Ast_Node *identifier;
      Ast_Node_Ptr_Array arguments;
    } function_call;

    Type_Info primitive_type;

    u64 symbol;

    struct {
      u64 symbol;
      Ast_Node *value;
      Ast_Node *type;
      Type_Info type_info;
    } decl; // is the representation of NODE_TYPED_DECL, NODE_UNTYPED_DECL_SET, or NODE_TYPED_DECL_SET
            // these determine which fields here are set or unset

    struct {
      Scope scope;
      Ast_Node_Ptr_Array statements;
    } block;

    struct {
      u64 symbol;
      Ast_Node *return_type;
      Ast_Node *body;
    } function_definition;

    struct {
      Ast_Node *value;
    } _return;
  } data;
} Ast_Node;

Ast_Node *allocate_null_ast_node();
Ast_Node *allocate_ast_node(Ast_Node node);
Compilation_Unit *allocate_null_compilation_unit();
Compilation_Unit *allocate_compilation_unit(Compilation_Unit unit);

void print_symbol(u64 symbol);
void print_ast(Ast ast);
void print_scope(Scope s);
void print_ast_statement_array(Ast_Node_Ptr_Array nodes);
void print_ast_node(Ast_Node node);
void print_type_info(Type_Info t);

void error_at_ast_node(char *message, Ast_Node node);

#endif /* end of include guard: AST_H */
