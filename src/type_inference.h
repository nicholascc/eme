#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include "ast.h"

Type_Info solidify_type(Type_Info x, Ast_Node node);

// DO NOT HOLD THE RETURNED POINTER! It is a pointer into a dynamic array.
Scope_Entry *get_entry_of_identifier_in_scope(u64 symbol, Scope *scope, Ast_Node *node);

// using_result refers to whether the result of this computation will be used for other computations. This is only necessary because if statements need to know if their results will be used by other code, since if their results are not used by other code they do not need to be type-checked.
Type_Info infer_types_of_block(Ast_Node *block, bool using_result);
Type_Info infer_type_of_expr(Ast_Node *n, Scope *scope, bool using_result);
void infer_types_of_compilation_unit(Compilation_Unit *unit, Scope *scope);

#endif
