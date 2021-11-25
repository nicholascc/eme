#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include "ast.h"

Type_Info solidify_type(Type_Info x, Ast_Node node);

// DO NOT HOLD THE RETURNED POINTER! It is a pointer into a dynamic array.
Scope_Entry *get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Ast_Node *node, bool *scope_is_ordered);

// using_result refers to whether the result of this computation will be used for other computations. This is only necessary because if statements need to know if their results will be used by other code, since if their results are not used by other code those results do not need to be type-checked.
Type_Info infer_types_of_block(Ast_Node *block, Ast_Function_Definition *function_definition, bool using_result, bool *unit_poisoned);
Type_Info infer_type_of_expr(Ast_Node *n, Scope *scope, Ast_Function_Definition *function_definition, bool using_result, bool *unit_poisoned);
void infer_types_of_compilation_unit(Compilation_Unit *unit);

#endif
