#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include "ast.h"

Type solidify_type(Type x, Ast_Node node);

// DO NOT HOLD THE RETURNED POINTER! It is a pointer into a dynamic array.
Scope_Entry *get_entry_of_identifier_in_scope(symbol symbol, Scope *scope, Location error_location, Scope **found_scope);

// using_result refers to whether the result of this computation will be used for other computations. This is only necessary because if statements need to know if their results will be used by other code, since if their results are not used by other code those results do not need to be type-checked.
Type infer_types_of_block(Ast_Node *block, Compilation_Unit *compilation_unit, bool using_result, bool *unit_poisoned);
Type infer_type_of_expr(Ast_Node *n, Scope *scope, Compilation_Unit *compilation_unit, bool using_result, bool *unit_poisoned);
void type_infer_compilation_unit(Compilation_Unit *unit); // infers the types *inside* the unit
Type infer_type_of_compilation_unit(Compilation_Unit *unit); // finds the type of the compilation unit itself. Also calls type_infer_compilation_unit.

#endif
