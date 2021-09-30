#ifndef TYPE_INFERENCE_H
#define TYPE_INFERENCE_H

#include "ast.h"

Type_Info infer_types_of_block(Ast_Node *block);
Type_Info infer_type_of_expr(Ast_Node *n, Scope *scope);
void infer_types_of_ast(Ast *ast);

#endif
