#include "dependency_graph.h"

#include <stdbool.h>
#include <assert.h>

#include "ast.h"
#include "error.h"

void add_dependencies_from_node(Ast_Node *node, Linear_Ast_Unit_Ptr_Array *dependencies) {
}

void form_dependency_graph(Ast *ast) {
  for(int i = 0; i < ast->linear_ast_units.length; i++) {
    Linear_Ast_Unit *unit = ast->linear_ast_units.data + i;

    add_dependencies_from_node(unit->node, &(unit->dependencies));
  }
}
