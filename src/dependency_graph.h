#ifndef DEPENDENCY_GRAPH_H
#define DEPENDENCY_GRAPH_H

#include <stdbool.h>

#include "ast.h"

// For each Linear_Ast_Unit in the Ast, finds all of its dependencies and adds the pointers to the "dependencies" list.
// aka, populates the "dependencies" list in all Linear_Ast_Unit structs
void form_dependency_graph(Ast *ast);

#endif
