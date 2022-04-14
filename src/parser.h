#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

Ast_Node *parse_expression(Token_Reader *r, Scope *scope, u8 min_power, bool *needs_semicolon, bool *uses_bind_symbol);
Ast_Node *parse_function_call(Token_Reader *r, Scope *scope, Ast_Node *function, Token open_paren, bool *uses_bind_symbol);
Ast_Node *parse_any_statement(Token_Reader *r, Scope *scope, bool require_semicolon_for_expressions);
Ast_Node *parse_decl(Token_Reader *r, Scope *scope);
Ast_Node *parse_block(Token_Reader *r, Scope *parent_scope);
Ast_Node *parse_definition(Token_Reader *r, Scope *scope);
Compilation_Unit *parse_file(int file_id); // Returns a UNIT_MODULE. If the file is
                                           // already parsed, the same unit is returned.

// Call this before any parsing occurs; this caches the symbols corresponding to e.g. u8, int, etc.
// Must be called after INTEGER_TYPES is set.
void register_parser_symbols();

#endif /* end of include guard: PARSER_H */
