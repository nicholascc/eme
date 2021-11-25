#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

Ast_Node *parse_expression(Token_Reader *r, Scope *scope, u8 min_power, bool *needs_semicolon);
Ast_Node *parse_function_call(Token_Reader *r, Scope *scope, Ast_Node *function, Token open_paren);
Ast_Node *parse_any_statement(Token_Reader *r, Scope *scope, bool require_semicolon_for_expressions);
Ast_Node *parse_type(Token_Reader *r, Scope *scope);
Ast_Node *parse_decl(Token_Reader *r, Scope *scope);
Ast_Node *parse_block(Token_Reader *r, Scope *parent_scope);
Ast_Node *parse_definition(Token_Reader *r, Scope *scope);
Ast *parse_file(Token_Reader *r);

// Call this before any parsing occurs; this caches the symbols corresponding to e.g. u8, int, etc.
void register_parser_symbols();

#endif /* end of include guard: PARSER_H */
