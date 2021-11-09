#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

Ast_Node *parse_expression(Lexer *l, Scope *scope, u8 min_power, bool *needs_semicolon);
Ast_Node *parse_function_call(Lexer *l, Scope *scope, Ast_Node *function, Token open_paren);
Ast_Node *parse_any_statement(Lexer *l, Scope *scope);
Ast_Node *parse_type(Lexer *l, Scope *scope);
Ast_Node *parse_decl(Lexer *l, Scope *scope);
Ast_Node *parse_block(Lexer *l, Scope *parent_scope);
Ast_Node *parse_definition(Lexer *l, Scope *scope);
Ast *parse_file(Lexer *l);

#endif /* end of include guard: PARSER_H */
