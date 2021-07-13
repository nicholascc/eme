#ifndef PARSER_H
#define PARSER_H

#include "lexer.h"
#include "ast.h"

Ast_Node parse_expression(Lexer *l, u8 min_power);
Ast_Node parse_function_call(Lexer *l, Ast_Node *function, Token open_paren);
Ast_Node parse_any_statement(Lexer *l);
Ast_Node parse_type(Lexer *l);
Ast_Node parse_decl(Lexer *l);
Ast_Node parse_block(Lexer *l);
Ast parse_file(Lexer *l);
Ast_Node parse_definition(Lexer *l);

#endif /* end of include guard: PARSER_H */
