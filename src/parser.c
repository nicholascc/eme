#include "parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lexer.h"

Ast_Node *allocate_ast_node(Ast_Node node) {
  Ast_Node *result = malloc(sizeof(Ast_Node));
  *result = node;
  return result;
}

Type_Info unknown_int_type_info() {
  Type_Info i;
  i.type = TYPE_UNKNOWN_INT;
  return i;
}


void error_token(char *message, Token t) {
  print_error_message(message, t.file_id, t.line, t.character);
  exit(1);
}

void error_unexpected_token(Token t) {
  error_token("Unexpected token.", t);
}

bool is_non_unary_operator(Token t) {
  switch(t.type) {
    case TPLUS:
    case TMINUS:
    case TASTERISK:
    case TFORWARD_SLASH:
    case TMODULUS:
    case TEQUALS:
    case TPLUS_EQUALS:
    case TMINUS_EQUALS:
    case TDOT:
    case TQUESTION_MARK:
      return true;
    default: return false;
  }
}

bool is_prefix_operator(Token t) {
  switch(t.type) {
    case TMINUS:
    case TASTERISK:
    case TAMPERSAND:
    case TDOUBLE_PLUS:
    case TDOUBLE_MINUS:
      return true;
    default:
      return false;
  }
}

bool is_postfix_operator(Token t) {
  return t.type == TDOUBLE_PLUS || t.type == TDOUBLE_MINUS;
}

Ast_Node literal_int_to_ast(Token t) {
  Ast_Node node;
  node.type = NODE_LITERAL;
  node.line = t.line;
  node.character = t.character;
  node.file_id = t.file_id;
  node.data.literal.type = unknown_int_type_info();
  node.data.literal.value = t.data.literal_int;
  return node;
}

Ast_Node symbol_to_ast(Token t) {
  Ast_Node node;
  node.type = NODE_SYMBOL;
  node.line = t.line;
  node.character = t.character;
  node.file_id = t.file_id;
  node.data.symbol = t.data.symbol;
  return node;
}

Ast_Node binary_op_to_ast(Ast_Node *first, Token op, Ast_Node *second) {
  Ast_Node node;
  node.file_id = op.file_id;
  node.line = op.line;
  node.character = op.character;

  node.type = NODE_BINARY_OP;

  Ast_Binary_Operator ast_op;
  switch(op.type) {
    case TPLUS: ast_op = OPPLUS; break;
    case TMINUS: ast_op = OPMINUS; break;
    case TASTERISK: ast_op = OPMUL; break;
    case TFORWARD_SLASH: ast_op = OPDIV; break;
    case TMODULUS: ast_op = OPMOD; break;
    case TEQUALS: ast_op = OPSET_EQUALS; break;
    case TPLUS_EQUALS: ast_op = OPPLUS_EQUALS; break;
    case TMINUS_EQUALS: ast_op = OPMINUS_EQUALS; break;
    case TDOT: ast_op = OPSTRUCT_MEMBER; break;
    case TOPEN_BRACKET: ast_op = OPSUBSCRIPT; break;
  }

  node.data.binary_op.first = first;
  node.data.binary_op.second = second;
  node.data.binary_op.op = ast_op;

  return node;
}

Ast_Node unary_op_to_ast(Token operator, Ast_Node *operand, bool prefix) {
  Ast_Node node;
  node.file_id = operator.file_id;
  node.line = operator.line;
  node.character = operator.character;

  node.type = NODE_UNARY_OP;

  Ast_Unary_Operator ast_op;
  switch(operator.type) {
    case TMINUS: ast_op = OPNEGATE; break;
    case TASTERISK: ast_op = OPDEREFERENCE; break;
    case TAMPERSAND: ast_op = OPREFERENCE; break;
    case TDOUBLE_PLUS:
      if(prefix) ast_op = OPPLUS_PLUS_FIRST;
      else ast_op = OPPLUS_PLUS_SECOND;
      break;
    case TDOUBLE_MINUS:
      if(prefix) ast_op = OPMINUS_MINUS_FIRST;
      else ast_op = OPMINUS_MINUS_SECOND;
      break;
  }

  node.data.unary_op.operator = ast_op;
  node.data.unary_op.operand = operand;

  return node;
}

Ast_Node ternary_if_to_ast(Ast_Node *cond, Token op, Ast_Node *first, Ast_Node *second) {
  assert(op.type == TQUESTION_MARK && "(internal compiler error) failed to convert ternary if to AST");
  Ast_Node node;
  node.file_id = op.file_id;
  node.line = op.line;
  node.character = op.character;

  node.type = NODE_TERNARY_IF;

  node.data.ternary_if.cond = cond;
  node.data.ternary_if.first = first;
  node.data.ternary_if.second = second;

  return node;
}

// Expressions are parsed using Pratt parsing to handle operator precedence. See https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

/*
Precedence heirarchy:

Right associative:
02: = += -=                       [✓]
04: ?: (ternary conditional)      [✓]

Left associative:
06: ||                            [ ]
08: ^^                            [ ]
10: &&                            [ ]
12: == !==                        [ ]
14: < <= > >=                     [ ]
16: |                             [ ]
18: ^                             [ ]
20: &                             [ ]
22: << >>                         [ ]
24: + -                           [✓]
26: * /                           [✓]
    %                             [✓]

Associativity doesn't matter, unary prefix:
40: everything prefix:
    ++ --                         [✓]
    -                             [✓]
    ! ~                           [ ]
    * &                           [✓]

Left associative:
50: everything postfix:
    ++ --                         [✓]
    function_call()               [✓]
    [] (subscript)                [✓]
    .                             [✓]
*/

typedef struct u8_pair {
  u8 left;
  u8 right;
} u8_pair;

u8_pair binary_op_binding_power(Token_Type type) {
  switch(type) {
    case TEQUALS:
    case TPLUS_EQUALS:
    case TMINUS_EQUALS:
      return (u8_pair){2,1};

    case TQUESTION_MARK:
      return (u8_pair){4,3};

    case TPLUS:
    case TMINUS:
      return (u8_pair){23,24};
    case TFORWARD_SLASH:
    case TASTERISK:
    case TMODULUS:
      return (u8_pair){25,26};

    case TDOT:
      return (u8_pair){49,50};
    default: assert(false && "(internal compiler error) binary op does not exist");
  }
}

u8 prefix_op_binding_power(Token_Type type) {
  switch(type) {
    case TMINUS:
    case TASTERISK:
    case TAMPERSAND:
    case TDOUBLE_PLUS:
    case TDOUBLE_MINUS:
      return 40;
    default: assert(false && "(internal compiler error) prefix op does not exist");
  }
}

Ast_Node parse_expression(Lexer *l, u8 min_power) {
  Token lhs = peek_token(l);
  Ast_Node lhs_ast;

  if(lhs.type == TLITERAL_INT) {
    lhs_ast = literal_int_to_ast(lhs);

  } else if(lhs.type == TSYMBOL) {
    lhs_ast = symbol_to_ast(lhs);

  } else if(is_prefix_operator(lhs)) {
    u8 power = prefix_op_binding_power(lhs.type);
    Ast_Node *operand_ast = allocate_ast_node(parse_expression(l, power));
    lhs_ast = unary_op_to_ast(lhs, operand_ast, true);

  } else if(lhs.type == TOPEN_PAREN) {
    lhs_ast = parse_expression(l, 0);

    Token next = peek_token(l);
    if(next.type != TCLOSE_PAREN)
      error_unexpected_token(next);

  } else error_unexpected_token(lhs);



  while(1) {
    save_state(l);
    Token op = peek_token(l);

    if(is_non_unary_operator(op)) {
      u8_pair powers = binary_op_binding_power(op.type);
      if(powers.left < min_power) {
        revert_state(l);
        break;
      }

      if(op.type == TQUESTION_MARK) {
        Ast_Node *mhs_ast = allocate_ast_node(parse_expression(l, 0));
        Token next = peek_token(l);
        if(next.type != TCOLON)
          error_unexpected_token(next);
        Ast_Node *rhs_ast = allocate_ast_node(parse_expression(l, powers.right));
        lhs_ast = ternary_if_to_ast(allocate_ast_node(lhs_ast), op, mhs_ast, rhs_ast);
      } else {
        Ast_Node *rhs_ast = allocate_ast_node(parse_expression(l, powers.right));
        lhs_ast = binary_op_to_ast(allocate_ast_node(lhs_ast), op, rhs_ast);
      }

    } else if(is_postfix_operator(op)) {
      lhs_ast = unary_op_to_ast(op, allocate_ast_node(lhs_ast), false);

    } else if(op.type == TOPEN_BRACKET) {
      Ast_Node *rhs_ast = allocate_ast_node(parse_expression(l, 0));

      Token next = peek_token(l);
      if(next.type != TCLOSE_BRACKET) {
        error_unexpected_token(next);
      }

      lhs_ast = binary_op_to_ast(allocate_ast_node(lhs_ast), op, rhs_ast);
    } else if(op.type == TOPEN_PAREN) {
      lhs_ast = parse_function_call(l, allocate_ast_node(lhs_ast), op);
    } else {
      revert_state(l);
      break;
    }
  }

  return lhs_ast;
}

Ast_Node parse_function_call(Lexer *l, Ast_Node *function, Token open_paren) {
  Ast_Node_Array args = init_Ast_Node_Array(2);
  save_state(l);
  Token first = peek_token(l);

  if(first.type != TCLOSE_PAREN) {
    revert_state(l);
    while(1) {
      Ast_Node arg = parse_expression(l, 0);
      Ast_Node_Array_push(&args, arg);
      Token t = peek_token(l);
      if(t.type == TCLOSE_PAREN) {
        break;
      } else if(t.type != TCOMMA) {
        error_unexpected_token(t);
      }
    }
  }

  Ast_Node n;
  n.file_id = open_paren.file_id;
  n.line = open_paren.line;
  n.character = open_paren.character;
  n.type = NODE_FUNCTION_CALL;
  n.data.function_call.function = function;
  n.data.function_call.arguments = args;

  return n;
}

Ast_Node parse_type(Lexer *l) {
  Token t = peek_token(l);
  if(t.type != TSYMBOL) error_unexpected_token(t);
  return symbol_to_ast(t);
}

// parses definitions, expressions, statements, declarations, blocks, etc.
Ast_Node parse_any_statement(Lexer *l) {
  save_state(l);
  Token first = peek_token(l);
  if(first.type == TOPEN_BRACE) {
    revert_state(l);
    return parse_block(l);
  } else if(first.type == TSYMBOL) { // could be decl, definition, or expression
    Token second = peek_token(l);

    if(second.type == TCOLON) {
      revert_state(l);
      return parse_decl(l);
    } else if(second.type == TDOUBLE_COLON) {
      revert_state(l);
      return parse_definition(l);
    }
  } else if(first.type == TSEMICOLON) {
    Ast_Node result;
    result.line = first.line;
    result.character = first.character;
    result.file_id = first.file_id;
    result.type = NODE_NULL;
    return result;
  } else if(first.type == TRETURN) {
    Ast_Node result;
    result.line = first.line;
    result.character = first.character;
    result.file_id = first.file_id;
    result.type = NODE_RETURN;
    result.data._return.value = allocate_ast_node(parse_expression(l, 0));
    Token semicolon = peek_token(l);
    if(semicolon.type != TSEMICOLON)
      error_token("Unexpected token. You might be missing a semicolon.", semicolon);
    return result;
  }

  revert_state(l);
  Ast_Node result = parse_expression(l, 0);
  Token semicolon = peek_token(l);
  if(semicolon.type != TSEMICOLON)
    error_token("Unexpected token. You might be missing a semicolon.", semicolon);
  return result;
}

Ast_Node parse_decl(Lexer *l) {
  Token id = peek_token(l);
  Token colon = peek_token(l);
  assert(colon.type == TCOLON && "(internal compiler error) declaration's second token must be a colon.");
  save_state(l);
  Token type = peek_token(l);

  if(type.type == TEQUALS) {
    Ast_Node value_ast = parse_expression(l, 0);
    Token semicolon = peek_token(l);
    if(semicolon.type != TSEMICOLON) {
      error_token("Unexpected token. You might be missing a semicolon.", semicolon);
    }
    // decl with set without type
    Ast_Node result;
    result.line = colon.line;
    result.character = colon.character;
    result.file_id = colon.file_id;
    result.type = NODE_DECL_WITH_SET;
    result.data.decl_with_set.symbol = id.data.symbol;
    result.data.decl_with_set.value = allocate_ast_node(value_ast);
    result.data.decl_with_set.has_type = false;
    return result;
  } else {
    revert_state(l);
    Ast_Node *type_ast = allocate_ast_node(parse_type(l));
    Token equals = peek_token(l);
    if(equals.type == TSEMICOLON) {
      // just decl
      Ast_Node result;
      result.line = colon.line;
      result.character = colon.character;
      result.file_id = colon.file_id;
      result.type = NODE_DECL;
      result.data.decl.symbol = id.data.symbol;
      result.data.decl.type = type_ast;
      return result;
    } else if(equals.type == TEQUALS) {
      // decl with type and set
      Ast_Node *value_ast = allocate_ast_node(parse_expression(l,0));
      Token semicolon = peek_token(l);
      if(semicolon.type != TSEMICOLON) {
        error_token("Unexpected token. You might be missing a semicolon.", semicolon);
      }
      Ast_Node result;
      result.line = colon.line;
      result.character = colon.character;
      result.file_id = colon.file_id;
      result.type = NODE_DECL_WITH_SET;
      result.data.decl_with_set.symbol = id.data.symbol;
      result.data.decl_with_set.type = type_ast;
      result.data.decl_with_set.value = value_ast;
      result.data.decl_with_set.has_type = true;
      return result;
    } else {
      error_unexpected_token(equals);
    }
  }
}

Ast_File parse_file(Lexer *l) {
  Ast_Node_Array nodes = init_Ast_Node_Array(2);
  Scope scope = init_Scope(2);
  while(1) {
    save_state(l);
    Token next = peek_token(l);
    revert_state(l);
    if(next.type == TEOL) {
      Ast_File r;
      r.scope = scope;
      r.nodes = nodes;
      return r;
    }

    Ast_Node node = parse_any_statement(l);
    Ast_Node_Array_push(&nodes, node);

    if(node.type == NODE_DECL || node.type == NODE_DECL_WITH_SET || node.type == NODE_FUNCTION_DEFINITION) {
      Scope_Entry e;
      switch(node.type) {
        case NODE_DECL: e.symbol = node.data.decl.symbol;
        case NODE_DECL_WITH_SET: e.symbol = node.data.decl_with_set.symbol;
        case NODE_FUNCTION_DEFINITION: e.symbol = node.data.function_definition.symbol;
      }
      e.declaration = nodes.data+nodes.length-1;
      assert(e.declaration->line == node.line); // @Cleanup using this for development of scope feature
      e.references = init_Node_Pointer_Array(2);
      Scope_push(&scope, e);
    }
  }
}

Ast_Node parse_block(Lexer *l) {
  Token first = peek_token(l);
  assert(first.type == TOPEN_BRACE && "(internal compiler error) block must begin with open brace");

  Ast_Node_Array nodes = init_Ast_Node_Array(2);
  Scope scope = init_Scope(2);
  while(1) {
    save_state(l);
    Token next = peek_token(l);
    if(next.type == TCLOSE_BRACE)
      break;
    else if(next.type == TEOL)
      error_token("'{' has no matching '}'", first);
    revert_state(l);

    Ast_Node node = parse_any_statement(l);
    Ast_Node_Array_push(&nodes, node);

    if(node.type == NODE_DECL || node.type == NODE_DECL_WITH_SET) {
      Scope_Entry e;
      switch(node.type) {
        case NODE_DECL: e.symbol = node.data.decl.symbol;
        case NODE_DECL_WITH_SET: e.symbol = node.data.decl_with_set.symbol;
      }
      e.declaration = nodes.data+nodes.length-1;
      assert(e.declaration->line == node.line); // @Cleanup using this for development of scope feature
      e.references = init_Node_Pointer_Array(2);
      Scope_push(&scope, e);
    } else if(node.type == NODE_FUNCTION_DEFINITION) {
      print_error_message("Function definitions are only allowed in the global scope.", node.file_id, node.line, node.character);
    }
  }

  Ast_Node result;
  result.line = first.line;
  result.character = first.character;
  result.file_id = first.file_id;
  result.type = NODE_BLOCK;
  result.data.block.statements = nodes;
  result.data.block.scope = scope;
  return result;
}

Ast_Node parse_definition(Lexer *l) {
  Token identifier = peek_token(l);
  Token double_colon = peek_token(l);
  assert(identifier.type == TSYMBOL && double_colon.type == TDOUBLE_COLON && "(internal compiler error) definition must begin with symbol followed by double colon");
  Token fn = peek_token(l);

  if(fn.type != TFN) error_token("Unexpected token. Only function definitions (using 'fn') are currently supported.", fn);

  Token open_paren = peek_token(l);
  if(open_paren.type != TOPEN_PAREN) error_unexpected_token(open_paren);

  Token close_paren = peek_token(l);
  if(close_paren.type != TCLOSE_PAREN) error_unexpected_token(close_paren);

  Token arrow = peek_token(l);
  if(arrow.type != TARROW) error_token("Unexpected token, expected '->'.", arrow);

  Ast_Node return_type = parse_type(l);
  Ast_Node body = parse_block(l);

  Ast_Node result;
  result.file_id = double_colon.file_id;
  result.line = double_colon.line;
  result.character = double_colon.character;

  result.type = NODE_FUNCTION_DEFINITION;
  result.data.function_definition.symbol = identifier.data.symbol;
  result.data.function_definition.return_type = allocate_ast_node(return_type);
  result.data.function_definition.body = allocate_ast_node(body);
  return result;
}
