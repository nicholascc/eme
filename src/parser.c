#include "parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lexer.h"
#include "errors.h"
#include "symbol_table.h"



// if message is NULL, lets the caller print the message
void parse_error(char *message, Location loc, bool should_exit_now) {
  print_error_message(message, loc);
  if(should_exit_now) exit(1);
  should_exit_after_parsing = true;
}

void error_unexpected_token(Token t) {
  parse_error("The parser did not expect to encounter this token.", t.loc, true);
}

void expect_and_eat_semicolon(Lexer *l) { // WARNING: will call save_state!
  save_state(l);
  Token semicolon = peek_token(l);
  if(semicolon.type != TSEMICOLON) {
    parse_error("The parser did not expect to encounter this token. You might be missing a semicolon.", semicolon.loc, false);
    revert_state(l);
  }
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
    case TLESS_THAN:
    case TLESS_THAN_OR_EQUAL_TO:
    case TGREATER_THAN:
    case TGREATER_THAN_OR_EQUAL_TO:
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

Ast_Literal *literal_int_to_ast(Token t) {
  Ast_Literal *n = allocate_ast_node(NODE_LITERAL, sizeof(Ast_Literal));
  n->n.loc = t.loc;
  n->type = (Type_Info) {TYPE_UNKNOWN_INT, 0, {0}};
  n->value = t.data.literal_int;
  return n;
}

Ast_Symbol *symbol_to_ast(Token t) {
  Ast_Symbol *n = allocate_ast_node(NODE_SYMBOL, sizeof(Ast_Symbol));
  n->n.loc = t.loc;
  n->symbol = t.data.symbol;
  return n;
}

Ast_Binary_Op *binary_op_to_ast(Ast_Node *first, Token op, Ast_Node *second) {
  Ast_Binary_Op *n = allocate_ast_node(NODE_BINARY_OP, sizeof(Ast_Binary_Op));
  n->n.loc = op.loc;

  Ast_Binary_Op_Type ast_op;
  switch(op.type) {
    case TPLUS: ast_op = OPPLUS; break;
    case TMINUS: ast_op = OPMINUS; break;
    case TASTERISK: ast_op = OPMUL; break;
    case TFORWARD_SLASH: ast_op = OPDIV; break;
    case TMODULUS: ast_op = OPMOD; break;
    case TEQUALS: ast_op = OPSET_EQUALS; break;
    case TPLUS_EQUALS: ast_op = OPPLUS_EQUALS; break;
    case TMINUS_EQUALS: ast_op = OPMINUS_EQUALS; break;
    case TLESS_THAN: ast_op = OPLESS_THAN; break;
    case TLESS_THAN_OR_EQUAL_TO: ast_op = OPLESS_THAN_OR_EQUAL_TO; break;
    case TGREATER_THAN: ast_op = OPGREATER_THAN; break;
    case TGREATER_THAN_OR_EQUAL_TO: ast_op = OPGREATER_THAN_OR_EQUAL_TO; break;
    case TDOT: ast_op = OPSTRUCT_MEMBER; break;
    case TOPEN_BRACKET: ast_op = OPSUBSCRIPT; break;
  }

  n->first = first;
  n->second = second;
  n->operator = ast_op;

  return n;
}

Ast_Unary_Op *unary_op_to_ast(Token operator, Ast_Node *operand, bool prefix) {
  Ast_Unary_Op *n = allocate_ast_node(NODE_UNARY_OP, sizeof(Ast_Unary_Op));
  n->n.loc = operator.loc;

  Ast_Unary_Op_Type ast_op;
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

  n->operator = ast_op;
  n->operand = operand;

  return n;
}

Ast_If *if_to_ast(Ast_Node *cond, Location loc, Ast_Node *first, Ast_Node *second) {
  Ast_If *n = allocate_ast_node(NODE_IF, sizeof(Ast_If));
  n->n.loc = loc;

  n->cond = cond;
  n->first = first;
  n->second = second;

  return n;
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

    case TLESS_THAN:
    case TLESS_THAN_OR_EQUAL_TO:
    case TGREATER_THAN:
    case TGREATER_THAN_OR_EQUAL_TO:
      return (u8_pair){14,13};

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

Ast_Node *parse_expression(Lexer *l, Scope *scope, u8 min_power) {
  save_state(l);
  Token lhs = peek_token(l);
  Ast_Node *lhs_ast;

  if(lhs.type == TLITERAL_INT) {
    lhs_ast = literal_int_to_ast(lhs);

  } else if(lhs.type == TSYMBOL) {
    lhs_ast = symbol_to_ast(lhs);

  } else if(is_prefix_operator(lhs)) {
    u8 power = prefix_op_binding_power(lhs.type);
    Ast_Node *operand_ast = parse_expression(l, scope, power);
    lhs_ast = unary_op_to_ast(lhs, operand_ast, true);

  } else if(lhs.type == TOPEN_BRACE) {
    revert_state(l);
    lhs_ast = parse_block(l, scope);
  } else if(lhs.type == TOPEN_PAREN) {
    lhs_ast = parse_expression(l, scope, 0);

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
        Ast_Node *mhs_ast = parse_expression(l, scope, 0);
        Token next = peek_token(l);
        if(next.type != TCOLON)
          error_unexpected_token(next);
        Ast_Node *rhs_ast = parse_expression(l, scope, powers.right);
        lhs_ast = if_to_ast(lhs_ast, op.loc, mhs_ast, rhs_ast);
      } else {
        Ast_Node *rhs_ast = parse_expression(l, scope, powers.right);
        lhs_ast = binary_op_to_ast(lhs_ast, op, rhs_ast);
      }

    } else if(is_postfix_operator(op)) {
      lhs_ast = unary_op_to_ast(op, lhs_ast, false);

    } else if(op.type == TOPEN_BRACKET) {
      Ast_Node *rhs_ast = parse_expression(l, scope, 0);

      Token next = peek_token(l);
      if(next.type != TCLOSE_BRACKET) {
        error_unexpected_token(next);
      }

      lhs_ast = binary_op_to_ast(lhs_ast, op, rhs_ast);
    } else if(op.type == TOPEN_PAREN) {
      lhs_ast = parse_function_call(l, scope, lhs_ast, op);
    } else {
      revert_state(l);
      break;
    }
  }

  return lhs_ast;
}

Ast_Node *parse_function_call(Lexer *l, Scope *scope, Ast_Node *identifier, Token open_paren) {
  Ast_Node_Ptr_Array args = init_Ast_Node_Ptr_Array(2);
  save_state(l);
  Token first = peek_token(l);

  if(first.type != TCLOSE_PAREN) {
    revert_state(l);
    while(1) {
      Ast_Node *arg = parse_expression(l, scope, 0);
      Ast_Node_Ptr_Array_push(&args, arg);
      Token t = peek_token(l);
      if(t.type == TCLOSE_PAREN) {
        break;
      } else if(t.type != TCOMMA) {
        error_unexpected_token(t);
      }
    }
  }


  Ast_Function_Call *n = allocate_ast_node(NODE_FUNCTION_CALL, sizeof(Ast_Function_Call));
  n->n.loc = open_paren.loc;
  assert(n->n.type == NODE_SYMBOL);
  n->identifier = (Ast_Symbol *)identifier;
  n->arguments = args;

  return n;
}

Ast_Node *parse_type(Lexer *l, Scope *scope) {
  Token t = peek_token(l);
  if(t.type != TSYMBOL) error_unexpected_token(t);
  int length;
  char *str = st_get_str_of(t.data.symbol, &length);
  Ast_Primitive_Type *n = allocate_ast_node(NODE_PRIMITIVE_TYPE, sizeof(Ast_Primitive_Type));
  n->n.loc = t.loc;
  bool is_signed;
  u8 width;
  if(length == 4) {
    if(strncmp("uint", str) == 0) {
      is_signed = false;
      width = 64;
    }
    else parse_error("I expected a primitive type.", t.loc, true);

  } else if(length == 3) {
    if(strncmp("int", str, 3) == 0) {
      is_signed = true;
      width = 64;
    } else if(strncmp("s16", str, 3) == 0) {
      is_signed = true;
      width = 16;
    } else if(strncmp("s32", str, 3) == 0) {
      is_signed = true;
      width = 32;
    } else if(strncmp("s64", str, 3) == 0) {
      is_signed = true;
      width = 64;
    } else if(strncmp("u16", str, 3) == 0) {
      is_signed = false;
      width = 16;
    } else if(strncmp("u32", str, 3) == 0) {
      is_signed = false;
      width = 32;
    } else if(strncmp("u64", str, 3) == 0) {
      is_signed = false;
      width = 64;
    } else parse_error("I expected a primitive type.", t.loc, true);

  } else if(length == 2) {
    if(strncmp("s8", str, 2) == 0) {
      is_signed = true;
      width = 8;
    } else if(strncmp("u8", str, 2) == 0) {
      is_signed = false;
      width = 8;
    }
    else parse_error("I expected a primitive type.", t.loc, true);
  } else parse_error("I expected a primitive type.", t.loc, true);

  n->type_info.type = TYPE_INT;
  n->type_info.data.integer.is_signed = is_signed;
  n->type_info.data.integer.width = width;

  return n;
}

// parses definitions, expressions, statements, declarations, blocks, etc.
Ast_Node *parse_any_statement(Lexer *l, Scope *scope) {
  save_state(l);
  Token first = peek_token(l);

  if(first.type == TSYMBOL) { // could be decl, definition, or expression
    Token second = peek_token(l);

    if(second.type == TCOLON) {
      revert_state(l);
      return parse_decl(l, scope);
    } else if(second.type == TDOUBLE_COLON) {
      revert_state(l);
      return parse_definition(l, scope);
    }
  } else if(first.type == TSEMICOLON) {
    Ast_Node *n = allocate_ast_node(NODE_NULL, sizeof(Ast_Node));
    n->loc = first.loc;
    return n;
  } else if(first.type == TRETURN) {
    Ast_Return *n = allocate_ast_node(NODE_RETURN, sizeof(Ast_Return));
    n->n.loc = first.loc;
    n->value = parse_expression(l, scope, 0);
    expect_and_eat_semicolon(l);
    return n;
  }

  revert_state(l);
  Ast_Node *n = parse_expression(l, scope, 0);
  expect_and_eat_semicolon(l);
  return n;
}

Ast_Node *parse_decl(Lexer *l, Scope *scope) {
  Token id = peek_token(l);
  Token colon = peek_token(l);
  assert(colon.type == TCOLON && "(internal compiler error) declaration's second token must be a colon.");
  save_state(l);
  Token type = peek_token(l);

  if(type.type == TEQUALS) {
    Ast_Node *value_ast = parse_expression(l, scope, 0);
    expect_and_eat_semicolon(l);
    Ast_Untyped_Decl_Set *n = allocate_ast_node(NODE_UNTYPED_DECL_SET, sizeof(Ast_Untyped_Decl_Set));
    n->n.loc = colon.loc;
    n->symbol = id.data.symbol;
    n->value = value_ast;
    n->type_info = UNKNOWN_TYPE_INFO;
    return n;
  } else {
    revert_state(l);
    Ast_Node *type_ast = parse_type(l, scope);
    Token equals = peek_token(l);
    if(equals.type == TSEMICOLON) {
      // just decl
      Ast_Typed_Decl *n = allocate_ast_node(NODE_TYPED_DECL, sizeof(Ast_Typed_Decl));
      n->n.loc = colon.loc;
      n->symbol = id.data.symbol;
      n->type = type_ast;
      n->type_info = UNKNOWN_TYPE_INFO;
      return n;
    } else if(equals.type == TEQUALS) {
      // decl with type and set
      Ast_Node *value_ast = parse_expression(l, scope, 0);
      expect_and_eat_semicolon(l);
      Ast_Typed_Decl_Set *n = allocate_ast_node(NODE_TYPED_DECL_SET, sizeof(Ast_Typed_Decl_Set));
      n->n.loc = colon.loc;
      n->symbol = id.data.symbol;
      n->type = type_ast;
      n->value = value_ast;
      n->type_info = UNKNOWN_TYPE_INFO;
      return n;
    } else {
      error_unexpected_token(equals);
    }
  }
}

Ast *parse_file(Lexer *l) {
  Ast *result = malloc(sizeof(Ast));
  result->compilation_units = init_Compilation_Unit_Ptr_Array(32);
  result->scope.is_ordered = false;
  result->scope.has_parent = false;
  result->scope.entries = init_Scope_Entry_Array(2);

  while(1) {
    save_state(l);
    Token next = peek_token(l);
    revert_state(l);
    if(next.type == TEOL)
      return result;

    Ast_Node *node = parse_any_statement(l, &result->scope);
    Compilation_Unit *unit = allocate_null_compilation_unit();
    unit->type_inferred = false;
    unit->type_inference_seen = false;
    unit->bytecode_generated = false;
    unit->bytecode_generation_seen = false;
    unit->poisoned = false;
    unit->node = node;
    Compilation_Unit_Ptr_Array_push(&result->compilation_units, unit);

    if(node->type == NODE_TYPED_DECL ||
       node->type == NODE_TYPED_DECL_SET ||
       node->type == NODE_UNTYPED_DECL_SET ||
       node->type == NODE_FUNCTION_DEFINITION) {
      Scope_Entry e;
      switch(node->type) {
        case NODE_TYPED_DECL: e.symbol = ((Ast_Typed_Decl *)node)->symbol; break;
        case NODE_TYPED_DECL_SET: e.symbol = ((Ast_Typed_Decl_Set *)node)->symbol; break;
        case NODE_UNTYPED_DECL_SET: e.symbol = ((Ast_Untyped_Decl_Set *)node)->symbol; break;
        case NODE_FUNCTION_DEFINITION: e.symbol = ((Ast_Function_Definition *)node)->symbol; break;
      }
      e.declaration.unit = unit;
      Scope_Entry_Array_push(&result->scope.entries, e);
    }
  }
}

Ast_Node *parse_block(Lexer *l, Scope *parent_scope) {
  Token first = peek_token(l);
  assert(first.type == TOPEN_BRACE && "(internal compiler error) block must begin with open brace");

  Ast_Block *result = allocate_ast_node(NODE_BLOCK, sizeof(Ast_Block));
  result->n.loc = first.loc;
  Ast_Node_Ptr_Array *statements = &result->statements;
  *statements = init_Ast_Node_Ptr_Array(2);
  Scope *scope = &result->scope;
  scope->is_ordered = true;
  scope->has_parent = true;
  scope->parent = parent_scope;
  scope->entries = init_Scope_Entry_Array(2);

  while(1) {
    save_state(l);
    Token next = peek_token(l);
    if(next.type == TCLOSE_BRACE)
      return result;
    else if(next.type == TEOL)
      parse_error("'{' has no matching '}'", first.loc, true);
    revert_state(l);

    Ast_Node *node = parse_any_statement(l, scope);
    Ast_Node_Ptr_Array_push(statements, node);

    if(node->type == NODE_TYPED_DECL ||
       node->type == NODE_TYPED_DECL_SET ||
       node->type == NODE_UNTYPED_DECL_SET) {
      Scope_Entry e;
      switch(node->type) {
        case NODE_TYPED_DECL: e.symbol = ((Ast_Typed_Decl *)node)->symbol; break;
        case NODE_TYPED_DECL_SET: e.symbol = ((Ast_Typed_Decl_Set *)node)->symbol; break;
        case NODE_UNTYPED_DECL_SET: e.symbol = ((Ast_Untyped_Decl_Set *)node)->symbol; break;
      }
      e.declaration.node = node;
      Scope_Entry_Array_push(&scope->entries, e);
    } else if(node->type == NODE_FUNCTION_DEFINITION) {
      parse_error("Function definitions are only allowed in the global scope.", node->loc, false);
    }
  }
}

Ast_Node *parse_definition(Lexer *l, Scope *scope) {
  Token identifier = peek_token(l);
  Token double_colon = peek_token(l);
  assert(identifier.type == TSYMBOL && double_colon.type == TDOUBLE_COLON && "(internal compiler error) definition must begin with symbol followed by double colon");
  Token fn = peek_token(l);

  if(fn.type != TFN) parse_error("Unexpected token. Only function definitions (using 'fn') are currently supported.", fn.loc, true);

  Token open_paren = peek_token(l);
  if(open_paren.type != TOPEN_PAREN) error_unexpected_token(open_paren);

  Token close_paren = peek_token(l);
  if(close_paren.type != TCLOSE_PAREN) error_unexpected_token(close_paren);

  Token arrow = peek_token(l);
  if(arrow.type != TARROW) parse_error("Unexpected token, expected '->'.", arrow.loc, true);

  Ast_Node *return_type = parse_type(l, scope);
  Ast_Node *body = parse_block(l, scope);

  Ast_Function_Definition *n = allocate_ast_node(NODE_FUNCTION_DEFINITION, sizeof(Ast_Function_Definition));
  n->n.loc = double_colon.loc;

  n->symbol = identifier.data.symbol;
  n->return_type = return_type;
  n->body = body;
  return n;
}
