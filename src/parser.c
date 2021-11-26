#include "parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lexer.h"
#include "errors.h"
#include "symbol_table.h"
#include "bytecode.h"



// if message is NULL, lets the caller print the message
void parse_error(char *message, Location loc, bool should_exit_now) {
  print_error_message(message, loc);
  if(should_exit_now) exit(1);
  should_exit_after_parsing = true;
}

void error_unexpected_token(Token t) {
  parse_error("The parser did not expect to encounter this token.", t.loc, true);
}

void expect_and_eat_semicolon(Token_Reader *r) { // WARNING: will call save_state!
  save_state(r);
  Token semicolon = peek_token(r);
  if(semicolon.type != TSEMICOLON) {
    parse_error("The parser did not expect to encounter this token. You might be missing a semicolon.", semicolon.loc, false);
    revert_state(r);
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
  n->result_is_used = false;
  n->result_type_info = NOTHING_TYPE_INFO;
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

// Can pass a null pointer to needs_semicolon if you don't need that information.
Ast_Node *parse_expression(Token_Reader *r, Scope *scope, u8 min_power, bool *needs_semicolon) {
  save_state(r);
  Token lhs = peek_token(r);
  Ast_Node *lhs_ast;

  bool lhs_needs_semicolon = true;

  if(lhs.type == TLITERAL_INT) {
    lhs_ast = literal_int_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TSYMBOL) {
    lhs_ast = symbol_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TIF) {
    {
      Token t = peek_token(r);
      if(t.type != TOPEN_PAREN) {
        print_error_message("The conditional for an if statement must be parenthesized.", t.loc);
        exit(1);
      }
    }
    Ast_Node *cond = parse_expression(r, scope, 0, NULL);
    {
      Token t = peek_token(r);
      if(t.type != TCLOSE_PAREN) error_unexpected_token(t);
    }
    Ast_Node *if_true = parse_any_statement(r, scope, false);

    save_state(r);
    Token t = peek_token(r);
    Ast_Node *if_false;

    if(t.type == TELSE) {
      if_false = parse_any_statement(r, scope, false);
      lhs_ast = if_to_ast(cond, lhs.loc, if_true, if_false);
    } else {
      revert_state(r);
      if_false = allocate_ast_node(NODE_NULL, sizeof(Ast_Node));
      if_false->loc.file_id = t.loc.file_id;
      if_false->loc.line = -1;
      if_false->loc.character = -1;
    }

    lhs_ast = if_to_ast(cond, lhs.loc, if_true, if_false);
    lhs_needs_semicolon = false;

  } else if(is_prefix_operator(lhs)) {
    u8 power = prefix_op_binding_power(lhs.type);
    Ast_Node *operand_ast = parse_expression(r, scope, power, NULL);
    lhs_ast = unary_op_to_ast(lhs, operand_ast, true);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TOPEN_BRACE) {
    revert_state(r);
    lhs_ast = parse_block(r, scope);
    lhs_needs_semicolon = false;
  } else if(lhs.type == TOPEN_PAREN) {
    lhs_ast = parse_expression(r, scope, 0, NULL);

    Token next = peek_token(r);
    if(next.type != TCLOSE_PAREN)
      error_unexpected_token(next);
    lhs_needs_semicolon = true;
  } else error_unexpected_token(lhs);



  while(1) {
    save_state(r);
    Token op = peek_token(r);

    if(is_non_unary_operator(op)) {
      u8_pair powers = binary_op_binding_power(op.type);
      if(powers.left < min_power) {
        revert_state(r);
        break;
      }

      if(op.type == TQUESTION_MARK) {
        Ast_Node *mhs_ast = parse_expression(r, scope, 0, NULL);
        Token next = peek_token(r);
        if(next.type != TCOLON)
          error_unexpected_token(next);
        Ast_Node *rhs_ast = parse_expression(r, scope, powers.right, NULL);
        lhs_ast = if_to_ast(lhs_ast, op.loc, mhs_ast, rhs_ast);
      } else {
        Ast_Node *rhs_ast = parse_expression(r, scope, powers.right, NULL);
        lhs_ast = binary_op_to_ast(lhs_ast, op, rhs_ast);
      }

      lhs_needs_semicolon = true;

    } else if(is_postfix_operator(op)) {
      lhs_ast = unary_op_to_ast(op, lhs_ast, false);
      lhs_needs_semicolon = true;

    } else if(op.type == TOPEN_BRACKET) {
      Ast_Node *rhs_ast = parse_expression(r, scope, 0, NULL);

      Token next = peek_token(r);
      if(next.type != TCLOSE_BRACKET) {
        error_unexpected_token(next);
      }

      lhs_ast = binary_op_to_ast(lhs_ast, op, rhs_ast);
      lhs_needs_semicolon = true;
    } else if(op.type == TOPEN_PAREN) {
      lhs_ast = parse_function_call(r, scope, lhs_ast, op);
      lhs_needs_semicolon = true;
    } else {
      revert_state(r);
      break;
    }
  }

  if(needs_semicolon != NULL) *needs_semicolon = lhs_needs_semicolon;
  return lhs_ast;
}

Ast_Node *parse_function_call(Token_Reader *r, Scope *scope, Ast_Node *identifier, Token open_paren) {
  Ast_Node_Ptr_Array args = init_Ast_Node_Ptr_Array(2);
  save_state(r);
  Token first = peek_token(r);
  if(first.type != TCLOSE_PAREN) {
    revert_state(r);
    while(1) {
      Ast_Node *arg = parse_expression(r, scope, 0, NULL);
      Ast_Node_Ptr_Array_push(&args, arg);
      Token t = peek_token(r);
      if(t.type == TCLOSE_PAREN) {
        break;
      } else if(t.type != TCOMMA) {
        error_unexpected_token(t);
      }
    }
  }


  Ast_Function_Call *n = allocate_ast_node(NODE_FUNCTION_CALL, sizeof(Ast_Function_Call));
  n->n.loc = open_paren.loc;
  assert(identifier->type == NODE_SYMBOL);
  n->identifier = (Ast_Symbol *)identifier;
  n->arguments = args;

  return n;
}

typedef struct Symbol_Integer_Pair {
  const char *str;
  symbol sym;
  u8 width;
  bool is_signed;
} Symbol_Integer_Pair;

int SYMBOL_INTEGERS_COUNT = 10;
Symbol_Integer_Pair symbol_integers[] =
  {{"u8", NULL, 8, false},
   {"u16", NULL, 16, false},
   {"u32", NULL, 32, false},
   {"u64", NULL, 64, false},
   {"s8", NULL, 8, true},
   {"s16", NULL, 16, true},
   {"s32", NULL, 32, true},
   {"s64", NULL, 64, true},
   {"uint", NULL, 64, false},
   {"int", NULL, 64, true}};


void register_parser_symbols() {
  for(int i = 0; i < SYMBOL_INTEGERS_COUNT; i++)
    symbol_integers[i].sym = st_get_id_of_null_terminated((char *)symbol_integers[i].str);
}

Ast_Node *parse_type(Token_Reader *r, Scope *scope) {
  Token t = peek_token(r);
  if(t.type != TSYMBOL) error_unexpected_token(t);
  Ast_Primitive_Type *n = allocate_ast_node(NODE_PRIMITIVE_TYPE, sizeof(Ast_Primitive_Type));
  n->n.loc = t.loc;

  bool is_signed;
  u8 width;
  bool should_error = true;
  for(int i = 0; i < SYMBOL_INTEGERS_COUNT; i++) {
    if(t.data.symbol == symbol_integers[i].sym) {
      n->type_info = INT_TYPE_INFO(symbol_integers[i].is_signed, symbol_integers[i].width);
      should_error = false;
      break;
    }
  }
  if(should_error) parse_error("I expected a primitive type.", t.loc, true);

  return n;
}

// parses definitions, expressions, statements, declarations, blocks, etc.
Ast_Node *parse_any_statement(Token_Reader *r, Scope *scope, bool require_semicolon_for_expressions) {
  save_state(r);
  Token first = peek_token(r);

  if(first.type == TSYMBOL) { // could be decl, definition, or expression
    Token second = peek_token(r);

    if(second.type == TCOLON) {
      revert_state(r);
      return parse_decl(r, scope);
    } else if(second.type == TDOUBLE_COLON) {
      revert_state(r);
      return parse_definition(r, scope);
    }
  } else if(first.type == TSEMICOLON) {
    Ast_Node *n = allocate_ast_node(NODE_NULL, sizeof(Ast_Node));
    n->loc = first.loc;
    return n;
  } else if(first.type == TRETURN) {
    Ast_Return *n = allocate_ast_node(NODE_RETURN, sizeof(Ast_Return));
    n->n.loc = first.loc;
    n->value = parse_expression(r, scope, 0, NULL);
    expect_and_eat_semicolon(r);
    return n;
  }

  revert_state(r);
  bool needs_semicolon;
  Ast_Node *n = parse_expression(r, scope, 0, &needs_semicolon);
  if(needs_semicolon && require_semicolon_for_expressions) expect_and_eat_semicolon(r);
  return n;
}

Ast_Node *parse_decl(Token_Reader *r, Scope *scope) {
  Token id = peek_token(r);
  Token colon = peek_token(r);
  assert(colon.type == TCOLON && "(internal compiler error) declaration's second token must be a colon.");
  save_state(r);
  Token type = peek_token(r);

  if(type.type == TEQUALS) {
    Ast_Node *value_ast = parse_expression(r, scope, 0, NULL);
    expect_and_eat_semicolon(r);
    Ast_Untyped_Decl_Set *n = allocate_ast_node(NODE_UNTYPED_DECL_SET, sizeof(Ast_Untyped_Decl_Set));
    n->n.loc = colon.loc;
    n->symbol = id.data.symbol;
    n->value = value_ast;
    n->type_info = UNKNOWN_TYPE_INFO;
    return n;
  } else {
    revert_state(r);
    Ast_Node *type_ast = parse_type(r, scope);
    Token equals = peek_token(r);
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
      Ast_Node *value_ast = parse_expression(r, scope, 0, NULL);
      expect_and_eat_semicolon(r);
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

Ast *parse_file(Token_Reader *r) {
  Ast *result = malloc(sizeof(Ast));
  result->compilation_units = init_Compilation_Unit_Ptr_Array(32);
  result->scope.is_ordered = false;
  result->scope.has_parent = false;
  result->scope.entries = init_Scope_Entry_Array(2);

  while(1) {
    save_state(r);
    Token next = peek_token(r);
    revert_state(r);
    if(next.type == TEOL)
      return result;

    Ast_Node *node = parse_any_statement(r, &result->scope, true);
    Compilation_Unit *declaration_unit; // to be referred to in the corresponding Scope_Entry
    if(node->type == NODE_FUNCTION_DEFINITION) {
      // A function is represented by a signature and body which must be
      // type-checked separately. See more explanation in ast.h for Compilation_Unit.
      Ast_Function_Definition *n = node;

      Compilation_Unit *sig = allocate_null_compilation_unit();
      Compilation_Unit *body = allocate_null_compilation_unit();

      sig->type = UNIT_FUNCTION_SIGNATURE;
      sig->type_inferred = false;
      sig->type_inference_seen = false;
      sig->bytecode_generated = false;
      sig->bytecode_generating = false;
      sig->poisoned = false;
      sig->node = node;
      sig->scope = &result->scope;
      sig->data.body = body;
      Compilation_Unit_Ptr_Array_push(&result->compilation_units, sig);

      body->type = UNIT_FUNCTION_BODY;
      body->type_inferred = false;
      body->type_inference_seen = false;
      body->bytecode_generated = false;
      body->bytecode_generating = false;
      body->poisoned = false;
      body->node = node;
      body->scope = &result->scope;
      body->data.signature = sig;
      body->bytecode.function = malloc(sizeof(Bytecode_Function));
      body->bytecode.function->parent = body;
      Compilation_Unit_Ptr_Array_push(&result->compilation_units, body);

      declaration_unit = sig;
    } else {
      assert(false);
    }

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
      e.declaration.unit = declaration_unit;
      Scope_Entry_Array_push(&result->scope.entries, e);
    }
  }
}

Ast_Node *parse_block(Token_Reader *r, Scope *parent_scope) {
  Token first = peek_token(r);
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
    save_state(r);
    Token next = peek_token(r);
    if(next.type == TCLOSE_BRACE)
      return result;
    else if(next.type == TEOL)
      parse_error("'{' has no matching '}'", first.loc, true);
    revert_state(r);

    Ast_Node *node = parse_any_statement(r, scope, true);
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

Ast_Node *parse_definition(Token_Reader *r, Scope *scope) {
  Token identifier = peek_token(r);
  Token double_colon = peek_token(r);
  assert(identifier.type == TSYMBOL && double_colon.type == TDOUBLE_COLON && "(internal compiler error) definition must begin with symbol followed by double colon");
  Token fn = peek_token(r);

  if(fn.type != TFN) parse_error("Unexpected token. Only function definitions (using 'fn') are currently supported.", fn.loc, true);

  Token open_paren = peek_token(r);
  if(open_paren.type != TOPEN_PAREN) error_unexpected_token(open_paren);

  Ast_Function_Definition *n = allocate_ast_node(NODE_FUNCTION_DEFINITION, sizeof(Ast_Function_Definition));
  n->n.loc = double_colon.loc;
  n->scope.is_ordered = true;
  n->scope.has_parent = true;
  n->scope.parent = scope;
  n->scope.entries = init_Scope_Entry_Array(2);

  Ast_Node_Ptr_Array parameters = init_Ast_Node_Ptr_Array(2);

  save_state(r);
  Token first = peek_token(r);
  if(first.type != TCLOSE_PAREN) {
    revert_state(r);
    while(1) {
      Token sym = peek_token(r);
      if(sym.type != TSYMBOL) error_unexpected_token(sym);
      Token colon = peek_token(r);
      if(colon.type != TCOLON) error_unexpected_token(colon);

      Ast_Node *type = parse_type(r, scope); // we are using the parent scope here because this should be not be able to use other arguments (at least for now...)
      Ast_Function_Parameter *param = allocate_ast_node(NODE_FUNCTION_PARAMETER, sizeof(Ast_Function_Parameter));
      param->n.loc = colon.loc;
      param->symbol = sym.data.symbol;
      param->type = type;
      param->type_info = UNKNOWN_TYPE_INFO;
      Ast_Node_Ptr_Array_push(&parameters, param);

      Scope_Entry e;
      e.symbol = sym.data.symbol;
      e.declaration.node = param;
      Scope_Entry_Array_push(&n->scope.entries, e);

      Token next = peek_token(r);
      if(next.type == TCLOSE_PAREN) break;
      else if(next.type != TCOMMA) error_unexpected_token(next);
    }
  }

  Token arrow = peek_token(r);
  if(arrow.type != TARROW) parse_error("Unexpected token, expected '->'.", arrow.loc, true);

  Ast_Node *return_type = parse_type(r, scope); // we are using the parent scope here because this should be not be able to use the arguments (at least for now...)
  Ast_Node *body = parse_block(r, &n->scope);


  n->symbol = identifier.data.symbol;
  n->parameters = parameters;
  n->return_type = return_type;
  n->return_type_info = UNKNOWN_TYPE_INFO;
  n->body = body;

  return n;
}
