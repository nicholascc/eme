#include "parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "ast.h"
#include "lexer.h"
#include "errors.h"
#include "symbol_table.h"
#include "bytecode.h"
#include "files.h"



// if message is NULL, lets the caller print the message
void parse_error(char *message, Location loc, bool should_exit_now) {
  print_error_message(message, loc);
  if(should_exit_now) exit(1);
  should_exit_after_parsing = true;
}

void error_unexpected_token(Token t) {
  parse_error("I did not expect to encounter this token.", t.loc, true);
}

void expect_and_eat_semicolon(Token_Reader *r) { // WARNING: will call save_state!
  save_state(r);
  Token semicolon = peek_token(r);
  if(semicolon.type != TSEMICOLON) {
    parse_error("I did not expect to encounter this token. You might be missing a semicolon.", semicolon.loc, false);
    revert_state(r);
  }
}

bool is_infix_operator(Token t) {
  switch(t.type) {
    case TPLUS:
    case TMINUS:
    case TASTERISK:
    case TFORWARD_SLASH:
    case TMODULUS:
    case TTILDE:
    case TEQUALS:
    case TPLUS_EQUALS:
    case TMINUS_EQUALS:
    case TDOUBLE_EQUALS:
    case TDOUBLE_AMPERSAND:
    case TDOUBLE_OR:
    case TLESS_THAN:
    case TLESS_THAN_OR_EQUAL_TO:
    case TGREATER_THAN:
    case TGREATER_THAN_OR_EQUAL_TO:
    case TDOT:
    case TDOT_CARET:
    case TOPEN_BRACKET:
    case TCARET_OPEN_BRACKET:
    case TOPEN_PAREN:
    case TQUESTION_MARK:
      return true;
    default: return false;
  }
}

bool is_prefix_operator(Token t) {
  switch(t.type) {
    case TMINUS:
    case TASTERISK:
    case TCARET:
    case TDOUBLE_PLUS:
    case TDOUBLE_MINUS:
    case TNOT:
      return true;
    default:
      return false;
  }
}

bool is_postfix_operator(Token t) {
  return t.type == TDOUBLE_PLUS || t.type == TDOUBLE_MINUS;
}

Ast_Literal *literal_int_to_ast(Token t) {
  Ast_Literal *n = (Ast_Literal *)allocate_ast_node_type(NODE_LITERAL, sizeof(Ast_Literal));
  n->n.loc = t.loc;
  n->type = allocate_unknown_int_type(t.data.literal_int);
  n->value = t.data.literal_int;
  return n;
}

Ast_Literal *literal_bool_to_ast(Token t) {
  Ast_Literal *n = (Ast_Literal *)allocate_ast_node_type(NODE_LITERAL, sizeof(Ast_Literal));
  n->n.loc = t.loc;
  n->type = BOOL_TYPE;
  n->value = t.data.literal_bool;
  return n;
}

Ast_Literal_String *literal_string_to_ast(Token t) {
  Ast_Literal_String *n = (Ast_Literal_String *)allocate_ast_node_type(NODE_LITERAL_STRING, sizeof(Ast_Literal_String));
  n->n.loc = t.loc;
  n->string = t.data.literal_string;
  n->length = strlen(n->string);
  return n;
}


typedef struct Symbol_Integer_Pair {
  const char *str;
  symbol sym;
  u8 width;
  bool is_signed;
  Type type;
} Symbol_Integer_Pair;

int SYMBOL_INTEGERS_COUNT = 10;
Symbol_Integer_Pair symbol_integers[] =
  {{"u8", NULL_SYMBOL, 8, false, {0,NULL}},
   {"u16", NULL_SYMBOL, 16, false, {0,NULL}},
   {"u32", NULL_SYMBOL, 32, false, {0,NULL}},
   {"u64", NULL_SYMBOL, 64, false, {0,NULL}},
   {"s8", NULL_SYMBOL, 8, true, {0,NULL}},
   {"s16", NULL_SYMBOL, 16, true, {0,NULL}},
   {"s32", NULL_SYMBOL, 32, true, {0,NULL}},
   {"s64", NULL_SYMBOL, 64, true, {0,NULL}},
   {"uint", NULL_SYMBOL, 64, false, {0,NULL}},
   {"int", NULL_SYMBOL, 64, true, {0,NULL}}};

typedef struct Symbol_Type_Pair {
  symbol sym;
  Type type;
} Symbol_Type_Pair;

Symbol_Type_Pair pair_boolean = {NULL_SYMBOL, {0,NULL}};
Symbol_Type_Pair pair_type = {NULL_SYMBOL, {0,NULL}};

Symbol_Type_Pair pair_f32 = {NULL_SYMBOL, {0,NULL}};
Symbol_Type_Pair pair_f64 = {NULL_SYMBOL, {0,NULL}};
Symbol_Type_Pair pair_float = {NULL_SYMBOL, {0,NULL}};

void register_parser_symbols() {
  for(int i = 0; i < SYMBOL_INTEGERS_COUNT; i++) {
    symbol_integers[i].sym = st_get_id_of((char *)symbol_integers[i].str, -1);
    symbol_integers[i].type = integer_type_with(symbol_integers[i].is_signed, symbol_integers[i].width);;
  }
  pair_boolean.sym = st_get_id_of("bool", -1);
  pair_boolean.type = BOOL_TYPE;
  pair_type.sym = st_get_id_of("type", -1);
  pair_type.type = FTYPE_TYPE;

  pair_f32.sym = st_get_id_of("f32", -1);
  pair_f32.type = FLOAT32_TYPE;
  pair_f64.sym = st_get_id_of("f64", -1);
  pair_f64.type = FLOAT64_TYPE;
  pair_float.sym = st_get_id_of("float", -1);
  pair_float.type = FLOAT64_TYPE;
}

Ast_Symbol *symbol_to_ast(Token t) {
  Ast_Symbol *n = (Ast_Symbol *)allocate_ast_node_type(NODE_SYMBOL, sizeof(Ast_Symbol));
  n->n.loc = t.loc;
  n->symbol = t.data.symbol;
  return n;
}

Ast_Bind_Symbol *bind_symbol_to_ast(Token sign, Token t) {
  Ast_Bind_Symbol *n = (Ast_Bind_Symbol *)allocate_ast_node_type(NODE_BIND_SYMBOL, sizeof(Ast_Bind_Symbol));
  n->n.loc = sign.loc;
  n->symbol = t.data.symbol;
  return n;
}

Ast_Primitive_Type *prim_type_to_ast(Token t, Type ty) {
  Ast_Primitive_Type *n = (Ast_Primitive_Type *)allocate_ast_node_type(NODE_PRIMITIVE_TYPE, sizeof(Ast_Primitive_Type));
  n->n.loc = t.loc;
  n->type = ty;
  return n;
}

Ast_Node *symbol_or_prim_type_to_ast(Token t) {
  for(int i = 0; i < SYMBOL_INTEGERS_COUNT; i++)
    if(t.data.symbol == symbol_integers[i].sym)
      return (Ast_Node *)prim_type_to_ast(t, symbol_integers[i].type);

  if(t.data.symbol == pair_boolean.sym)
    return (Ast_Node *)prim_type_to_ast(t, pair_boolean.type);
  if(t.data.symbol == pair_type.sym)
    return (Ast_Node *)prim_type_to_ast(t, pair_type.type);
  if(t.data.symbol == pair_f32.sym)
    return (Ast_Node *)prim_type_to_ast(t, pair_f32.type);
  if(t.data.symbol == pair_f64.sym)
    return (Ast_Node *)prim_type_to_ast(t, pair_f64.type);
  if(t.data.symbol == pair_float.sym)
    return (Ast_Node *)prim_type_to_ast(t, pair_float.type);

  return (Ast_Node *)symbol_to_ast(t);
}

Ast_Binary_Op *binary_op_to_ast(Ast_Node *first, Token op, Ast_Node *second) {
  Ast_Binary_Op *n = (Ast_Binary_Op *)allocate_ast_node_type(NODE_BINARY_OP, sizeof(Ast_Binary_Op));
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
    case TDOUBLE_EQUALS: ast_op = OPEQUALS; break;
    case TDOUBLE_AMPERSAND: ast_op = OPAND; break;
    case TDOUBLE_OR: ast_op = OPOR; break;
    case TLESS_THAN: ast_op = OPLESS_THAN; break;
    case TLESS_THAN_OR_EQUAL_TO: ast_op = OPLESS_THAN_OR_EQUAL_TO; break;
    case TGREATER_THAN: ast_op = OPGREATER_THAN; break;
    case TGREATER_THAN_OR_EQUAL_TO: ast_op = OPGREATER_THAN_OR_EQUAL_TO; break;
    case TDOT: ast_op = OPSTRUCT_MEMBER; break;
    case TDOT_CARET: ast_op = OPSTRUCT_MEMBER_REF; break;
    case TCARET_OPEN_BRACKET: ast_op = OPSUBSCRIPT_REF; break;
    case TOPEN_BRACKET: ast_op = OPSUBSCRIPT; break;
    default: assert(false);
  }

  n->first = first;
  n->second = second;
  n->operator = ast_op;

  return n;
}

Ast_Unary_Op *unary_op_to_ast(Token operator, Ast_Node *operand, bool prefix) {
  Ast_Unary_Op *n = (Ast_Unary_Op *)allocate_ast_node_type(NODE_UNARY_OP, sizeof(Ast_Unary_Op));
  n->n.loc = operator.loc;

  Ast_Unary_Op_Type ast_op;
  switch(operator.type) {
    case TMINUS: ast_op = OPNEGATE; break;
    case TASTERISK: ast_op = OPDEREFERENCE; break;
    case TCARET: ast_op = OPREFERENCE; break;
    case TNOT: ast_op = OPNOT; break;
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
  Ast_If *n = (Ast_If *)allocate_ast_node_type(NODE_IF, sizeof(Ast_If));
  n->n.loc = loc;
  n->result_is_used = false;
  n->result_type = NOTHING_TYPE;
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
Left associative: --- type parsing starts at this precedence
06: ||                            [✓]
08: ^^                            [ ]
10: &&                            [✓]
12: == !==                        [✓]
14: < <= > >=                     [✓]
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
    $                             [✓]
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

u8_pair infix_op_binding_power(Token_Type type) {
  switch(type) {
    case TEQUALS:
    case TPLUS_EQUALS:
    case TMINUS_EQUALS:
      return (u8_pair){2,1};

    case TQUESTION_MARK:
      return (u8_pair){4,3};

    case TDOUBLE_OR:
      return (u8_pair){6,5};

    case TDOUBLE_AMPERSAND:
      return (u8_pair){10,9};

    case TDOUBLE_EQUALS:
      return (u8_pair){12,11};

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

    case TTILDE:
      return (u8_pair){4,3};

    case TDOT:
    case TDOT_CARET:
    case TOPEN_BRACKET:
    case TCARET_OPEN_BRACKET:
    case TOPEN_PAREN:
      return (u8_pair){49,50};
    default: assert(false && "(internal compiler error) binary op does not exist");
  }
}

u8 prefix_op_binding_power(Token_Type type) {
  switch(type) {
    case TMINUS:
    case TASTERISK:
    case TCARET:
    case TDOUBLE_PLUS:
    case TDOUBLE_MINUS:
    case TNOT:
      return 40;
    default: assert(false && "(internal compiler error) prefix op does not exist");
  }
}

// Can pass a null pointer to needs_semicolon and uses_binding_operator if you don't need that information.
Ast_Node *parse_expression(Token_Reader *r, Scope *scope, u8 min_power, bool *needs_semicolon, bool *uses_bind_symbol) {
  save_state(r);
  Token lhs = peek_token(r);
  Ast_Node *lhs_ast;

  bool lhs_needs_semicolon = true;
  bool this_uses_bind_symbol = false;

  if(lhs.type == TLITERAL_INT) {
    lhs_ast = (Ast_Node *)literal_int_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TLITERAL_BOOL) {
    lhs_ast = (Ast_Node *)literal_bool_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TLITERAL_STRING) {
    lhs_ast = (Ast_Node *)literal_string_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TSYMBOL) {
    lhs_ast = (Ast_Node *)symbol_or_prim_type_to_ast(lhs);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TIF) {
    {
      Token t = peek_token(r);
      if(t.type != TOPEN_PAREN) {
        print_error_message("The conditional for an if statement must be parenthesized.", t.loc);
        exit(1);
      }
    }
    Ast_Node *cond = parse_expression(r, scope, 0, NULL, uses_bind_symbol);
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
      lhs_ast = (Ast_Node *)if_to_ast(cond, lhs.loc, if_true, if_false);
    } else {
      revert_state(r);
      if_false = allocate_ast_node_type(NODE_NULL, sizeof(Ast_Node));
      if_false->loc.file_id = t.loc.file_id;
      if_false->loc.line = -1;
      if_false->loc.character = -1;
    }

    lhs_ast = (Ast_Node *)if_to_ast(cond, lhs.loc, if_true, if_false);
    lhs_needs_semicolon = false;

  } else if(is_prefix_operator(lhs)) {
    u8 power = prefix_op_binding_power(lhs.type);
    Ast_Node *operand_ast = parse_expression(r, scope, power, NULL, uses_bind_symbol);
    lhs_ast = (Ast_Node *)unary_op_to_ast(lhs, operand_ast, true);
    lhs_needs_semicolon = true;

  } else if(lhs.type == TDOLLAR_SIGN) {
    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);
    lhs_ast = (Ast_Node *)bind_symbol_to_ast(lhs, sym);
    {
      if(scope->type != SYMBOL_SCOPE) {
        print_error_message("The binding ($) operator may only be used in the arguments to a function.", lhs.loc);
        exit(1);
      }
      Scope_Entry e;
      e.symbol = sym.data.symbol;
      Scope_Entry_Array_push(&scope->entries, e);
    }
    this_uses_bind_symbol = true;
  } else if(lhs.type == TOPEN_BRACE) {
    revert_state(r);
    lhs_ast = (Ast_Node *)parse_block(r, scope);
    lhs_needs_semicolon = false;
  } else if(lhs.type == TOPEN_PAREN) {
    lhs_ast = (Ast_Node *)parse_expression(r, scope, 0, NULL, uses_bind_symbol);

    Token next = peek_token(r);
    if(next.type != TCLOSE_PAREN)
      error_unexpected_token(next);
    lhs_needs_semicolon = true;
  } else error_unexpected_token(lhs);



  while(1) {
    save_state(r);
    Token op = peek_token(r);

    if(is_infix_operator(op)) {
      u8_pair powers = infix_op_binding_power(op.type);
      if(powers.left < min_power) {
        revert_state(r);
        break;
      }

      if(op.type == TQUESTION_MARK) {
        Ast_Node *mhs_ast = parse_expression(r, scope, 0, NULL, uses_bind_symbol);
        Token next = peek_token(r);
        if(next.type != TCOLON)
          error_unexpected_token(next);
        Ast_Node *rhs_ast = parse_expression(r, scope, powers.right, NULL, uses_bind_symbol);
        lhs_ast = (Ast_Node *)if_to_ast(lhs_ast, op.loc, mhs_ast, rhs_ast);
      } else if(op.type == TOPEN_BRACKET || op.type == TCARET_OPEN_BRACKET) {
        Ast_Node *rhs_ast = parse_expression(r, scope, 0, NULL, uses_bind_symbol);
        Token next = peek_token(r);
        if(next.type != TCLOSE_BRACKET)
          error_unexpected_token(next);
        lhs_ast = (Ast_Node *)binary_op_to_ast(lhs_ast, op, rhs_ast);
        lhs_needs_semicolon = true;
      } else if(op.type == TTILDE) {
        Token next = peek_token(r);
        if(next.type != TSYMBOL)
          error_unexpected_token(next);
        Ast_Node *identifier = (Ast_Node *)symbol_to_ast(next);
        Token paren = peek_token(r);
        if(paren.type != TOPEN_PAREN)
          error_unexpected_token(paren);
        lhs_ast = parse_function_call(r, scope, lhs_ast, identifier, paren, uses_bind_symbol);
      } else if(op.type == TOPEN_PAREN) {
        lhs_ast = (Ast_Node *)parse_function_call(r, scope, NULL, lhs_ast, op, uses_bind_symbol);
      } else {
        Ast_Node *rhs_ast = parse_expression(r, scope, powers.right, NULL, uses_bind_symbol);
        lhs_ast = (Ast_Node *)binary_op_to_ast(lhs_ast, op, rhs_ast);
      }

      lhs_needs_semicolon = true;

    } else if(is_postfix_operator(op)) {
      lhs_ast = (Ast_Node *)unary_op_to_ast(op, lhs_ast, false);
      lhs_needs_semicolon = true;
    } else {
      revert_state(r);
      break;
    }
  }

  if(needs_semicolon != NULL) *needs_semicolon = lhs_needs_semicolon;
  if(uses_bind_symbol != NULL) *uses_bind_symbol = *uses_bind_symbol || this_uses_bind_symbol;
  return lhs_ast;
}

Ast_Node *parse_function_call(Token_Reader *r, Scope *scope, Ast_Node *tilde_argument, Ast_Node *identifier, Token open_paren, bool *uses_bind_symbol) {
  Ast_Node_Ptr_Array args = init_Ast_Node_Ptr_Array(2);
  if(tilde_argument) Ast_Node_Ptr_Array_push(&args, tilde_argument);
  save_state(r);
  Token first = peek_token(r);
  if(first.type != TCLOSE_PAREN) {
    revert_state(r);
    while(1) {
      Ast_Node *arg = parse_expression(r, scope, 0, NULL, uses_bind_symbol);
      Ast_Node_Ptr_Array_push(&args, arg);
      Token t = peek_token(r);
      if(t.type == TCLOSE_PAREN) {
        break;
      } else if(t.type != TCOMMA) {
        error_unexpected_token(t);
      }
    }
  }


  Ast_Function_Call *n = (Ast_Function_Call *)allocate_ast_node_type(NODE_FUNCTION_CALL, sizeof(Ast_Function_Call));
  n->n.loc = open_paren.loc;
  assert(identifier->type == NODE_SYMBOL);
  n->identifier = identifier;
  n->arguments = args;
  n->return_type = UNKNOWN_TYPE;

  return (Ast_Node *)n;
}

// parses definitions, expressions, statements, declarations, blocks, etc.
Ast_Node *parse_any_statement(Token_Reader *r, Scope *scope, bool require_semicolon_for_expressions) {
  save_state(r);
  Token first = peek_token(r);

  if(first.type == TSYMBOL) { // could be decl, definition, or expression
    Token second = peek_token(r);

    if(second.type == TCOLON || second.type == TNUMBER_SIGN) {
      revert_state(r);
      return parse_decl(r, scope);
    } else if(second.type == TDOUBLE_COLON) {
      revert_state(r);
      return parse_definition(r, scope);
    }
  } else if(first.type == TSEMICOLON) {
    Ast_Node *n = allocate_ast_node_type(NODE_NULL, sizeof(Ast_Node));
    n->loc = first.loc;
    return n;
  } else if(first.type == TIMPORT) {
    Ast_Import *n = (Ast_Import *)allocate_ast_node_type(NODE_IMPORT, sizeof(Ast_Import));
    n->n.loc = first.loc;

    n->is_export = false;
    Token literal;
    while(true) {
      Token number_sign = peek_token(r);
      if(number_sign.type == TLITERAL_STRING) {
        literal = number_sign;
        break;
      } else if(number_sign.type != TNUMBER_SIGN)
        error_unexpected_token(number_sign);

      Token sym = peek_token(r);
      if(sym.type != TSYMBOL)
        error_unexpected_token(sym);

      symbol s = sym.data.symbol;
      if(s == st_get_id_of("export", -1)) {
        if(n->is_export) parse_error("I found the '#export' directive multiple times in this import.", sym.loc, true);
        n->is_export = true;
      } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
    }


    n->filename = literal.data.literal_string;

    return (Ast_Node *)n;
  } else if(first.type == TRETURN) {
    Ast_Return *n = (Ast_Return *)allocate_ast_node_type(NODE_RETURN, sizeof(Ast_Return));
    n->n.loc = first.loc;
    save_state(r);
    Token t = peek_token(r);
    if(t.type == TSEMICOLON) {
      n->is_return_nothing = true;
      return (Ast_Node *)n;
    }
    revert_state(r);
    n->is_return_nothing = false;
    n->value = parse_expression(r, scope, 0, NULL, NULL);
    expect_and_eat_semicolon(r);
    return (Ast_Node *)n;
  } else if(first.type == TWHILE) {
    {
      Token t = peek_token(r);
      if(t.type != TOPEN_PAREN) {
        print_error_message("The conditional for a while statement must be parenthesized.", t.loc);
        exit(1);
      }
    }
    Ast_While *n = (Ast_While *)allocate_ast_node_type(NODE_WHILE, sizeof(Ast_While));
    n->n.loc = first.loc;
    n->cond = parse_expression(r, scope, 0, NULL, NULL);
    {
      Token t = peek_token(r);
      if(t.type != TCLOSE_PAREN) error_unexpected_token(t);
    }
    n->body = parse_any_statement(r, scope, false);
    return (Ast_Node *)n;
  } else if(first.type == TEACH) {
    {
      Token t = peek_token(r);
      if(t.type != TOPEN_PAREN) {
        print_error_message("The iteration definition of an each statement must be parenthesized.", t.loc);
        exit(1);
      }
    }
    Ast_Each *n = (Ast_Each *)allocate_ast_node_type(NODE_EACH, sizeof(Ast_Each));
    n->n.loc = first.loc;
    n->element = st_get_id_of("it", -1);
    n->index = st_get_id_of("it_index", -1);

    {
      save_state(r);
      Token first_sym = peek_token(r);
      if(first_sym.type != TSYMBOL) {
        revert_state(r);
        goto each_vars_done;
      }

      Token comma = peek_token(r);
      if(comma.type == TCOLON) {
        n->element = first_sym.data.symbol;
        goto each_vars_done;
      } else if(comma.type != TCOMMA) {
        revert_state(r);
        goto each_vars_done;
      }

      Token second_sym = peek_token(r);
      if(second_sym.type != TSYMBOL) {
        error_unexpected_token(second_sym);
      }
      Token colon = peek_token(r);
      if(colon.type != TCOLON) {
        error_unexpected_token(colon);
      }

      n->element = first_sym.data.symbol;
      n->index = second_sym.data.symbol;
    }
    each_vars_done:

    n->collection = parse_expression(r, scope, 0, NULL, NULL);

    {
      Token t = peek_token(r);
      if(t.type != TCLOSE_PAREN) error_unexpected_token(t);
    }

    n->scope.type = REGISTER_SCOPE;
    n->scope.has_parent = true;
    n->scope.parent = scope;
    n->scope.entries = init_Scope_Entry_Array(2);
    {
      Scope_Entry e;
      e.symbol = n->element;
      e.data.reg.type = UNKNOWN_TYPE;
      Scope_Entry_Array_push(&n->scope.entries, e);
    }
    {
      Scope_Entry e;
      e.symbol = n->index;
      e.data.reg.type = UNKNOWN_TYPE;
      Scope_Entry_Array_push(&n->scope.entries, e);
    }

    n->body = parse_any_statement(r, &n->scope, false);
    return (Ast_Node *)n;
  }

  revert_state(r);
  bool needs_semicolon;
  Ast_Node *n = parse_expression(r, scope, 0, &needs_semicolon, NULL);
  if(needs_semicolon && require_semicolon_for_expressions) expect_and_eat_semicolon(r);
  return n;
}

Ast_Node *parse_decl(Token_Reader *r, Scope *scope) {
  Token id = peek_token(r);

  Token colon;
  bool is_export = false;
  while(true) {
    Token number_sign = peek_token(r);
    if(number_sign.type == TCOLON) {
      colon = number_sign;
      break;
    } else if(number_sign.type != TNUMBER_SIGN)
      error_unexpected_token(number_sign);

    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);

    symbol s = sym.data.symbol;

    if(s == st_get_id_of("export", -1)) {
      if(is_export) parse_error("I found the '#export' directive multiple times in this declaration.", sym.loc, true);
      is_export = true;
    } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
  }

  save_state(r);
  Token type = peek_token(r);

  if(type.type == TEQUALS) {
    Ast_Node *value_ast = parse_expression(r, scope, 0, NULL, NULL);
    expect_and_eat_semicolon(r);
    Ast_Untyped_Decl_Set *n = (Ast_Untyped_Decl_Set *)allocate_ast_node_type(NODE_UNTYPED_DECL_SET, sizeof(Ast_Untyped_Decl_Set));
    n->n.loc = colon.loc;
    n->symbol = id.data.symbol;
    n->is_export = is_export;
    n->value = value_ast;
    return (Ast_Node *)n;
  } else {
    revert_state(r);
    Ast_Node *type_ast = parse_expression(r, scope, 5, NULL, NULL);
    Token equals = peek_token(r);
    if(equals.type == TSEMICOLON) {
      // just decl
      Ast_Typed_Decl *n = (Ast_Typed_Decl *)allocate_ast_node_type(NODE_TYPED_DECL, sizeof(Ast_Typed_Decl));
      n->n.loc = colon.loc;
      n->symbol = id.data.symbol;
      n->type_node = type_ast;
      return (Ast_Node *)n;
    } else if(equals.type == TEQUALS) {
      // decl with type and set
      Ast_Node *value_ast = parse_expression(r, scope, 0, NULL, NULL);
      expect_and_eat_semicolon(r);
      Ast_Typed_Decl_Set *n = (Ast_Typed_Decl_Set *)allocate_ast_node_type(NODE_TYPED_DECL_SET, sizeof(Ast_Typed_Decl_Set));
      n->n.loc = colon.loc;
      n->symbol = id.data.symbol;
      n->is_export = is_export;
      n->type_node = type_ast;
      n->value = value_ast;
      return (Ast_Node *)n;
    } else {
      error_unexpected_token(equals);
    }
  }
}

Ast_Node *parse_block(Token_Reader *r, Scope *parent_scope) {
  Token first = peek_token(r);
  assert(first.type == TOPEN_BRACE && "(internal compiler error) block must begin with open brace");

  Ast_Block *result = (Ast_Block *)allocate_ast_node_type(NODE_BLOCK, sizeof(Ast_Block));
  result->n.loc = first.loc;
  Ast_Node_Ptr_Array *statements = &result->statements;
  *statements = init_Ast_Node_Ptr_Array(2);
  Scope *scope = &result->scope;
  scope->type = REGISTER_SCOPE;
  scope->has_parent = true;
  scope->parent = parent_scope;
  scope->entries = init_Scope_Entry_Array(2);

  while(1) {
    save_state(r);
    Token next = peek_token(r);
    if(next.type == TCLOSE_BRACE)
      return (Ast_Node *)result;
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
      e.data.reg.type = UNKNOWN_TYPE;
      Scope_Entry_Array_push(&scope->entries, e);
    } else if(node->type == NODE_FUNCTION_DEFINITION) {
      parse_error("Function definitions are only allowed in the global scope.", node->loc, false);
    }
  }
}

// uses_bind_symbol can be NULL, if you don't need that information.
Ast_Node_Ptr_Array parse_function_parameters(Token_Reader *r, Scope *bound_type_scope, Scope *argument_scope, bool *uses_bind_symbol) {
  Ast_Node_Ptr_Array parameters = init_Ast_Node_Ptr_Array(2);

  bool result_uses_bind_symbol = false;

  save_state(r);
  Token first = peek_token(r);
  if(first.type != TCLOSE_PAREN) {
    revert_state(r);
    while(1) {
      save_state(r);
      Token sym = peek_token(r);
      Token colon = peek_token(r);
      if(sym.type == TSYMBOL && colon.type == TCOLON) {
        Ast_Node *type;
        {
          bool this_uses_bind_symbol = false;
          type = parse_expression(r, bound_type_scope, 0, NULL, &this_uses_bind_symbol); // we are using the parent scope here because this should be not be able to use other arguments (at least for now...)
          if(this_uses_bind_symbol) result_uses_bind_symbol = true;
        }
        Ast_Passed_Parameter *param = (Ast_Passed_Parameter *)allocate_ast_node_type(NODE_PASSED_PARAMETER, sizeof(Ast_Passed_Parameter));
        param->n.loc = colon.loc;
        param->symbol = sym.data.symbol;
        param->type_node = type;
        param->type = UNKNOWN_TYPE;
        Ast_Node_Ptr_Array_push(&parameters, (Ast_Node *)param);

        Scope_Entry e;
        e.symbol = sym.data.symbol;
        e.data.reg.type = UNKNOWN_TYPE;
        Scope_Entry_Array_push(&argument_scope->entries, e);
      } else {
        revert_state(r);
        bool this_uses_bind_symbol = false;
        Ast_Node *expr = parse_expression(r, bound_type_scope, 0, NULL, &this_uses_bind_symbol); // we are using the parent scope here because this should be not be able to use other arguments (at least for now...)
        if(this_uses_bind_symbol) result_uses_bind_symbol = true;

        Ast_Matched_Parameter *param = (Ast_Matched_Parameter *)allocate_ast_node_type(NODE_MATCHED_PARAMETER, sizeof(Ast_Matched_Parameter));
        param->n.loc = colon.loc;
        param->node = expr;
        param->value = UNKNOWN_TYPE;
        Ast_Node_Ptr_Array_push(&parameters, (Ast_Node *)param);
      }
      Token next = peek_token(r);
      if(next.type == TCLOSE_PAREN) break;
      else if(next.type != TCOMMA) error_unexpected_token(next);
    }
  }
  if(uses_bind_symbol) *uses_bind_symbol = result_uses_bind_symbol;
  return parameters;
}

Ast_Node *parse_function_definition(Token_Reader *r, symbol identifier, Location loc, Scope *scope) {

  Ast_Function_Definition *n = (Ast_Function_Definition *)allocate_ast_node_type(NODE_FUNCTION_DEFINITION, sizeof(Ast_Function_Definition));
  // parse directives
  n->is_inline = false;
  n->is_operator = false;
  n->is_export = false;
  Token open_paren;
  while(true) {
    Token number_sign = peek_token(r);
    if(number_sign.type == TOPEN_PAREN) {
      open_paren = number_sign;
      break;
    } else if(number_sign.type != TNUMBER_SIGN)
      error_unexpected_token(number_sign);

    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);

    symbol s = sym.data.symbol;

    if(s == st_get_id_of("inline", -1)) {
      if(n->is_inline) parse_error("I found the '#inline' directive multiple times in this function definition.", sym.loc, true);
      n->is_inline = true;
    } else if(s == st_get_id_of("operator", -1)) {
      if(n->is_operator) parse_error("I found the '#operator' directive multiple times in this function definition.", sym.loc, true);
      n->is_operator = true;
    } else if(s == st_get_id_of("export", -1)) {
      if(n->is_export) parse_error("I found the '#export' directive multiple times in this function definition.", sym.loc, true);
      n->is_export = true;
    } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
  }

  n->n.loc = loc;
  n->bound_type_scope.type = SYMBOL_SCOPE;
  n->bound_type_scope.has_parent = true;
  n->bound_type_scope.parent = scope;
  n->bound_type_scope.entries = init_Scope_Entry_Array(2);
  n->parameter_scope.type = REGISTER_SCOPE;
  n->parameter_scope.has_parent = true;
  n->parameter_scope.parent = &n->bound_type_scope;
  n->parameter_scope.entries = init_Scope_Entry_Array(2);
  bool is_poly;
  n->parameters = parse_function_parameters(r, &n->bound_type_scope, &n->parameter_scope, &is_poly);
  if(is_poly) n->def_type = FN_POLYMORPHIC;
  else n->def_type = FN_SIMPLE;
  n->symbol = identifier;
  n->return_type = UNKNOWN_TYPE;

  save_state(r);
  Token arrow = peek_token(r);
  if(arrow.type == TARROW) {
    n->return_type_node = parse_expression(r, &n->bound_type_scope, 0, NULL, NULL);
  } else if(arrow.type == TOPEN_BRACE) {
    revert_state(r);
    Ast_Primitive_Type *type_node = (Ast_Primitive_Type *)allocate_ast_node_type(NODE_PRIMITIVE_TYPE, sizeof(Ast_Primitive_Type));
    type_node->n.loc = open_paren.loc;
    type_node->type = NOTHING_TYPE;
    n->return_type_node = (Ast_Node *)type_node;
  } else parse_error("Unexpected token, expected '->' or '{'.", arrow.loc, true);

  n->body = parse_block(r, &n->parameter_scope);

  return (Ast_Node *)n;
}

Ast_Node *parse_struct_definition(Token_Reader *r, symbol identifier, Location loc, Scope *scope) {

  Token open_brace;
  bool is_export = false;

  while(true) {
    Token number_sign = peek_token(r);
    if(number_sign.type == TOPEN_PAREN || number_sign.type == TOPEN_BRACE) {
      open_brace = number_sign;
      break;
    } else if(number_sign.type != TNUMBER_SIGN)
      error_unexpected_token(number_sign);

    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);

    symbol s = sym.data.symbol;

    if(s == st_get_id_of("export", -1)) {
      if(is_export) parse_error("I found the '#export' directive multiple times in this struct definition.", sym.loc, true);
      is_export = true;
    } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
  }

  if(open_brace.type == TOPEN_BRACE) {
    Ast_Struct_Definition *n = (Ast_Struct_Definition *)allocate_ast_node_type(NODE_STRUCT_DEFINITION, sizeof(Ast_Struct_Definition));
    n->n.loc = loc;
    n->symbol = identifier;
    n->is_export = is_export;
    n->type = UNKNOWN_TYPE;
    n->members = init_Compilation_Unit_Ptr_Array(2);

    while(1) {
      save_state(r);
      Token next = peek_token(r);
      if(next.type == TCLOSE_BRACE)
        return (Ast_Node *)n;
      else if(next.type == TEOL)
        parse_error("'{' has no matching '}'", open_brace.loc, true);
      revert_state(r);

      Ast_Node *node = parse_decl(r, scope);
      if(node->type == NODE_UNTYPED_DECL_SET)
        parse_error("Struct members must have an explicit type.", node->loc, true);
      assert(node->type == NODE_TYPED_DECL ||
             node->type == NODE_TYPED_DECL_SET);

      Compilation_Unit *unit = allocate_null_compilation_unit();
      unit->type = UNIT_STRUCT_MEMBER;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      unit->node = node;
      unit->scope = scope;
      unit->data.struct_member.type = UNKNOWN_TYPE;

      Compilation_Unit_Ptr_Array_push(&n->members, unit);
    }

    return (Ast_Node *)n;
  } else if(open_brace.type == TOPEN_PAREN) {
    Ast_Poly_Struct_Definition *n = (Ast_Poly_Struct_Definition *)allocate_ast_node_type(NODE_POLY_STRUCT_DEFINITION, sizeof(Ast_Poly_Struct_Definition));
    n->n.loc = loc;
    n->symbol = identifier;
    n->is_export = is_export;
    n->param_scope.type = BASIC_SCOPE;
    n->param_scope.has_parent = true;
    n->param_scope.parent = scope;
    n->param_scope.entries = init_Scope_Entry_Array(2);
    n->member_scope.type = SYMBOL_SCOPE;
    n->member_scope.has_parent = true;
    n->member_scope.parent = &n->param_scope;
    n->member_scope.entries = init_Scope_Entry_Array(2);
    n->parameters = parse_function_parameters(r, &n->param_scope, &n->param_scope, NULL);
    n->members = init_Ast_Node_Ptr_Array(2);
    Token open_brace = peek_token(r);
    if(open_brace.type != TOPEN_BRACE) error_unexpected_token(open_brace);

    while(1) {
      save_state(r);
      Token next = peek_token(r);
      if(next.type == TCLOSE_BRACE)
        return (Ast_Node *)n;
      else if(next.type == TEOL)
        parse_error("'{' has no matching '}'", open_brace.loc, true);
      revert_state(r);

      Ast_Node *node = parse_decl(r, scope);
      if(node->type == NODE_UNTYPED_DECL_SET)
        parse_error("Struct members must have an explicit type.", node->loc, true);
      assert(node->type == NODE_TYPED_DECL ||
             node->type == NODE_TYPED_DECL_SET);

      Ast_Node_Ptr_Array_push(&n->members, node);

      Scope_Entry e;
      switch(node->type) {
        case NODE_TYPED_DECL: e.symbol = ((Ast_Typed_Decl *)node)->symbol; break;
        case NODE_TYPED_DECL_SET: e.symbol = ((Ast_Typed_Decl_Set *)node)->symbol; break;
        case NODE_UNTYPED_DECL_SET: e.symbol = ((Ast_Untyped_Decl_Set *)node)->symbol; break;
      }
      e.data.struct_.node = node;
      e.data.struct_.param_index = n->member_scope.entries.length;
      Scope_Entry_Array_push(&n->member_scope.entries, e);
    }
    return (Ast_Node *)n;
  } else error_unexpected_token(open_brace);
}

Ast_Node *parse_foreign_definition(Token_Reader *r, symbol identifier, Location loc, Scope *scope) {
  Token def_type = peek_token(r);
  if(def_type.type != TFN) error_unexpected_token(def_type);

  Ast_Foreign_Definition *n = (Ast_Foreign_Definition *)allocate_ast_node_type(NODE_FOREIGN_DEFINITION, sizeof(Ast_Foreign_Definition));
  n->n.loc = loc;
  n->symbol = identifier;

  Token open_paren;
  n->is_export = false;

  while(true) {
    Token number_sign = peek_token(r);
    if(number_sign.type == TOPEN_PAREN) {
      open_paren = number_sign;
      break;
    } else if(number_sign.type != TNUMBER_SIGN)
      error_unexpected_token(number_sign);

    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);

    symbol s = sym.data.symbol;

    if(s == st_get_id_of("export", -1)) {
      if(n->is_export) parse_error("I found the '#export' directive multiple times in this function definition.", sym.loc, true);
      n->is_export = true;
    } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
  }

  if(open_paren.type != TOPEN_PAREN) error_unexpected_token(open_paren);

  n->parameters = init_Ast_Node_Ptr_Array(2);
  n->parameter_types = init_Type_Array(2);

  save_state(r);
  Token close_paren = peek_token(r);
  if(close_paren.type != TCLOSE_PAREN) {
    revert_state(r);
    while(true) {
      Ast_Node_Ptr_Array_push(&n->parameters, parse_expression(r, scope, 0, NULL, NULL));
      Type_Array_push(&n->parameter_types, UNKNOWN_TYPE);
      Token comma = peek_token(r);
      if(comma.type == TCOMMA) continue;
      else if(comma.type == TCLOSE_PAREN) {
        close_paren = comma;
        break;
      } else error_unexpected_token(comma);
    }
  }

  save_state(r);
  Token arrow = peek_token(r);
  if(arrow.type == TARROW) {
    n->return_type_node = parse_expression(r, scope, 0, NULL, NULL);
  } else {
    revert_state(r);
    Ast_Primitive_Type *type_node = (Ast_Primitive_Type *)allocate_ast_node_type(NODE_PRIMITIVE_TYPE, sizeof(Ast_Primitive_Type));
    type_node->n.loc = open_paren.loc;
    type_node->type = NOTHING_TYPE;
    n->return_type_node = (Ast_Node *)type_node;
  }

  n->return_type = UNKNOWN_TYPE;

  return (Ast_Node *)n;
}

Ast_Node *parse_unique_definition(Token_Reader *r, symbol identifier, Location loc, Scope *scope) {
  bool is_export = false;

  while(true) {
    save_state(r);
    Token number_sign = peek_token(r);
    if(number_sign.type != TNUMBER_SIGN) {
      revert_state(r);
      break;
    }

    Token sym = peek_token(r);
    if(sym.type != TSYMBOL)
      error_unexpected_token(sym);

    symbol s = sym.data.symbol;

    if(s == st_get_id_of("export", -1)) {
      if(is_export) parse_error("I found the '#export' directive multiple times in this unique definition.", sym.loc, true);
      is_export = true;
    } else parse_error("I do not recognize this compiler directive.", sym.loc, true);
  }

  Ast_Unique_Definition *n = (Ast_Unique_Definition *)allocate_ast_node_type(NODE_UNIQUE_DEFINITION, sizeof(Ast_Unique_Definition));
  n->symbol = identifier;
  n->is_export = is_export;
  n->node = parse_expression(r, scope, 0, NULL, NULL);
  return (Ast_Node *)n;
}

Ast_Node *parse_definition(Token_Reader *r, Scope *scope) {
  Token identifier = peek_token(r);
  Token double_colon = peek_token(r);
  assert(identifier.type == TSYMBOL && double_colon.type == TDOUBLE_COLON && "(internal compiler error) definition must begin with symbol followed by double colon");
  Token def_type = peek_token(r);
  if(def_type.type == TFN) {
    return parse_function_definition(r, identifier.data.symbol, double_colon.loc, scope);
  } else if(def_type.type == TSTRUCT) {
    return parse_struct_definition(r, identifier.data.symbol, double_colon.loc, scope);
  } else if(def_type.type == TFOREIGN) {
    return parse_foreign_definition(r, identifier.data.symbol, double_colon.loc, scope);
  } else if(def_type.type == TUNIQUE) {
    return parse_unique_definition(r, identifier.data.symbol, double_colon.loc, scope);
  } else parse_error("I expected 'struct' or 'fn'.", def_type.loc, true);
}




Compilation_Unit *parse_file(int file_id) {
  assert(files.length > file_id);
  File_Data *file_data = files.data + file_id;
  if(file_data->parsed) return file_data->module;
  else file_data->module = allocate_null_compilation_unit();

  Compilation_Unit *result = file_data->module;
  result->type = UNIT_MODULE;
  result->type_inferred = false;
  result->type_inference_seen = false;
  result->bytecode_generated = false;
  result->bytecode_generating = false;
  result->poisoned = false;
  result->data.module.file_id = file_id;
  result->data.module.compilation_units = init_Compilation_Unit_Ptr_Array(32);
  result->data.module.scope.type = UNIT_SCOPE;
  result->data.module.scope.has_parent = false;
  result->data.module.scope.entries = init_Scope_Entry_Array(2);

  file_data->tokens = lex_string(file_data->contents, file_id);
  Token_Reader r = (Token_Reader){file_data->tokens, 0, 0};

  while(1) {
    save_state(&r);
    Token next = peek_token(&r);
    revert_state(&r);
    if(next.type == TEOL) {
      file_data->parsed = true;
      return result;
    }

    Ast_Node *node = parse_any_statement(&r, &result->data.module.scope, true);
    Compilation_Unit *declaration_unit; // to be referred to in the corresponding Scope_Entry
    if(node->type == NODE_FUNCTION_DEFINITION) {

      Ast_Function_Definition *n = (Ast_Function_Definition *)node;
      if(n->def_type == FN_POLYMORPHIC) {
        Compilation_Unit *fn = allocate_null_compilation_unit();
        fn->type = UNIT_POLY_FUNCTION;
        fn->type_inferred = false;
        fn->type_inference_seen = false;
        fn->bytecode_generated = false;
        fn->bytecode_generating = false;
        fn->poisoned = false;
        fn->is_export = n->is_export;
        fn->node = node;
        fn->scope = &result->data.module.scope;
        fn->data.poly_function_def.instances = init_Compilation_Unit_Ptr_Table();
        fn->data.poly_function_def.current_instance_id = 0;
        Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, fn);

        declaration_unit = fn;
      } else if(n->def_type == FN_SIMPLE) {
        // A function is represented by a signature and body which must be
        // type-checked separately. See more explanation in ast.h for Compilation_Unit.
        Compilation_Unit *sig = allocate_null_compilation_unit();
        Compilation_Unit *body = allocate_null_compilation_unit();

        sig->type = UNIT_FUNCTION_SIGNATURE;
        sig->type_inferred = false;
        sig->type_inference_seen = false;
        sig->bytecode_generated = false;
        sig->bytecode_generating = false;
        sig->poisoned = false;
        sig->is_export = n->is_export;
        sig->node = node;
        sig->scope = &result->data.module.scope;
        sig->data.signature.body = body;
        Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, sig);

        body->type = UNIT_FUNCTION_BODY;
        body->type_inferred = false;
        body->type_inference_seen = false;
        body->bytecode_generated = false;
        body->bytecode_generating = false;
        body->poisoned = false;
        body->is_export = n->is_export;
        body->node = node;
        body->scope = &result->data.module.scope;
        body->data.body.signature = sig;
        body->data.body.bytecode = allocate_bytecode_unit_type(BYTECODE_FUNCTION, sizeof(Bytecode_Function));

        Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, body);

        declaration_unit = sig;
      } else assert(false);
    } else if(node->type == NODE_FOREIGN_DEFINITION) {
      Ast_Foreign_Definition *n = (Ast_Foreign_Definition *)node;

      Compilation_Unit *sig = allocate_null_compilation_unit();
      sig->type = UNIT_FOREIGN_FUNCTION;
      sig->type_inferred = false;
      sig->type_inference_seen = false;
      sig->bytecode_generated = false;
      sig->bytecode_generating = false;
      sig->poisoned = false;
      sig->is_export = n->is_export;
      sig->node = node;
      sig->scope = &result->data.module.scope;
      sig->data.foreign.bytecode = allocate_bytecode_unit_type(BYTECODE_FOREIGN_FUNCTION, sizeof(Bytecode_Foreign_Function));
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, sig);

      declaration_unit = sig;
    } else if(node->type == NODE_STRUCT_DEFINITION) {
      Ast_Struct_Definition *n = (Ast_Struct_Definition *)node;
      Compilation_Unit *unit = allocate_null_compilation_unit();

      unit->type = UNIT_STRUCT;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      unit->is_export = n->is_export;
      unit->node = node;
      unit->scope = &result->data.module.scope;
      unit->data.struct_def.type = UNKNOWN_TYPE;
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, unit);

      declaration_unit = unit;
    } else if(node->type == NODE_POLY_STRUCT_DEFINITION) {
      Ast_Poly_Struct_Definition *n = (Ast_Poly_Struct_Definition *)node;
      Compilation_Unit *unit = allocate_null_compilation_unit();

      unit->type = UNIT_POLY_STRUCT;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      unit->is_export = n->is_export;
      unit->node = node;
      unit->scope = &result->data.module.scope;
      unit->data.poly_struct_def.instances = init_Type_Table();
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, unit);

      declaration_unit = unit;
    } else if(node->type == NODE_IMPORT) {
      Ast_Import *n = (Ast_Import *)node;
      Compilation_Unit *unit = allocate_null_compilation_unit();

      unit->type = UNIT_IMPORT;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      unit->is_export = n->is_export;
      unit->node = node;
      unit->scope = &result->data.module.scope;
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, unit);

      declaration_unit = unit;
    } else if(node->type == NODE_TYPED_DECL_SET || node->type == NODE_UNTYPED_DECL_SET) {
      Compilation_Unit *unit = allocate_null_compilation_unit();
      unit->type = UNIT_CONSTANT;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      if(node->type == NODE_TYPED_DECL_SET)
        unit->is_export = ((Ast_Typed_Decl_Set *)node)->is_export;
      else if(node->type == NODE_UNTYPED_DECL_SET)
        unit->is_export = ((Ast_Untyped_Decl_Set *)node)->is_export;
      else assert(false);
      unit->node = node;
      unit->scope = &result->data.module.scope;
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, unit);

      declaration_unit = unit;
    } else if(node->type == NODE_UNIQUE_DEFINITION) {
      Ast_Unique_Definition *n = (Ast_Unique_Definition *)node;

      Compilation_Unit *unit = allocate_null_compilation_unit();
      unit->type = UNIT_UNIQUE;
      unit->type_inferred = false;
      unit->type_inference_seen = false;
      unit->bytecode_generated = false;
      unit->bytecode_generating = false;
      unit->poisoned = false;
      unit->is_export = n->is_export;
      unit->node = node;
      unit->scope = &result->data.module.scope;
      Compilation_Unit_Ptr_Array_push(&result->data.module.compilation_units, unit);

      declaration_unit = unit;
    } else if(node->type == NODE_NULL) {
      // do nothing
    } else {
      print_ast_node(node);
      assert(false);
    }

    if(node->type == NODE_TYPED_DECL ||
       node->type == NODE_TYPED_DECL_SET ||
       node->type == NODE_UNTYPED_DECL_SET ||
       node->type == NODE_FUNCTION_DEFINITION ||
       node->type == NODE_STRUCT_DEFINITION ||
       node->type == NODE_POLY_STRUCT_DEFINITION ||
       node->type == NODE_FOREIGN_DEFINITION ||
       node->type == NODE_UNIQUE_DEFINITION ||
       node->type == NODE_IMPORT) {
      Scope_Entry e;
      switch(node->type) {
        case NODE_TYPED_DECL: e.symbol = ((Ast_Typed_Decl *)node)->symbol; break;
        case NODE_TYPED_DECL_SET: e.symbol = ((Ast_Typed_Decl_Set *)node)->symbol; break;
        case NODE_UNTYPED_DECL_SET: e.symbol = ((Ast_Untyped_Decl_Set *)node)->symbol; break;
        case NODE_FUNCTION_DEFINITION: e.symbol = ((Ast_Function_Definition *)node)->symbol; break;
        case NODE_STRUCT_DEFINITION: e.symbol = ((Ast_Struct_Definition *)node)->symbol; break;
        case NODE_POLY_STRUCT_DEFINITION: e.symbol = ((Ast_Poly_Struct_Definition *)node)->symbol; break;
        case NODE_FOREIGN_DEFINITION: e.symbol = ((Ast_Foreign_Definition *)node)->symbol; break;
        case NODE_UNIQUE_DEFINITION: e.symbol = ((Ast_Unique_Definition *)node)->symbol; break;
        case NODE_IMPORT: e.symbol = st_get_id_of("*", -1); break;
      }
      e.data.unit.unit = declaration_unit;
      Scope_Entry_Array_push(&result->data.module.scope.entries, e);
    }
  }
}
