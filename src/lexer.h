#ifndef LEXER_H
#define LEXER_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "errors.h"

typedef enum Token_Type {
  TEOL = 0,
  TSYMBOL = 1,

  TOPEN_PAREN,
  TCLOSE_PAREN,
  TOPEN_BRACE,
  TCLOSE_BRACE,
  TOPEN_BRACKET,
  TCLOSE_BRACKET,

  TPLUS,
  TMINUS,
  TASTERISK,
  TFORWARD_SLASH,
  TMODULUS,

  TDOUBLE_PLUS,
  TDOUBLE_MINUS,

  TAMPERSAND,

  TOR,
  TXOR,
  TTILDA,
  TDOUBLE_OR,
  TDOUBLE_XOR,
  TDOUBLE_AMPERSAND,
  TNOT,
  TDOUBLE_EQUALS,
  TNOT_EQUALS,
  TLESS_THAN,
  TLESS_THAN_OR_EQUAL_TO,
  TGREATER_THAN,
  TGREATER_THAN_OR_EQUAL_TO,


  TEQUALS,
  TPLUS_EQUALS,
  TMINUS_EQUALS,
  // might be more similar operators that should be implemented

  TQUESTION_MARK,
  TCOLON,
  TDOUBLE_COLON,
  TARROW,
  TSEMICOLON,
  TDOT,
  TCOMMA,
  TDOLLAR_SIGN,

  TFN,
  TRETURN,
  TIF,
  TELSE,

  TLITERAL_INT,
  TLITERAL_BOOL,
  TLITERAL_STRING
} Token_Type;

typedef struct Token {
  Location loc;
  Token_Type type;

  union {
    u64 symbol;
    int literal_int;
    bool literal_bool;
    char *literal_string;
  } data;
} Token;

void print_token(Token token);

typedef struct Lexer_State {
  int i;
  int comment_level; // allows for nested /* and */
  Location loc;
  bool line_is_commented; // have we seen a '//' comment on this line already?
  bool finished;
  char *to_lex;
} Lexer_State;

typedef struct Lexer {
  Lexer_State saved_state;
  Lexer_State s;
} Lexer; // stack (darray) of Lexer_States defined in lexer.c

/*
  @Incomplete TODO: Add big lexer explanation comment;
*/

// to_lex must be zero-terminated and have at least one character of whitespace at the end
Lexer new_lexer(char *to_lex, int file_id);
Token peek_token(Lexer *l);
void save_state(Lexer *l);
void revert_state(Lexer *l);

#endif /* end of include guard: LEXER_H */
