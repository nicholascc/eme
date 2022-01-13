#ifndef LEXER_H
#define LEXER_H

#include <stdbool.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "errors.h"
#include "symbol_table.h"

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
  TSTRUCT,
  TRETURN,
  TIF,
  TELSE,
  TWHILE,

  TLITERAL_INT,
  TLITERAL_BOOL,
  TLITERAL_STRING
} Token_Type;

typedef struct Token {
  Location loc;
  Token_Type type;

  union {
    symbol symbol;
    int literal_int;
    bool literal_bool;
    char *literal_string;
  } data;
} Token;

GENERATE_DARRAY_HEADER(Token, Token_Array);

void print_token(Token token);
void print_token_array(Token_Array tokens);


typedef struct Token_Reader {
  Token_Array tokens;
  u32 current;
  u32 saved;
} Token_Reader;

/*
  @Incomplete TODO: Add big lexer explanation comment;
*/

// to_lex must be zero-terminated and have at least one character of whitespace at the end
Token_Array lex_string(char *to_lex, int file_id);
void save_state(Token_Reader *r);
void revert_state(Token_Reader *r);
Token peek_token(Token_Reader *r);

#endif /* end of include guard: LEXER_H */
