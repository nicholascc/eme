#include "lexer.h"

// Greatly modified from EME programming language design and compiler v1

#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"

GENERATE_DARRAY_CODE(Token, Token_Array);

void print_token(Token t) {
  printf("Token at file %i, line %i, and character %i: ", t.loc.file_id, t.loc.line, t.loc.character);
  if(t.type == TEOL) {
    printf("EOL\n");
    return;
  } else if(t.type == TSYMBOL) {
    printf("SYM(%s)\n", st_get_str_of(t.data.symbol));
    return;
  } else if(t.type == TLITERAL_INT) {
    printf("LITERAL_INT(%i)\n", t.data.literal_int);
    return;
  } else if(t.type == TLITERAL_BOOL) {
    printf("LITERAL_BOOL(%s)\n", t.data.literal_bool ? "true" : "false");
    return;
  }


  printf("STX(\"");
  switch(t.type) {
    case TOPEN_PAREN:    printf("("); break;
    case TCLOSE_PAREN:   printf(")"); break;
    case TOPEN_BRACE:    printf("{"); break;
    case TCLOSE_BRACE:   printf("}"); break;
    case TOPEN_BRACKET:  printf("["); break;
    case TCLOSE_BRACKET: printf("]"); break;

    case TPLUS:            printf("+"); break;
    case TMINUS:           printf("-"); break;
    case TASTERISK:        printf("*"); break;
    case TFORWARD_SLASH:   printf("/"); break;
    case TMODULUS:         printf("%%"); break;

    case TDOUBLE_PLUS:  printf("++"); break;
    case TDOUBLE_MINUS: printf("--"); break;

    case TAMPERSAND:     printf("&"); break;

    case TOR:               printf("|"); break;
    case TCARET:              printf("^"); break;
    case TTILDA:            printf("~"); break;
    case TDOUBLE_OR:        printf("||"); break;
    case TDOUBLE_AMPERSAND: printf("&&"); break;
    case TNOT:              printf("!"); break;

    case TDOUBLE_EQUALS:            printf("=="); break;
    case TNOT_EQUALS:               printf("!="); break;
    case TLESS_THAN:                printf("<"); break;
    case TLESS_THAN_OR_EQUAL_TO:    printf("<="); break;
    case TGREATER_THAN:             printf(">"); break;
    case TGREATER_THAN_OR_EQUAL_TO: printf(">="); break;

    case TEQUALS:       printf("="); break;
    case TPLUS_EQUALS:  printf("+="); break;
    case TMINUS_EQUALS: printf("-="); break;

    case TQUESTION_MARK: printf("?"); break;
    case TCOLON:         printf(":"); break;
    case TDOUBLE_COLON:  printf("::"); break;
    case TARROW:         printf("->"); break;
    case TSEMICOLON:     printf(";"); break;
    case TDOT:           printf("."); break;
    case TDOT_CARET:     printf(".^"); break;
    case TCOMMA:         printf(","); break;
    case TDOLLAR_SIGN:   printf("$"); break;
    case TNUMBER_SIGN:   printf("#"); break;

    case TFN:      printf("fn"); break;
    case TSTRUCT:  printf("struct"); break;
    case TFOREIGN: printf("foreign"); break;
    case TRETURN:  printf("return"); break;
    case TIF:      printf("if"); break;
    case TELSE:    printf("else"); break;
    case TWHILE:   printf("while"); break;
    default: printf("unknown: %i", t.type); break;
  }
  printf("\")\n");
}

void print_token_array(Token_Array tokens) {
  for(int i = 0; i < tokens.length; i++) {
    print_token(tokens.data[i]);
  }
}

Token eol_token(Location l) {
  Token t;
  t.loc.character = -1;
  t.loc.line = l.line;
  t.loc.file_id = l.file_id;
  t.type = TEOL;
  return t;
}

void save_state(Token_Reader *r) {
  r->saved = r->current;
}

void revert_state(Token_Reader *r) {
  r->current = r->saved;
}

Token peek_token(Token_Reader *r) {
  return r->tokens.data[r->current++];
}

bool is_valid_symbol_start_char(char c) {
  return isalpha(c) || c == '_';
}

bool is_valid_symbol_char(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

Token_Array lex_string(char *to_lex, int file_id) {

  int i = -1;
  int comment_level = 0; // allows for nested /* and */
  Location loc;
  loc.line = 1;
  loc.character = 0;
  loc.file_id = file_id;
  bool line_is_commented = false; // have we seen a '//' comment on this line already?
  bool finished = false;

  Token_Array tokens = init_Token_Array(2);

  char c;
  while(c = to_lex[++i]) {
    if(c == '\n') {
      loc.character = 0;
      loc.line++;
      line_is_commented = false;
      continue;
    }
    loc.character++;

    // HANDLE UNICODE
    // All we're going to do here is read through unicode characters. If they are some sort of apostrophe or quotation mark we convert them to their ASCII form, and if they're unrecognized we just print an error and skip them.
    // If the most significant bit is 0, this is already in 7-bit ASCII.
    // Otherwise, it's a multi-byte unicode character.
    if(c & (1 << 7)) {
      char utf8_ch1 = c;

      if((utf8_ch1 & 0xe0) == 0xc0) {
        // if the three most significant bits are 110 then the unicode character is 2 utf-8 characters wide.
        char utf8_ch2 = to_lex[++i];

        char ub1 =  (utf8_ch1 << 3) & 0xe0;
        char ub2 = ((utf8_ch1 << 6) & 0xc0) | (utf8_ch2 & 0x3f);
        u16  u_char = (((u16)ub1)<<8) | ((u16)ub2);

        switch(u_char) {
          case 0x00B4: c = '\''; break; //	´ - ACUTE ACCENT
          default:
            print_error(loc);
            printf("Unrecognized unicode character.\n");
            continue;
        }

      } else if((utf8_ch1 & 0xf0) == 0xe0) {
        // if the four most significant bits are 1110 then the unicode character is 3 utf-8 characters wide.
        char utf8_ch2 = to_lex[++i];
        char utf8_ch3 = to_lex[++i];

        char ub1 = ((utf8_ch1 << 4) & 0xf0) | ((utf8_ch2 >> 2) & 0x0f);
        char ub2 = ((utf8_ch2 << 6) & 0xc0) | (utf8_ch3 & 0x3f);
        u16  u_char = (((u16)ub1)<<8) | ((u16)ub2);

        switch(u_char) {
          case 0x2018:                  // ‘	- LEFT SINGLE QUOTATION MARK
          case 0x2019: c = '\''; break; // ’	- RIGHT SINGLE QUOTATION MARK
          case 0x201C:                  // “	- LEFT DOUBLE QUOTATION MARK
          case 0x201D:                  // ”	- RIGHT DOUBLE QUOTATION MARK
          case 0x201F:                  // ‟  - DOUBLE HIGH-REVERSED-9 QUOTATION MARK
          case 0x275D:                  // ❝  - HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT
          case 0x275E: c = '"' ; break; // ❞  - HEAVY DOUBLE COMMA QUOTATION MARK ORNAMENT
          default:
            print_error(loc);
            printf("Unrecognized unicode character.\n");
            continue;
        }

      } else {
        // if the five most significant bits are 11110 then the unicode character is 4 utf-8 characters wide.
        // we don't recognize any of these characters (for now, at least)
        print_error(loc);
        printf("Unrecognized unicode character.\n");
        continue;
      }
    }
    // Finished handling unicode, now onto actually processing the character.



    if(isspace(c)) continue;

    // handle comments
    if(c == '/') {
      if(to_lex[i+1] == '*') {
        comment_level++;
        i++;
        loc.character++;
        continue;
      } else if(to_lex[i+1] == '/') {
        line_is_commented = true;
        i++;
        loc.character++;
        continue;
      }
    } else if(c == '*' && to_lex[i+1] == '/') {
      comment_level--;
      i++;
      loc.character++;
      if(comment_level < 0) {
        print_error(loc);
        printf("Unexpected comment end, ignoring.\n");
      }
      continue;
    }

    if(comment_level > 0 || line_is_commented)
      continue;

    // finally, actually generate tokens
    if(is_valid_symbol_start_char(c)) {
      int len = 0;
      while(is_valid_symbol_char(to_lex[i + (++len)])) {};
      char *symbol_str = to_lex + i;

      Token t;
      t.loc = loc;

      if(len == 2 && 0 == memcmp(symbol_str, "fn", 2)) {
        t.type = TFN;
      } else if(len == 2 && 0 == memcmp(symbol_str, "if", 2)) {
        t.type = TIF;
      } else if(len == 4 && 0 == memcmp(symbol_str, "else", 4)) {
        t.type = TELSE;
      } else if(len == 6 && 0 == memcmp(symbol_str, "return", 6)) {
        t.type = TRETURN;
      } else if(len == 6 && 0 == memcmp(symbol_str, "struct", 6)) {
        t.type = TSTRUCT;
      } else if(len == 7 && 0 == memcmp(symbol_str, "foreign", 7)) {
        t.type = TFOREIGN;
      } else if(len == 5 && 0 == memcmp(symbol_str, "while", 5)) {
        t.type = TWHILE;
      } else if(len == 4 && 0 == memcmp(symbol_str, "true", 4)) {
        t.type = TLITERAL_BOOL;
        t.data.literal_bool = true;
      } else if(len == 5 && 0 == memcmp(symbol_str, "false", 5)) {
        t.type = TLITERAL_BOOL;
        t.data.literal_bool = false;
      } else {
        t.data.symbol = st_get_id_of(symbol_str, len);
        t.type = TSYMBOL;
      }

      i = i + len-1;
      loc.character += len-1;
      Token_Array_push(&tokens, t);
    } else if(isdigit(c)) {
      int len = 0;
      while(isdigit(to_lex[i + (++len)]));
      char *str = to_lex + i;
      int value = atoi(str);

      Token t;
      t.loc = loc;
      t.type = TLITERAL_INT;
      t.data.literal_int = value;
      i += len-1;
      loc.character += len-1;
      Token_Array_push(&tokens, t);
    } else { // some syntax token

      Token t;
      t.loc = loc;

      char next_c = to_lex[i+1];

      switch(c) {
        case '(': t.type = TOPEN_PAREN; break;
        case ')': t.type = TCLOSE_PAREN; break;
        case '{': t.type = TOPEN_BRACE; break;
        case '}': t.type = TCLOSE_BRACE; break;
        case '[': t.type = TOPEN_BRACKET; break;
        case ']': t.type = TCLOSE_BRACKET; break;

        case '+': if(next_c == '=') {
                    t.type = TPLUS_EQUALS;
                    i++;
                    loc.character++;
                    break;
                  } else if(next_c == '+') {
                    t.type = TDOUBLE_PLUS;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TPLUS;
                    break;
                  }
        case '-': if(next_c == '>') {
                    t.type = TARROW;
                    i++;
                    loc.character++;
                    break;
                  } else if(next_c == '=') {
                    t.type = TMINUS_EQUALS;
                    i++;
                    loc.character++;
                    break;
                  } else if(next_c == '-') {
                    t.type = TDOUBLE_MINUS;
                    i++;
                    loc.character++;
                    break;
                  }else {
                    t.type = TMINUS;
                    break;
                  }
        case '*': t.type = TASTERISK; break;
        case '/': t.type = TFORWARD_SLASH; break;
        case '%': t.type = TMODULUS; break;

        case '&': if(next_c == '&') {
                    t.type = TDOUBLE_AMPERSAND;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TAMPERSAND;
                    break;
                  }
        case '|': if(next_c == '|') {
                    t.type = TDOUBLE_OR;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TOR;
                    break;
                  }
        case '^': t.type = TCARET; break;
        case '~': t.type = TTILDA; break;
        case '!': if(next_c == '=') {
                    t.type = TNOT_EQUALS;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TNOT;
                    break;
                  }
        case '<': if(next_c == '=') {
                    t.type = TLESS_THAN_OR_EQUAL_TO;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TLESS_THAN;
                    break;
                  }
        case '>': if(next_c == '=') {
                    t.type = TGREATER_THAN_OR_EQUAL_TO;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TGREATER_THAN;
                    break;
                  }


        case '=': if(next_c == '=') {
                    t.type = TDOUBLE_EQUALS;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TEQUALS;
                    break;
                  }

        case '?': t.type = TQUESTION_MARK; break;

        case ':': if(next_c == ':') {
                    t.type = TDOUBLE_COLON;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TCOLON;
                    break;
                  }

        case ';': t.type = TSEMICOLON; break;
        case '.': if(next_c == '^') {
                    t.type = TDOT_CARET;
                    i++;
                    loc.character++;
                    break;
                  } else {
                    t.type = TDOT;
                    break;
                  }
        case ',': t.type = TCOMMA; break;
        case '$': t.type = TDOLLAR_SIGN; break;
        case '#': t.type = TNUMBER_SIGN; break;

        default: print_error(loc);
                 printf("Unrecognized special character.\n");
      }

      Token_Array_push(&tokens, t);
    }

  }

  Token_Array_push(&tokens, eol_token(loc));

  return tokens;
}
