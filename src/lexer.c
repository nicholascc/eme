#include "lexer.h"

// Greatly modified from EME programming language design and compiler v1

#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

#include "c-utils/integer.h"
#include "c-utils/darray.h"
#include "symbol_table.h"
#include "errors.h"


void print_token(Token t) {
  printf("Token at file %i, line %i, and character %i: ", t.loc.file_id, t.loc.line, t.loc.character);
  if(t.type == TEOL) {
    printf("EOL\n");
    return;
  } else if(t.type == TSYMBOL) {
    printf("SYM(%llu,\"",t.data.symbol);
    int length;
    char *str = st_get_str_of(t.data.symbol, &length);
    for(int i = 0; i < length; i++)
      printf("%c", str[i]);
    printf("\")\n");
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
    case TXOR:              printf("^"); break;
    case TTILDA:            printf("~"); break;
    case TDOUBLE_OR:        printf("||"); break;
    case TDOUBLE_XOR:       printf("^^"); break;
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
    case TCOMMA:         printf(","); break;
    case TDOLLAR_SIGN:   printf("$"); break;

    case TFN:     printf("fn"); break;
    case TRETURN: printf("return"); break;
    default: printf("unknown"); break;
  }
  printf("\")\n");
}

Token eol_token(Location l) {
  Token t;
  t.loc.character = -1;
  t.loc.line = l.line;
  t.loc.file_id = l.file_id;
  t.type = TEOL;
  return t;
}
// to_lex must be zero-terminated and have at least one character of whitespace at the end
Lexer new_lexer(char *to_lex, int file_id) {
  Lexer_State s;
  s.i = -1;
  s.loc.line = 1;
  s.loc.character = 0;
  s.loc.file_id = file_id;
  s.to_lex = to_lex;
  s.comment_level = 0;
  s.line_is_commented = false;
  s.finished = false;
  Lexer l;
  l.s = s;
  l.saved_state = s;
  return l;
}

void save_state(Lexer *l) {
  l->saved_state = l->s;
}

void revert_state(Lexer *l) {
  l->s = l->saved_state;
}

bool is_valid_symbol_start_char(char c) {
  return isalpha(c) || c == '_';
}

bool is_valid_symbol_char(char c) {
  return isalpha(c) || isdigit(c) || c == '_';
}

Token peek_token(Lexer *l) {

  char c;

  assert(!l->s.finished && "trying to get token but lexer is already finished");


  while(c = l->s.to_lex[++l->s.i]) {
    if(c == '\n') {
      l->s.loc.character = 0;
      l->s.loc.line++;
      l->s.line_is_commented = false;
      continue;
    }
    l->s.loc.character++;

    // HANDLE UNICODE
    // All we're going to do here is read through unicode characters. If they are some sort of apostrophe or quotation mark we convert them to their ASCII form, and if they're unrecognized we just print an error and skip them.
    // If the most significant bit is 0, this is already in 7-bit ASCII.
    // Otherwise, it's a multi-byte unicode character.
    if(c & (1 << 7)) {
      char utf8_ch1 = c;

      if((utf8_ch1 & 0xe0) == 0xc0) {
        // if the three most significant bits are 110 then the unicode character is 2 utf-8 characters wide.
        char utf8_ch2 = l->s.to_lex[++l->s.i];

        char ub1 =  (utf8_ch1 << 3) & 0xe0;
        char ub2 = ((utf8_ch1 << 6) & 0xc0) | (utf8_ch2 & 0x3f);
        u16  u_char = (((u16)ub1)<<8) | ((u16)ub2);

        switch(u_char) {
          case 0x00B4: c = '\''; break; //	´ - ACUTE ACCENT
          default:
            print_error(l->s.loc);
            printf("Unrecognized unicode character.\n");
            continue;
        }

      } else if((utf8_ch1 & 0xf0) == 0xe0) {
        // if the four most significant bits are 1110 then the unicode character is 3 utf-8 characters wide.
        char utf8_ch2 = l->s.to_lex[++l->s.i];
        char utf8_ch3 = l->s.to_lex[++l->s.i];

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
            print_error(l->s.loc);
            printf("Unrecognized unicode character.\n");
            continue;
        }

      } else {
        // if the five most significant bits are 11110 then the unicode character is 4 utf-8 characters wide.
        // we don't recognize any of these characters (for now, at least)
        print_error(l->s.loc);
        printf("Unrecognized unicode character.\n");
        continue;
      }
    }
    // Finished handling unicode, now onto actually processing the character.



    if(isspace(c)) continue;

    // handle comments
    if(c == '/') {
      if(l->s.to_lex[l->s.i+1] == '*')
        l->s.comment_level++;
      else if(l->s.to_lex[l->s.i+1] == '/')
        l->s.line_is_commented = true;
      l->s.i++;
      l->s.loc.character++;
      continue;
    } else if(c == '*' && l->s.to_lex[l->s.i+1] == '/') {
      l->s.comment_level--;
      l->s.i++;
      l->s.loc.character++;
      if(l->s.comment_level < 0) {
        print_error(l->s.loc);
        printf("Unexpected comment end.\n");
      }
      continue;
    }

    if(l->s.comment_level > 0 || l->s.line_is_commented)
      continue;

    // finally, actually generate tokens
    if(is_valid_symbol_start_char(c)) {
      int len = 0;
      while(is_valid_symbol_char(l->s.to_lex[l->s.i + (++len)])) {};
      char *symbol_str = l->s.to_lex + l->s.i;

      Token t;
      t.loc = l->s.loc;

      if(len == 2 && 0 == memcmp(symbol_str, "fn", 2)) {
        t.type = TFN;
      } else if(len == 6 && 0 == memcmp(symbol_str, "return", 6)) {
        t.type = TRETURN;
      } else {
        t.data.symbol = st_get_id_of(symbol_str, len);
        t.type = TSYMBOL;
      }

      l->s.i = l->s.i + len-1;
      l->s.loc.character += len-1;
      return t;
    } else if(isdigit(c)) {
      int len = 0;
      while(isdigit(l->s.to_lex[l->s.i + (++len)]));
      char *str = l->s.to_lex + l->s.i;
      int value = atoi(str);

      Token t;
      t.loc = l->s.loc;
      t.type = TLITERAL_INT;
      t.data.literal_int = value;
      l->s.i += len-1;
      l->s.loc.character += len-1;
      return t;
    } else { // some syntax token

      Token t;
      t.loc = l->s.loc;

      char next_c = l->s.to_lex[l->s.i+1];

      switch(c) {
        case '(': t.type = TOPEN_PAREN; break;
        case ')': t.type = TCLOSE_PAREN; break;
        case '{': t.type = TOPEN_BRACE; break;
        case '}': t.type = TCLOSE_BRACE; break;
        case '[': t.type = TOPEN_BRACKET; break;
        case ']': t.type = TCLOSE_BRACKET; break;

        case '+': if(next_c == '=') {
                    t.type = TPLUS_EQUALS;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else if(next_c == '+') {
                    t.type = TDOUBLE_PLUS;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TPLUS;
                    break;
                  }
        case '-': if(next_c == '>') {
                    t.type = TARROW;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else if(next_c == '=') {
                    t.type = TMINUS_EQUALS;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else if(next_c == '-') {
                    t.type = TDOUBLE_MINUS;
                    l->s.i++;
                    l->s.loc.character++;
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
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TAMPERSAND;
                    break;
                  }
        case '|': if(next_c == '|') {
                    t.type = TDOUBLE_OR;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TOR;
                    break;
                  }
        case '^': if(next_c == '^') {
                    t.type = TDOUBLE_XOR;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TXOR;
                    break;
                  }
        case '~': t.type = TTILDA; break;
        case '!': if(next_c == '=') {
                    t.type = TNOT_EQUALS;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TNOT;
                    break;
                  }
        case '<': if(next_c == '=') {
                    t.type = TLESS_THAN_OR_EQUAL_TO;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TLESS_THAN;
                    break;
                  }
        case '>': if(next_c == '=') {
                    t.type = TGREATER_THAN_OR_EQUAL_TO;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TGREATER_THAN;
                    break;
                  }


        case '=': if(next_c == '=') {
                    t.type = TDOUBLE_EQUALS;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TEQUALS;
                    break;
                  }

        case '?': t.type = TQUESTION_MARK; break;

        case ':': if(next_c == ':') {
                    t.type = TDOUBLE_COLON;
                    l->s.i++;
                    l->s.loc.character++;
                    break;
                  } else {
                    t.type = TCOLON;
                    break;
                  }

        case ';': t.type = TSEMICOLON; break;
        case '.': t.type = TDOT; break;
        case ',': t.type = TCOMMA; break;
        case '$': t.type = TDOLLAR_SIGN; break;

        default: print_error(l->s.loc);
                 printf("Unrecognized special character.\n");
      }

      return t;
    }

  }

  l->s.finished = true;
  return eol_token(l->s.loc);
}
