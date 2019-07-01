%{
#include "interning.h"
#define YY_DECL int yylex(stab_t *interned, arena_p *a)
#include "ast.h"
#define YYSTYPE node_p
#include "parser.tab.h"
#include <string.h>

char *yytextcpy() {
  char *s = malloc(yyleng + 1);
  memcpy(s, yytext, yyleng + 1);
  return s;
}

char *yytextunescape() {
  char *s = malloc(yyleng + 1);
  char *p = s;
  for (char *q = yytext + 1; *q; ++p, ++q) { // Skip leading quote
    if (*q == '\\') {
      switch (*++q) {
        case 't': *p = '\t'; break;
        case 'r': *p = '\r'; break;
        case 'n': *p = '\n'; break;
        case '"': *p = '"'; break;
        default:
          fprintf(stderr, "Unrecognized escape code '%c'\n", *q);
          exit(1);
      }
    } else
      *p = *q;
  }
  *(p - 1) = 0; // Clobber trailing quote
  return s;
}
%}
%option yylineno
%%
[|<>@=:;,*&+.\[\](){}]        return yytext[0];
:=                            return ASGN;
->                            return RARROW;
case                          return CASE;
end                           return END;

0|[1-9][0-9]*                 yylval = node_num(a, atoi(yytext)); return NUM;
[a-zA-Z][a-zA-Z0-9_]*         yylval = node_id(a, stab_add(a, interned, yytext)); return ID;
\"(\\.|[^\\"])*\"             { char *s = yytextunescape();
                                yylval = node_str(a, stab_add(a, interned, s));
                                free(s);
                                return STR;
                              }

[ \t\r]                       ;

\n                            ;

