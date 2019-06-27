%{
#include "interning.h"
#define YY_DECL int yylex(stab_t *interned)
#include "ast.h"
#define YYSTYPE node_t
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
  for (char *q = yytext; *q; ++p, ++q) {
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
  *p = 0;
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

[1-9][0-9]*                   yylval = node_num(atoi(yytext)); return NUM;
[a-zA-Z][a-zA-Z0-9_]*         yylval = node_id(stab_add(interned, yytext)); return ID;
\"(\\.|[^\\"])*\"             { char *s = yytextunescape();
                                yylval = node_str(stab_add(interned, s));
                                free(s);
                                return STR;
                              }

[ \t\r]                       ;

\n                            ;

