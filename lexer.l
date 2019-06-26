%{
#include "ast.h"
#define YYSTYPE node_t
#include "parser.tab.h"
#include <string.h>

char *yytextcpy() {
  char *s = malloc(yyleng + 1);
  memcpy(s, yytext, yyleng + 1);
  return s;
}
%}
%option yylineno
%%
[1-9][0-9]*           yylval = node_num(atoi(yytext)); return NUM;
[a-zA-Z][a-zA-Z0-9_]* yylval = node_id(yytextcpy()); return ID;
\"(\\.|[^\\"])*\"     yylval = node_str(yytextcpy()); return STR;

[<>@=:;,*&+.\[\](){}] return yytext[0];
:=                    return ASGN;
->                    return RARROW;

[ \t\r]               ;

\n                    ;

