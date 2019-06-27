%{

#include "misc.h"
#include "ast.h"
#include "vec.h"

#define YYSTYPE node_t
#define YYERROR_VERBOSE

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void yyerror(node_t *out, char const *s);
int yylex(void);
int yylex_destroy(void);
FILE *yyin;

%}

%parse-param {node_t *out}
%token NUM
%token STR
%token ID
%token ASGN
%token CASE
%token END
%token RARROW
%right RARROW
%left '+'
%left '*'
%left NEG
%left REF DEREF
%left '[' '('
%right '@'
%left '.'

%%

init : prgm { *out = $1; }

prgm : func      { $$ = node_prgm(vec_sing($1)); }
     | prgm func { vec_add(&$1->as.prgm, $2); $$ = $1; }

func : ID '(' ty_fields ')' ':' type '=' fn_body {
  $$ = node_func($1->as.id, $3->as.vec, $6, $8);
  free($1); free($3);
}

fn_body : body | expr { $$ = $1; }

body : stmts ';' expr            { $$ = node_body($1->as.vec, $3); free($1); }

stmts : stmt                     { $$ = node_vec(vec_sing($1)); }
      | stmts ';' stmt           { vec_add(&$1->as.vec, $3); $$ = $1; }

stmt : ID ':' type '=' expr      { $$ = node_let($1->as.id, $3, $5); free($1); }
     | expr ASGN expr            { $$ = node_set($1, $3); }

type : ID                        { $$ = $1; }
     | '{' ty_fields '}'         { $$ = node_ty_record($2->as.vec); free($2); }
     | '<' ty_fields '>'         { $$ = node_ty_variant($2->as.vec); free($2); }
     | '*' type                  { $$ = node_ty_ptr($2); }
     | '(' types ')' RARROW type { $$ = node_fptr($2->as.vec, $5); free($2); }

types :                { $$ = node_vec(vec_new()); }
      | type           { $$ = node_vec(vec_sing($1)); }
      | types ',' type { vec_add(&$1->as.vec, $3); $$ = $1; }

ty_fields :                        { $$ = node_vec(vec_new()); }
          | ty_field               { $$ = node_vec(vec_sing($1->as.pair)); free($1); }
          | ty_fields ',' ty_field { vec_add(&$1->as.vec, $3->as.pair); $$ = $1; free($3); }

ty_field : ID ':' type { $$ = node_pair(pair_new($1->as.id, $3)); free($1); }

expr : ID | NUM | STR        { $$ = $1; }
     | expr '+' expr         { $$ = node_bop($1, BOP_ADD, $3); }
     | expr '*' expr         { $$ = node_bop($1, BOP_MUL, $3); }
     | '-' expr %prec NEG    { $$ = node_uop(UOP_NEG, $2); }
     | '*' expr %prec DEREF  { $$ = node_uop(UOP_DEREF, $2); }
     | '&' expr %prec REF    { $$ = node_uop(UOP_REF, $2); }
     | expr '.' ID           { $$ = node_proj($1, $3->as.id); free($3); }
     | '(' body ')'          { $$ = $2; }
     | '(' expr ')'          { $$ = $2; }
     | expr '[' expr ']'     { $$ = node_index($1, $3); }
     | expr '(' exprs ')'    { $$ = node_call($1, $3->as.vec); free($3); }
     | '{' fields '}'        { $$ = node_record($2->as.vec); free($2); }
     | ID '@' expr           { $$ = node_variant($1->as.id, $3); free($1); }
     | CASE expr arms END    { $$ = node_match($2, $3->as.vec); free($3); }

arms : '{' '}'              { $$ = node_vec(vec_new()); }
     | arm                  { $$ = node_vec(vec_sing($1)); }
     | arms arm             { vec_add(&$1->as.vec, $2); $$ = $1; }

arm : '|' ID '@' ID RARROW fn_body {
  $$ = node_arm($2->as.id, $4->as.id, $6);
  free($2);
  free($4);
}

fields :                  { $$ = node_vec(vec_new()); }
       | field            { $$ = node_vec(vec_sing($1->as.pair)); free($1); }
       | fields ',' field { vec_add(&$1->as.vec, $3->as.pair); $$ = $1; free($3); }

field : ID '=' expr { $$ = node_pair(pair_new($1->as.id, $3)); free($1); }

exprs :                { $$ = node_vec(vec_new()); }
      | expr           { $$ = node_vec(vec_sing($1)); }
      | exprs ',' expr { vec_add(&$1->as.vec, $3); $$ = $1; }

%%

void yyerror(node_t *out, char const *s) { printf("%s\n", s); exit(1); }

void main(int argc, char **argv) {
  yyin = argc > 1 ? fopen(argv[1], "r") : stdin;
  node_t e;
  yyparse(&e);
  node_pp(stdout, e);
  node_free(e);
  if (yyin != stdin)
    fclose(yyin);
  yylex_destroy();
}
