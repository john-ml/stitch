%{

#include "misc.h"
#include "ast.h"
#include "vec.h"
#include "interning.h"
#include "arena.h"

#define YYSTYPE node_t
#define YYERROR_VERBOSE

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void yyerror(node_t *out, stab_t *interned, arena_t *a, char const *s);
int yylex(stab_t *interned, arena_t *a);
int yylex_destroy(void);
FILE *yyin;

%}

%lex-param {stab_t *interned} {arena_t *a}
%parse-param {node_t *out} {stab_t *interned} {arena_t *a}
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

prgm : func      { $$ = node_prgm(a, vec_sing($1)); }
     | prgm func { vec_add(&$1->as.prgm, $2); $$ = $1; }

func : ID '(' ty_fields ')' opt_type '=' fn_body {
  $$ = node_func(a, $1->as.id, $3->as.vec, $5, $7);
}

fn_body : body | expr { $$ = $1; }

body : stmts ';' expr            { $$ = node_body(a, $1->as.vec, $3); }

stmts : stmt                     { $$ = node_vec(a, vec_sing($1)); }
      | stmts ';' stmt           { vec_add(&$1->as.vec, $3); $$ = $1; }

stmt : ID opt_type '=' expr      { $$ = node_let(a, $1->as.id, $2, $4); }
     | expr ASGN expr            { $$ = node_set(a, $1, $3); }

type : ID                        { $$ = $1; }
     | '{' ty_fields '}'         { $$ = node_ty_record(a, $2->as.vec); }
     | '<' ty_fields '>'         { $$ = node_ty_variant(a, $2->as.vec); }
     | '*' type                  { $$ = node_ty_ptr(a, $2); }
     | '(' types ')' RARROW type { $$ = node_fptr(a, $2->as.vec, $5); }

types :                { $$ = node_vec(a, vec_new()); }
      | type           { $$ = node_vec(a, vec_sing($1)); }
      | types ',' type { vec_add(&$1->as.vec, $3); $$ = $1; }

ty_fields :                        { $$ = node_vec(a, vec_new()); }
          | ty_field               { $$ = node_vec(a, vec_sing($1->as.pair)); }
          | ty_fields ',' ty_field { vec_add(&$1->as.vec, $3->as.pair); $$ = $1; }

ty_field : ID opt_type { $$ = node_pair(a, pair_new((void *)(size_t)$1->as.id, $2)); }

opt_type :          { $$ = NULL; }
         | ':' type { $$ = $2; }

expr : ID | NUM | STR        { $$ = $1; }
     | expr '+' expr         { $$ = node_bop(a, $1, BOP_ADD, $3); }
     | expr '*' expr         { $$ = node_bop(a, $1, BOP_MUL, $3); }
     | '-' expr %prec NEG    { $$ = node_uop(a, UOP_NEG, $2); }
     | '*' expr %prec DEREF  { $$ = node_uop(a, UOP_DEREF, $2); }
     | '&' expr %prec REF    { $$ = node_uop(a, UOP_REF, $2); }
     | expr '.' ID           { $$ = node_proj(a, $1, $3->as.id); }
     | '(' body ')'          { $$ = $2; }
     | '(' expr ')'          { $$ = $2; }
     | expr '[' expr ']'     { $$ = node_index(a, $1, $3); }
     | expr '(' exprs ')'    { $$ = node_call(a, $1, $3->as.vec); }
     | '{' fields '}'        { $$ = node_record(a, $2->as.vec); }
     | ID '@' expr           { $$ = node_variant(a, $1->as.id, $3); }
     | CASE expr arms END    { $$ = node_match(a, $2, $3->as.vec); }

arms : '{' '}'              { $$ = node_vec(a, vec_new()); }
     | arm                  { $$ = node_vec(a, vec_sing($1)); }
     | arms arm             { vec_add(&$1->as.vec, $2); $$ = $1; }

arm : '|' ID '@' ID RARROW fn_body { $$ = node_arm(a, $2->as.id, $4->as.id, $6); }

fields :                  { $$ = node_vec(a, vec_new()); }
       | field            { $$ = node_vec(a, vec_sing($1->as.pair)); }
       | fields ',' field { vec_add(&$1->as.vec, $3->as.pair); $$ = $1; }

field : ID '=' expr { $$ = node_pair(a, pair_new((void *)(size_t)$1->as.id, $3)); }

exprs :                { $$ = node_vec(a, vec_new()); }
      | expr           { $$ = node_vec(a, vec_sing($1)); }
      | exprs ',' expr { vec_add(&$1->as.vec, $3); $$ = $1; }

%%

void yyerror(node_t *out, stab_t *interned, arena_t *a, char const *s) {
  puts(s);
  exit(1);
}
