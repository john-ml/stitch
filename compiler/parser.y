%{

#include "misc.h"
#include "ast.h"
#include "vec.h"
#include "interning.h"
#include "arena.h"

#define YYSTYPE node_p
#define YYERROR_VERBOSE

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void yyerror(node_p *out, stab_t *interned, arena_p *a, char const *s);
int yylex(stab_t *interned, arena_p *a);
int yylex_destroy(void);
FILE *yyin;

%}

%lex-param {stab_t *interned} {arena_p *a}
%parse-param {node_p *out} {stab_t *interned} {arena_p *a}
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

prgm : func      { $$ = node_prgm(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
     | prgm func { vec_add(a, (vec_p)&$1->as.prgm, (any_t)&$2); $$ = $1; }

func : ID poly_opt '(' ty_fields ')' type_opt ':' fn_body {
  $$ = node_func(a, $1->as.id, $4->as.vec, $6, $8); // TODO poly_opt
}

type_opt :      { $$ = NULL; }
         | type { $$ = $1; }

anno_opt :          { $$ = NULL; }
         | ':' type { $$ = $2; }

poly_opt :               { $$ = node_vec(a, vec_new(a, sizeof(sid_t))); }
         | '[' polys ']' { $$ = $2; }

polys : ID       { $$ = node_vec(a, vec_sing(a, sizeof(sid_t), (any_t)&$1->as.id)); }
      | polys ID { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$2->as.id); $$ = $1; }

fn_body : body | expr { $$ = $1; }

body : stmts ';' expr            { $$ = node_body(a, $1->as.vec, $3); }

stmts : stmt                     { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
      | stmts ';' stmt           { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

stmt : ID anno_opt '=' expr      { $$ = node_let(a, $1->as.id, $2, $4); }
     | expr ASGN expr            { $$ = node_set(a, $1, $3); }

type : ID                        { $$ = $1; }
     | '{' ty_fields '}'         { $$ = node_ty_record(a, $2->as.vec); }
     | '{' ty_fields ';' ID '}'  { $$ = node_ty_record(a, $2->as.vec); } // TODO
     | '<' ty_fields '>'         { $$ = node_ty_variant(a, $2->as.vec); }
     | '*' type                  { $$ = node_ty_ptr(a, $2); }
     | '(' types ')' RARROW type { $$ = node_fptr(a, $2->as.vec, $5); }

types :                { $$ = node_vec(a, vec_new(a, sizeof(node_p))); }
      | type           { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
      | types ',' type { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

ty_fields :                        { $$ = node_vec(a, vec_new(a, sizeof(node_p))); }
          | ty_field               { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
          | ty_fields ',' ty_field { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

ty_field : ID type_opt { $$ = node_id_e(a, $1->as.id, $2); }

expr : ID | NUM | STR         { $$ = $1; }
     | expr '+' expr          { $$ = node_bop(a, $1, BOP_ADD, $3); }
     | expr '*' expr          { $$ = node_bop(a, $1, BOP_MUL, $3); }
     | '-' expr %prec NEG     { $$ = node_uop(a, UOP_NEG, $2); }
     | '*' expr %prec DEREF   { $$ = node_uop(a, UOP_DEREF, $2); }
     | '&' expr %prec REF     { $$ = node_uop(a, UOP_REF, $2); }
     | expr '.' ID            { $$ = node_proj(a, $1, $3->as.id); }
     | '(' body ')'           { $$ = $2; }
     | '(' expr ')'           { $$ = $2; }
     | expr '[' expr ']'      { $$ = node_index(a, $1, $3); }
     | expr '(' exprs ')'     { $$ = node_call(a, $1, $3->as.vec); }
     | ID '@' '[' types ']' 
              '(' exprs ')'   { $$ = node_call(a, $1, $7->as.vec); } // TODO
     | '{' fields '}'         { $$ = node_record(a, $2->as.vec); }
     | ID '@' expr            { $$ = node_variant(a, $1->as.id, $3); }
     | CASE expr '{' arms '}' { $$ = node_match(a, $2, $4->as.vec); }

arms :                      { $$ = node_vec(a, vec_new(a, sizeof(node_p))); }
     | arm                  { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
     | arms ',' arm         { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

arm : ID ID ':' fn_body { $$ = node_arm(a, $1->as.id, $2->as.id, $4); }

fields :                  { $$ = node_vec(a, vec_new(a, sizeof(node_p))); }
       | field            { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
       | fields ',' field { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

field : ID '=' expr { $$ = node_id_e(a, $1->as.id, $3); }

exprs :                { $$ = node_vec(a, vec_new(a, sizeof(node_p))); }
      | expr           { $$ = node_vec(a, vec_sing(a, sizeof($1), (any_t)&$1)); }
      | exprs ',' expr { vec_add(a, (vec_p)&$1->as.vec, (any_t)&$3); $$ = $1; }

%%

void yyerror(node_p *out, stab_t *interned, arena_p *a, char const *s) {
  puts(s);
  arena_del(*a);
  if (yyin != stdin)
    fclose(yyin);
  yylex_destroy();
  exit(1);
}
