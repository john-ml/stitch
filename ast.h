#ifndef AST_INCLUDED_H
#define AST_INCLUDED_H

#include "vec.h"
#include "pair.h"
#include <stdio.h>

typedef enum { UOP_NEG, UOP_REF, UOP_DEREF } uop_t;
typedef enum { BOP_ADD, BOP_MUL } bop_t;

typedef enum {
  EXP_PRGM, EXP_FUNC,
  EXP_LET, EXP_SET, EXP_BODY,
  EXP_TY_RECORD, EXP_TY_VARIANT, EXP_TY_PTR, EXP_FPTR,
  EXP_ID, EXP_NUM, EXP_STR,
  EXP_UOP, EXP_BOP,
  EXP_PROJ, EXP_INDEX, EXP_CALL,
  EXP_RECORD, EXP_VARIANT,
  EXP_VEC, EXP_PAIR
} node_tag_t;
#define REC struct node_t *
typedef struct node_t {
  node_tag_t is;
  union {
    vec_t prgm;
    struct { char *f; vec_t args; REC ret; REC body; } func;

    struct { char *x; REC t; REC e; } let;
    struct { REC x; REC e; } set;
    struct { vec_t stmts; REC ret; } body;

    vec_t ty_record;
    vec_t ty_variant;
    REC ty_ptr;
    struct { vec_t args; REC ret; } fptr;

    char *id;
    int num;
    char *str;

    struct { uop_t op; REC e; } uop;
    struct { REC l; bop_t op; REC r; } bop;

    struct { REC e; char *id; } proj;
    struct { REC e; REC i; } index;
    struct { REC f; vec_t args; } call;
    vec_t record;
    struct { char *name; REC e; } variant;

    vec_t vec;
    pair_t pair;
  } as;
} *node_t;
#undef REC

node_t node_prgm(vec_t funcs);
node_t node_func(char *f, vec_t args, node_t ret, node_t body);
node_t node_let(char *x, node_t t, node_t e);
node_t node_set(node_t x, node_t e);
node_t node_body(vec_t stmts, node_t ret);
node_t node_ty_record(vec_t fields);
node_t node_ty_variant(vec_t fields);
node_t node_ty_ptr(node_t ty);
node_t node_fptr(vec_t args, node_t ret);
node_t node_id(char *id);
node_t node_str(char *str);
node_t node_num(int num);
node_t node_uop(uop_t op, node_t e);
node_t node_bop(node_t l, bop_t op, node_t r);
node_t node_proj(node_t e, char *id);
node_t node_index(node_t e, node_t i);
node_t node_call(node_t f, vec_t args);
node_t node_record(vec_t fields);
node_t node_variant(char *name, node_t e);
node_t node_vec(vec_t v);
node_t node_pair(pair_t p);
void node_free(node_t e);
void node_pp(FILE *fp, node_t e);

#endif // AST_INCLUDED_H
