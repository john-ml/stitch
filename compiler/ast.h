#ifndef AST_INCLUDED_H
#define AST_INCLUDED_H

#include "vec.h"
#include "pair.h"
#include "interning.h"
#include "uf.h"
#include <stdio.h>

#define UOP_TODO
typedef enum { UOP_NEG, UOP_REF, UOP_DEREF } uop_t;

#define BOP_TODO
typedef enum { BOP_ADD, BOP_MUL, BOP_SUB } bop_t;

#define EXP_TODO
#define TY_TODO
#define NODE_TODO TY_TODO EXP_TODO
typedef enum {
  EXP_PRGM, EXP_FUNC,
  EXP_LET, EXP_SET, EXP_BODY,
  EXP_TY_RECORD, EXP_TY_VARIANT, EXP_TY_PTR, EXP_FPTR, EXP_UVAR,
  EXP_ID, EXP_NUM, EXP_STR,
  EXP_UOP, EXP_BOP,
  EXP_PROJ, EXP_INDEX, EXP_CALL,
  EXP_RECORD, EXP_VARIANT,
  EXP_MATCH, EXP_ARM,
  EXP_VEC, EXP_PAIR
} node_tag_t;
#define REC struct node_t *
typedef struct node_t {
  node_tag_t is;
  union {
    vec_t prgm;
    struct { sid_t f; vec_t args; REC ret; REC body; } func;

    struct { sid_t x; REC t; REC e; } let;
    struct { REC x; REC e; } set;
    struct { vec_t stmts; REC ret; } body;

    vec_t ty_record;
    vec_t ty_variant;
    REC ty_ptr;
    struct { vec_t args; REC ret; } fptr;
    uf_id_t uvar;

    sid_t id;
    int num;
    sid_t str;

    struct { uop_t op; REC e; } uop;
    struct { REC l; bop_t op; REC r; } bop;

    struct { REC e; sid_t id; } proj;
    struct { REC e; REC i; } index;
    struct { REC f; vec_t args; } call;
    vec_t record;
    struct { sid_t name; REC e; } variant;

    struct { REC e; vec_t arms; } match;
    struct { sid_t ctr; sid_t x; REC e; } arm;

    vec_t vec;
    pair_t pair;
  } as;
} *node_t;
#undef REC

node_t node_prgm(vec_t funcs);
node_t node_func(sid_t f, vec_t args, node_t ret, node_t body);
node_t node_let(sid_t x, node_t t, node_t e);
node_t node_set(node_t x, node_t e);
node_t node_body(vec_t stmts, node_t ret);
node_t node_ty_record(vec_t fields);
node_t node_ty_variant(vec_t fields);
node_t node_ty_ptr(node_t ty);
node_t node_fptr(vec_t args, node_t ret);
node_t node_id(sid_t id);
node_t node_str(sid_t str);
node_t node_num(int num);
node_t node_uop(uop_t op, node_t e);
node_t node_bop(node_t l, bop_t op, node_t r);
node_t node_proj(node_t e, sid_t id);
node_t node_index(node_t e, node_t i);
node_t node_call(node_t f, vec_t args);
node_t node_record(vec_t fields);
node_t node_variant(sid_t name, node_t e);
node_t node_match(node_t e, vec_t arms);
node_t node_arm(sid_t ctr, sid_t x, node_t e);
node_t node_vec(vec_t v);
node_t node_pair(pair_t p);
void node_del(node_t e);
void node_pp(stab_t t, FILE *fp, node_t e);

int node_is_ty(node_t e);
int node_is_tm(node_t e);

void uop_pp(FILE *fp, uop_t op);
void bop_pp(FILE *fp, bop_t op);

#endif // AST_INCLUDED_H
