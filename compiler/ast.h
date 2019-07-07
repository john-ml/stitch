#ifndef AST_INCLUDED_H
#define AST_INCLUDED_H

#include "vec.h"
#include "interning.h"
#include "uf.h"
#include "arena.h"
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
  EXP_ID_E, EXP_VEC
} node_tag_t;
#define REC struct node_t *
typedef struct node_t {
  node_tag_t is;
  union {
    VEC(REC) prgm;
    struct { sid_t f; VEC(REC) args; REC ret; REC body; } func;

    struct { sid_t x; REC t; REC e; } let;
    struct { REC x; REC e; } set;
    struct { VEC(REC) stmts; REC ret; } body;

    VEC(REC) ty_record;
    VEC(REC) ty_variant;
    REC ty_ptr;
    struct { VEC(REC) args; REC ret; } fptr;
    uf_id_t uvar;

    sid_t id;
    int num;
    sid_t str;

    struct { uop_t op; REC e; } uop;
    struct { REC l; bop_t op; REC r; } bop;

    struct { REC e; sid_t id; } proj;
    struct { REC e; REC i; } index;
    struct { REC f; VEC(REC) args; } call;
    VEC(REC) record;
    struct { sid_t name; REC e; } variant;

    struct { REC e; VEC(REC) arms; } match;
    struct { sid_t ctr; sid_t x; REC e; } arm;

    struct { sid_t id; REC e; } id_e;
    vec_p vec;
  } as;
} *node_p;
#undef REC

node_p node_prgm(arena_p *a, VEC(node_p) funcs);
node_p node_func(arena_p *a, sid_t f, VEC(node_p) args, node_p ret, node_p body);
node_p node_let(arena_p *a, sid_t x, node_p t, node_p e);
node_p node_set(arena_p *a, node_p x, node_p e);
node_p node_body(arena_p *a, VEC(node_p) stmts, node_p ret);
node_p node_ty_record(arena_p *a, VEC(node_p) fields);
node_p node_ty_variant(arena_p *a, VEC(node_p) fields);
node_p node_ty_ptr(arena_p *a, node_p ty);
node_p node_fptr(arena_p *a, VEC(node_p) args, node_p ret);
node_p node_id(arena_p *a, sid_t id);
node_p node_str(arena_p *a, sid_t str);
node_p node_num(arena_p *a, int num);
node_p node_uop(arena_p *a, uop_t op, node_p e);
node_p node_bop(arena_p *a, node_p l, bop_t op, node_p r);
node_p node_proj(arena_p *a, node_p e, sid_t id);
node_p node_index(arena_p *a, node_p e, node_p i);
node_p node_call(arena_p *a, node_p f, VEC(node_p) args);
node_p node_record(arena_p *a, VEC(node_p) fields);
node_p node_variant(arena_p *a, sid_t name, node_p e);
node_p node_match(arena_p *a, node_p e, VEC(node_p) arms);
node_p node_arm(arena_p *a, sid_t ctr, sid_t x, node_p e);
node_p node_vec(arena_p *a, VEC(node_p) v);
node_p node_id_e(arena_p *a, sid_t id, node_p e);
void node_pp(stab_t t, FILE *fp, node_p e);

int node_is_ty(node_p e);
int node_is_tm(node_p e);

void uop_pp(FILE *fp, uop_t op);
void bop_pp(FILE *fp, bop_t op);

#endif // AST_INCLUDED_H
