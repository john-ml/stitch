#include "pair.h"
#include "ast.h"
#include <stdlib.h>
#include <stdio.h>

node_t node_prgm(vec_t funcs) {
  puts("node_prgm");
  node_t e = malloc(sizeof(*e));
  e->is = EXP_PRGM;
  e->as.prgm = funcs;
  return e;
}

node_t node_func(char *f, vec_t args, node_t ret, node_t body) {
  puts("node_func");
  node_t e = malloc(sizeof(*e));
  e->is = EXP_FUNC;
  e->as.func.f = f;
  e->as.func.args = args;
  e->as.func.ret = ret;
  e->as.func.body = body;
  return e;
}

node_t node_let(char *x, node_t t, node_t e) {
  puts("node_let");
  node_t r = malloc(sizeof(*r));
  r->is = EXP_LET;
  r->as.let.x = x;
  r->as.let.t = t;
  r->as.let.e = e;
  return r;
}

node_t node_set(node_t x, node_t e) {
  puts("node_set");
  node_t r = malloc(sizeof(*r));
  r->is = EXP_LET;
  r->as.set.x = x;
  r->as.set.e = e;
  return e;
}

node_t node_body(vec_t stmts, node_t ret) {
  puts("node_body");
  node_t r = malloc(sizeof(*r));
  r->is = EXP_BODY;
  r->as.body.stmts = stmts;
  r->as.body.ret = ret;
  return r;
}

node_t node_ty_record(char *name, vec_t fields) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_TY_RECORD;
  r->as.ty_record.name = name;
  r->as.ty_record.fields = fields;
  return r;
}

node_t node_ty_variant(vec_t fields) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_TY_VARIANT;
  r->as.ty_variant = fields;
  return r;
}

node_t node_fptr(vec_t args, node_t ret) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_FPTR;
  r->as.fptr.args = args;
  r->as.fptr.ret = ret;
  return r;
}

node_t node_id(char *id) {
  printf("node_id %s\n", id);
  node_t r = malloc(sizeof(*r));
  r->is = EXP_ID;
  r->as.id = id;
  return r;
}

node_t node_num(int num) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_NUM;
  r->as.num = num;
  return r;
}

node_t node_uop(uop_t op, node_t e) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_UOP;
  r->as.uop.op = op;
  r->as.uop.e = e;
  return r;
}

node_t node_bop(node_t l, bop_t op, node_t r) {
  node_t e = malloc(sizeof(*e));
  e->is = EXP_BOP;
  e->as.bop.l = l;
  e->as.bop.op = op;
  e->as.bop.r = r;
  return e;
}

node_t node_proj(node_t e, char *id) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_PROJ;
  r->as.proj.e = e;
  r->as.proj.id = id;
  return r;
}

node_t node_index(node_t e, node_t i) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_INDEX;
  r->as.index.e = e;
  r->as.index.i = i;
  return r;
}

node_t node_call(node_t f, vec_t args) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_CALL;
  r->as.call.f = f;
  r->as.call.args = args;
  return r;
}

node_t node_record(char *name, vec_t fields) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_RECORD;
  r->as.record.name = name;
  r->as.record.fields = fields;
  return r;
}

node_t node_variant(char *name, node_t e) {
  node_t r = malloc(sizeof(*r));
  r->is = EXP_VARIANT;
  r->as.variant.name = name;
  r->as.variant.e = e;
  return r;
}

node_t node_vec(vec_t v) {
  puts("node_vec");
  node_t e = malloc(sizeof(*e));
  e->is = EXP_VEC;
  e->as.vec = v;
  return e;
}

node_t node_pair(pair_t p) {
  printf("node_pair %p\n", p);
  node_t e = malloc(sizeof(*e));
  e->is = EXP_PAIR;
  e->as.pair = p;
  return e;
}

void node_free(node_t e) {
  switch (e->is) {
    case EXP_PRGM:
      for (int i = 0; i < vec_len(e->as.prgm); ++i)
        node_free(e->as.prgm->data[i]);
      vec_free(e->as.prgm);
    break;
    case EXP_FUNC:
      free(e->as.func.f);
      for (int i = 0; i < vec_len(e->as.func.args); ++i) {
        pair_t p = e->as.func.args->data[i];
        free(p->a);
        node_free(p->b);
        free(p);
      }
      vec_free(e->as.func.args);
      node_free(e->as.func.ret);
      node_free(e->as.func.body);
    break;
    case EXP_LET:
      free(e->as.let.x);
      node_free(e->as.let.t);
      node_free(e->as.let.e);
    break;
    case EXP_SET:
      node_free(e->as.set.x);
      node_free(e->as.set.e);
    break;
    case EXP_BODY:
      for (int i = 0; i < vec_len(e->as.body.stmts); ++i)
        node_free(e->as.body.stmts->data[i]);
      vec_free(e->as.body.stmts);
      node_free(e->as.body.ret);
    break;
    case EXP_TY_RECORD:
      free(e->as.ty_record.name);
      for (int i = 0; i < vec_len(e->as.ty_record.fields); ++i) {
        pair_t p = e->as.ty_record.fields->data[i];
        free(p->a);
        node_free(p->b);
        free(p);
      }
      vec_free(e->as.ty_record.fields);
    break;
    case EXP_TY_VARIANT:
      for (int i = 0; i < vec_len(e->as.ty_variant); ++i) {
        pair_t p = e->as.ty_variant->data[i];
        free(p->a);
        node_free(p->b);
        free(p);
      }
      vec_free(e->as.ty_variant);
    break;
    case EXP_FPTR:
      for (int i = 0; i < vec_len(e->as.fptr.args); ++i)
        node_free(e->as.fptr.args->data[i]);
      vec_free(e->as.fptr.args);
      node_free(e->as.fptr.ret);
    break;
    case EXP_ID: free(e->as.id); break;
    case EXP_NUM: break;
    case EXP_UOP: node_free(e->as.uop.e); break;
    case EXP_BOP:
      node_free(e->as.bop.l);
      node_free(e->as.bop.r);
    break;
    case EXP_PROJ:
      node_free(e->as.proj.e);
      free(e->as.proj.id);
    break;
    case EXP_INDEX:
      node_free(e->as.index.e);
      node_free(e->as.index.i);
    break;
    case EXP_CALL:
      node_free(e->as.call.f);
      for (int i = 0; i < vec_len(e->as.call.args); ++i)
        node_free(e->as.call.args->data[i]);
      vec_free(e->as.call.args);
    break;
    case EXP_RECORD:
      free(e->as.record.name);
      for (int i = 0; i < vec_len(e->as.record.fields); ++i) {
        pair_t p = e->as.record.fields->data[i];
        free(p->a);
        node_free(p->b);
        free(p);
      }
      vec_free(e->as.record.fields);
    break;
    case EXP_VARIANT:
      free(e->as.variant.name);
      node_free(e->as.variant.e);
    break;
  } 
  free(e);
}

void node_pp_newline(FILE *fp, int lvl) {
  fputc('\n', fp);
  for (int i = 0; i < lvl; ++i)
    fputc(' ', fp);
}

void uop_pp(FILE *fp, uop_t op) {
  switch (op) {
    case UOP_NEG: fputc('-', fp); break;
    case UOP_REF: fputc('&', fp); break;
    case UOP_DEREF: fputc('*', fp); break;
  }
}

void bop_pp(FILE *fp, bop_t op) {
  switch (op) {
    case BOP_ADD: fputc('+', fp); break;
    case BOP_MUL: fputc('*', fp); break;
  }
}

void node_pp_(FILE *fp, node_t e, int lvl) {
  switch (e->is) {
    case EXP_PRGM:
      for (int i = 0; i < vec_len(e->as.prgm); ++i) {
        node_pp_(fp, e->as.prgm->data[i], lvl);
        node_pp_newline(fp, lvl);
      }
    break;
    case EXP_FUNC:
      fprintf(fp, "%s (", e->as.func.f);
      for (int i = 0; i < vec_len(e->as.func.args); ++i) {
        pair_t p = e->as.func.args->data[i];
        fprintf(fp, "%s: ", (char *)(p->a));
        node_pp_(fp, p->b, lvl);
        if (i < vec_len(e->as.func.args) - 1)
          fprintf(fp, ", ");
      }
      fprintf(fp, "): ");
      node_pp_(fp, e->as.func.ret, lvl);
      fprintf(fp, " =");
      node_pp_newline(fp, lvl + 2);
      node_pp_(fp, e->as.func.body, lvl + 2);
    break;
    case EXP_LET:
      fprintf(fp, "%s: ", e->as.let.x);
      node_pp_(fp, e->as.let.t, lvl);
      fprintf(fp, " = ");
      node_pp_(fp, e->as.let.e, lvl);
      fputc(';', fp);
      node_pp_newline(fp, lvl);
    break;
    case EXP_SET:
      node_pp_(fp, e->as.set.x, lvl);
      fprintf(fp, " = ");
      node_pp_(fp, e->as.set.e, lvl);
      fputc(';', fp);
      node_pp_newline(fp, lvl);
    break;
    case EXP_BODY:
      for (int i = 0; i < vec_len(e->as.body.stmts); ++i)
        node_pp_(fp, e->as.body.stmts->data[i], lvl);
      node_pp_(fp, e->as.body.ret, lvl);
    break;
    case EXP_TY_RECORD:
      // free(e->as.ty_record.name);
      // for (int i = 0; i < vec_len(e->as.ty_record.fields); ++i) {
      //   pair_t p = e->as.ty_record.fields->data[i];
      //   free(p->a);
      //   node_free(p->b);
      //   free(p);
      // }
      // vec_free(e->as.ty_record.fields);
    break;
    case EXP_TY_VARIANT:
      // for (int i = 0; i < vec_len(e->as.ty_variant); ++i) {
      //   pair_t p = e->as.ty_variant->data[i];
      //   free(p->a);
      //   node_free(p->b);
      //   free(p);
      // }
      // vec_free(e->as.ty_variant);
    break;
    case EXP_FPTR:
      // for (int i = 0; i < vec_len(e->as.fptr.args); ++i)
      //   node_free(e->as.fptr.args->data[i]);
      // vec_free(e->as.fptr.args);
      // node_free(e->as.fptr.ret);
    break;
    case EXP_ID: fprintf(fp, "%s", e->as.id); break;
    case EXP_NUM: fprintf(fp, "%d", e->as.num); break;
    case EXP_UOP:
      fputc('(', fp);
      uop_pp(fp, e->as.uop.op);
      node_pp_(fp, e->as.uop.e, lvl);
      fputc(')', fp);
    break;
    case EXP_BOP:
      fputc('(', fp);
      node_pp_(fp, e->as.bop.l, lvl);
      fputc(' ', fp);
      bop_pp(fp, e->as.bop.op);
      fputc(' ', fp);
      node_pp_(fp, e->as.bop.r, lvl);
      fputc(')', fp);
    break;
    case EXP_PROJ:
      // node_free(e->as.proj.e);
      // free(e->as.proj.id);
    break;
    case EXP_INDEX:
      // node_free(e->as.index.e);
      // node_free(e->as.index.i);
    break;
    case EXP_CALL:
      node_pp_(fp, e->as.call.f, lvl);
      fputc('(', fp);
      for (int i = 0; i < vec_len(e->as.call.args); ++i) {
        node_pp_(fp, e->as.call.args->data[i], lvl);
        if (i < vec_len(e->as.call.args) - 1)
          fprintf(fp, ", ");
      }
      fputc(')', fp);
    break;
    case EXP_RECORD:
      // free(e->as.record.name);
      // for (int i = 0; i < vec_len(e->as.record.fields); ++i) {
      //   pair_t p = e->as.record.fields->data[i];
      //   free(p->a);
      //   node_free(p->b);
      //   free(p);
      // }
      // vec_free(e->as.record.fields);
    break;
    case EXP_VARIANT:
      // free(e->as.variant.name);
      // node_free(e->as.variant.e);
    break;
  }
}

void node_pp(FILE *fp, node_t e) { node_pp_(fp, e, 0); }
