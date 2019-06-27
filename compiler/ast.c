#include "misc.h"
#include "ast.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

node_t node_prgm(vec_t funcs) { MK_SING(node_t, EXP_PRGM, prgm, funcs); }

node_t node_func(sid_t f, vec_t args, node_t ret, node_t body) {
  MK(node_t, EXP_FUNC, func, .f = f, .args = args, .ret = ret, .body = body);
}

node_t node_let(sid_t x, node_t t, node_t e) {
  MK(node_t, EXP_LET, let, .x = x, .t = t, .e = e);
}

node_t node_set(node_t x, node_t e) { MK(node_t, EXP_SET, set, .x = x, .e = e); }

node_t node_body(vec_t stmts, node_t ret) {
  MK(node_t, EXP_BODY, body, .stmts = stmts, .ret = ret);
}

node_t node_ty_record(vec_t fields) {
  MK_SING(node_t, EXP_TY_RECORD, ty_record, fields);
}

node_t node_ty_variant(vec_t fields) {
  MK_SING(node_t, EXP_TY_VARIANT, ty_variant, fields);
}

node_t node_ty_ptr(node_t ty) { MK_SING(node_t, EXP_TY_PTR, ty_ptr, ty); }

node_t node_fptr(vec_t args, node_t ret) {
  MK(node_t, EXP_FPTR, fptr, .args = args, .ret = ret);
}

node_t node_id(sid_t id) { MK_SING(node_t, EXP_ID, id, id); }

node_t node_str(sid_t str) { MK_SING(node_t, EXP_STR, str, str); }

node_t node_num(int num) { MK_SING(node_t, EXP_NUM, num, num); }

node_t node_uop(uop_t op, node_t e) {
  MK(node_t, EXP_UOP, uop, .op = op, .e = e);
}

node_t node_bop(node_t l, bop_t op, node_t r) {
  MK(node_t, EXP_BOP, bop, .l = l, .op = op, .r = r);
}

node_t node_proj(node_t e, sid_t id) {
  MK(node_t, EXP_PROJ, proj, .e = e, .id = id);
}

node_t node_index(node_t e, node_t i) {
  MK(node_t, EXP_INDEX, index, .e = e, .i = i);
}

node_t node_call(node_t f, vec_t args) {
  MK(node_t, EXP_CALL, call, .f = f, .args = args);
}

node_t node_record(vec_t fields) { MK_SING(node_t, EXP_RECORD, record, fields); }

node_t node_variant(sid_t name, node_t e) {
  MK(node_t, EXP_VARIANT, variant, .name = name, .e = e);
}

node_t node_match(node_t e, vec_t arms) {
  MK(node_t, EXP_MATCH, match, .e = e, .arms = arms);
}

node_t node_arm(sid_t ctr, sid_t x, node_t e) {
  MK(node_t, EXP_ARM, arm, .ctr = ctr, .x = x, .e = e);
}

node_t node_vec(vec_t v) { MK_SING(node_t, EXP_VEC, vec, v); }

node_t node_pair(pair_t p) { MK_SING(node_t, EXP_PAIR, pair, p); }

void field_del(pair_t id_exp) {
  if (id_exp->b)
    node_del(id_exp->b);
  free(id_exp);
}

void node_del(node_t e) {
  switch (e->is) {
    case EXP_PRGM: vec_del(e->as.prgm, (del_t)node_del); break;
    case EXP_FUNC:
      vec_del(e->as.func.args, (del_t)field_del);
      if (e->as.func.ret)
        node_del(e->as.func.ret);
      node_del(e->as.func.body);
      break;
    case EXP_LET:
      if (e->as.let.t)
        node_del(e->as.let.t);
      node_del(e->as.let.e);
      break;
    case EXP_SET:
      node_del(e->as.set.x);
      node_del(e->as.set.e);
      break;
    case EXP_BODY:
      vec_del(e->as.body.stmts, (del_t)node_del);
      node_del(e->as.body.ret);
      break;
    case EXP_TY_RECORD: vec_del(e->as.ty_record, (del_t)field_del); break;
    case EXP_TY_VARIANT: vec_del(e->as.ty_variant, (del_t)field_del); break;
    case EXP_TY_PTR: node_del(e->as.ty_ptr); break;
    case EXP_FPTR:
      vec_del(e->as.fptr.args, (del_t)node_del);
      node_del(e->as.fptr.ret);
      break;
    case EXP_ID: break;
    case EXP_NUM: break;
    case EXP_STR: break;
    case EXP_UOP: node_del(e->as.uop.e); break;
    case EXP_BOP:
      node_del(e->as.bop.l);
      node_del(e->as.bop.r);
      break;
    case EXP_PROJ: node_del(e->as.proj.e); break;
    case EXP_INDEX:
      node_del(e->as.index.e);
      node_del(e->as.index.i);
      break;
    case EXP_CALL:
      node_del(e->as.call.f);
      vec_del(e->as.call.args, (del_t)node_del);
      break;
    case EXP_RECORD:
      vec_del(e->as.record, (del_t)field_del);
      break;
    case EXP_VARIANT: node_del(e->as.variant.e); break;
    case EXP_MATCH:
      node_del(e->as.match.e);
      vec_del(e->as.match.arms, (del_t)node_del);
      break;
    case EXP_ARM: node_del(e->as.arm.e); break;
    EXP_TODO
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
    UOP_TODO
  }
}

void bop_pp(FILE *fp, bop_t op) {
  switch (op) {
    case BOP_ADD: fputc('+', fp); break;
    case BOP_MUL: fputc('*', fp); break;
    case BOP_SUB: fputc('-', fp); break;
    BOP_TODO
  }
}

void node_pp_(stab_t t, FILE *fp, node_t e, int lvl);
void node_pp_rhs(stab_t t, FILE *fp, node_t e, int lvl, int semi) {
  switch (e->is) {
    case EXP_BODY:
      fputc('(', fp);
      node_pp_newline(fp, lvl + 2);
      node_pp_(t, fp, e, lvl + 2);
      node_pp_newline(fp, lvl);
      if (semi)
        fprintf(fp, ");");
      else
        fputc(')', fp);
      node_pp_newline(fp, lvl);
      break;
    EXP_TODO
    default:
      node_pp_(t, fp, e, lvl);
      if (semi)
        fputc(';', fp);
      node_pp_newline(fp, lvl);
  }
}

void node_pp_field(stab_t t, FILE *fp, pair_t id_e, int lvl, char const *sep) {
  fprintf(fp, "%s", stab_get(t, (sid_t)(size_t)(id_e->a)));
  if (id_e->b) {
    fprintf(fp, "%s", sep);
    node_pp_(t, fp, id_e->b, lvl);
  }
}

#define PP_VEC_COMMA_SEP(x, v, ...) \
  VEC_FOR(x, v, __PP_VEC_COMMA_SEP_i, __PP_VEC_COMMA_SEP_n) { \
    __VA_ARGS__ \
    if (__PP_VEC_COMMA_SEP_i < __PP_VEC_COMMA_SEP_n - 1) \
      fprintf(fp, ", "); \
  }

void node_pp_(stab_t t, FILE *fp, node_t e, int lvl) {
  switch (e->is) {
    case EXP_PRGM:
      VEC_FOREACH(func, e->as.prgm) {
        node_pp_(t, fp, func, lvl);
        node_pp_newline(fp, lvl);
      }
      break;
    case EXP_FUNC:
      fprintf(fp, "%s(", stab_get(t, e->as.func.f));
      PP_VEC_COMMA_SEP(arg, e->as.func.args,
        node_pp_field(t, fp, arg, lvl, ": ");
      )
      fprintf(fp, ")");
      if (e->as.func.ret) {
        fprintf(fp, ": ");
        node_pp_(t, fp, e->as.func.ret, lvl);
      }
      fprintf(fp, " =");
      node_pp_newline(fp, lvl + 2);
      node_pp_(t, fp, e->as.func.body, lvl + 2);
      break;
    case EXP_LET:
      fprintf(fp, "%s", stab_get(t, e->as.let.x));
      if (e->as.let.t) {
        fprintf(fp, ": ");
        node_pp_(t, fp, e->as.let.t, lvl);
      }
      fprintf(fp, " = ");
      node_pp_rhs(t, fp, e->as.let.e, lvl, 1);
      break;
    case EXP_SET:
      node_pp_(t, fp, e->as.set.x, lvl);
      fprintf(fp, " := ");
      node_pp_rhs(t, fp, e->as.set.e, lvl, 1);
      break;
    case EXP_BODY:
      VEC_FOREACH(stmt, e->as.body.stmts)
        node_pp_(t, fp, stmt, lvl);
      node_pp_(t, fp, e->as.body.ret, lvl);
      break;
    case EXP_TY_RECORD:
      fputc('{', fp);
      PP_VEC_COMMA_SEP(field, e->as.ty_record,
        node_pp_field(t, fp, field, lvl, ": ");
      )
      fputc('}', fp);
      break;
    case EXP_TY_VARIANT:
      fputc('<', fp);
      PP_VEC_COMMA_SEP(field, e->as.ty_variant,
        node_pp_field(t, fp, field, lvl, ": ");
      )
      fputc('>', fp);
      break;
    case EXP_TY_PTR:
      fputc('*', fp);
      node_pp_(t, fp, e->as.ty_ptr, lvl);
      break;
    case EXP_FPTR:
      fputc('(', fp);
      PP_VEC_COMMA_SEP(arg, e->as.fptr.args,
        node_pp_(t, fp, arg, lvl);
      )
      fprintf(fp, ") -> ");
      node_pp_(t, fp, e->as.fptr.ret, lvl);
      break;
    case EXP_ID: fprintf(fp, "%s", stab_get(t, e->as.id)); break;
    case EXP_NUM: fprintf(fp, "%d", e->as.num); break;
    case EXP_STR: fprintf(fp, "%s", stab_get(t, e->as.str)); break;
    case EXP_UOP:
      fputc('(', fp);
      uop_pp(fp, e->as.uop.op);
      node_pp_(t, fp, e->as.uop.e, lvl);
      fputc(')', fp);
      break;
    case EXP_BOP:
      fputc('(', fp);
      node_pp_(t, fp, e->as.bop.l, lvl);
      fputc(' ', fp);
      bop_pp(fp, e->as.bop.op);
      fputc(' ', fp);
      node_pp_(t, fp, e->as.bop.r, lvl);
      fputc(')', fp);
      break;
    case EXP_PROJ:
      fputc('(', fp);
      node_pp_(t, fp, e->as.proj.e, lvl);
      fprintf(fp, ".%s)", stab_get(t, e->as.proj.id));
      break;
    case EXP_INDEX:
      fputc('(', fp);
      node_pp_(t, fp, e->as.index.e, lvl);
      fputc('[', fp);
      node_pp_(t, fp, e->as.index.i, lvl);
      fprintf(fp, "])");
      break;
    case EXP_CALL:
      node_pp_(t, fp, e->as.call.f, lvl);
      fputc('(', fp);
      PP_VEC_COMMA_SEP(arg, e->as.call.args,
        node_pp_(t, fp, arg, lvl);
      )
      fputc(')', fp);
      break;
    case EXP_RECORD:
      fputc('{', fp);
      PP_VEC_COMMA_SEP(field, e->as.record,
        node_pp_field(t, fp, field, lvl, " = ");
      )
      fputc('}', fp);
      break;
    case EXP_VARIANT:
      fprintf(fp, "%s@(", stab_get(t, e->as.variant.name));
      node_pp_(t, fp, e->as.variant.e, lvl);
      fputc(')', fp);
      break;
    case EXP_MATCH:
      fprintf(fp, "case ");
      node_pp_(t, fp, e->as.match.e, lvl);
      node_pp_newline(fp, lvl);
      VEC_FOREACH(arm, e->as.match.arms)
        node_pp_(t, fp, arm, lvl);
      fprintf(fp, "end");
      node_pp_newline(fp, lvl);
      break;
    case EXP_ARM:
      fprintf(fp, "| %s@%s -> ", 
        stab_get(t, e->as.arm.ctr),
        stab_get(t, e->as.arm.x));
      node_pp_rhs(t, fp, e->as.arm.e, lvl, 0);
      break;
    EXP_TODO
  }
}

#undef PP_VEC_COMMA_SEP

void node_pp(stab_t t, FILE *fp, node_t e) { node_pp_(t, fp, e, 0); }

int node_is_ty(node_t e) {
  switch (e->is) {
    case EXP_TY_RECORD:
    case EXP_TY_VARIANT:
    case EXP_TY_PTR:
    case EXP_FPTR:
    case EXP_ID: 
      return 1;
    default:
      return 0;
    TY_TODO
  }
}

int node_is_tm(node_t e) {
  switch (e->is) {
    case EXP_LET:
    case EXP_SET:
    case EXP_BODY:
    case EXP_ID:
    case EXP_NUM:
    case EXP_STR:
    case EXP_UOP:
    case EXP_BOP:
    case EXP_PROJ:
    case EXP_INDEX:
    case EXP_CALL:
    case EXP_RECORD:
    case EXP_VARIANT:
    case EXP_MATCH:
      return 1;
    default:
      return 0;
    EXP_TODO
  }
}
