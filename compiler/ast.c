#include "ast.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

// Make ADT constructors
#define MK_SING(ty, ctr, field, e) \
  node_p __res = arena_alloc(a, sizeof(*__res)); \
  *__res = (struct ty){.is = ctr, .as = {.field = e}}; \
  return __res
#define MK(ty, ctr, field, ...) \
  node_p __res = arena_alloc(a, sizeof(*__res)); \
  *__res = (struct ty){.is = ctr, .as = {.field = {__VA_ARGS__}}}; \
  return __res

node_p node_prgm(arena_p *a, VEC(node_p) funcs) { MK_SING(node_t, EXP_PRGM, prgm, funcs); }

node_p node_func(arena_p *a, sid_t f, VEC(node_p) args, node_p ret, node_p body) {
  MK(node_t, EXP_FUNC, func, .f = f, .args = args, .ret = ret, .body = body);
}

node_p node_let(arena_p *a, sid_t x, node_p t, node_p e) {
  MK(node_t, EXP_LET, let, .x = x, .t = t, .e = e);
}

node_p node_set(arena_p *a, node_p x, node_p e) { MK(node_t, EXP_SET, set, .x = x, .e = e); }

node_p node_body(arena_p *a, VEC(node_p) stmts, node_p ret) {
  MK(node_t, EXP_BODY, body, .stmts = stmts, .ret = ret);
}

node_p node_py_record(arena_p *a, VEC(node_p) fields) {
  MK_SING(node_t, EXP_TY_RECORD, ty_record, fields);
}

node_p node_py_variant(arena_p *a, VEC(node_p) fields) {
  MK_SING(node_t, EXP_TY_VARIANT, ty_variant, fields);
}

node_p node_py_ptr(arena_p *a, node_p ty) { MK_SING(node_t, EXP_TY_PTR, ty_ptr, ty); }

node_p node_fptr(arena_p *a, VEC(node_p) args, node_p ret) {
  MK(node_t, EXP_FPTR, fptr, .args = args, .ret = ret);
}

node_p node_uvar(arena_p *a, uf_id_t uvar) { MK_SING(node_t, EXP_UVAR, uvar, uvar); }

node_p node_id(arena_p *a, sid_t id) { MK_SING(node_t, EXP_ID, id, id); }

node_p node_str(arena_p *a, sid_t str) { MK_SING(node_t, EXP_STR, str, str); }

node_p node_num(arena_p *a, int num) { MK_SING(node_t, EXP_NUM, num, num); }

node_p node_uop(arena_p *a, uop_t op, node_p e) {
  MK(node_t, EXP_UOP, uop, .op = op, .e = e);
}

node_p node_bop(arena_p *a, node_p l, bop_t op, node_p r) {
  MK(node_t, EXP_BOP, bop, .l = l, .op = op, .r = r);
}

node_p node_proj(arena_p *a, node_p e, sid_t id) {
  MK(node_t, EXP_PROJ, proj, .e = e, .id = id);
}

node_p node_index(arena_p *a, node_p e, node_p i) {
  MK(node_t, EXP_INDEX, index, .e = e, .i = i);
}

node_p node_call(arena_p *a, node_p f, VEC(node_p) args) {
  MK(node_t, EXP_CALL, call, .f = f, .args = args);
}

node_p node_record(arena_p *a, VEC(node_p) fields) {
  MK_SING(node_t, EXP_RECORD, record, fields);
}

node_p node_variant(arena_p *a, sid_t name, node_p e) {
  MK(node_t, EXP_VARIANT, variant, .name = name, .e = e);
}

node_p node_match(arena_p *a, node_p e, VEC(node_p) arms) {
  MK(node_t, EXP_MATCH, match, .e = e, .arms = arms);
}

node_p node_arm(arena_p *a, sid_t ctr, sid_t x, node_p e) {
  MK(node_t, EXP_ARM, arm, .ctr = ctr, .x = x, .e = e);
}

node_p node_vec(arena_p *a, VEC(node_p) v) { MK_SING(node_t, EXP_VEC, vec, v); }

node_p node_id_e(arena_p *a, sid_t id, node_p e) {
  MK(node_t, EXP_ID_E, id_e, .id = id, .e = e);
}

NODE_TODO

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

void node_pp_(stab_t t, FILE *fp, node_p e, int lvl);
void node_pp_rhs(stab_t t, FILE *fp, node_p e, int lvl, int semi) {
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

void node_pp_id_e(stab_t t, FILE *fp, node_p id_e, int lvl, char const *sep) {
  assert(id_e->is == EXP_ID_E);
  fprintf(fp, "%s", stab_get(t, id_e->as.id_e.id));
  if (id_e->as.id_e.e) {
    fprintf(fp, "%s", sep);
    node_pp_(t, fp, id_e->as.id_e.e, lvl);
  }
}

#define PP_VEC_COMMA_SEP(t, x, v, ...) \
  VEC_FOR(t, x, v, __PP_VEC_COMMA_SEP_i, __PP_VEC_COMMA_SEP_n) { \
    __VA_ARGS__ \
    if (__PP_VEC_COMMA_SEP_i < __PP_VEC_COMMA_SEP_n - 1) \
      fprintf(fp, ", "); \
  }

void node_pp_(stab_t t, FILE *fp, node_p e, int lvl) {
  switch (e->is) {
    case EXP_PRGM: {
      VEC_FOREACH(node_p, func, e->as.prgm) {
        node_pp_(t, fp, func, lvl);
        node_pp_newline(fp, lvl);
      }
    } break;
    case EXP_FUNC: {
      fprintf(fp, "%s(", stab_get(t, e->as.func.f));
      PP_VEC_COMMA_SEP(node_p, arg, e->as.func.args,
        node_pp_id_e(t, fp, arg, lvl, " ");
      )
      fprintf(fp, ")");
      if (e->as.func.ret) {
        fprintf(fp, " ");
        node_pp_(t, fp, e->as.func.ret, lvl);
      }
      fprintf(fp, ":");
      node_pp_newline(fp, lvl + 2);
      node_pp_(t, fp, e->as.func.body, lvl + 2);
    } break;
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
    case EXP_BODY: {
      VEC_FOREACH(node_p, stmt, e->as.body.stmts)
        node_pp_(t, fp, stmt, lvl);
      node_pp_(t, fp, e->as.body.ret, lvl);
    } break;
    case EXP_TY_RECORD: {
      fputc('{', fp);
      PP_VEC_COMMA_SEP(node_p, field, e->as.ty_record,
        node_pp_id_e(t, fp, field, lvl, " ");
      )
      fputc('}', fp);
    } break;
    case EXP_TY_VARIANT: {
      fputc('<', fp);
      PP_VEC_COMMA_SEP(node_p, field, e->as.ty_variant,
        node_pp_id_e(t, fp, field, lvl, " ");
      )
      fputc('>', fp);
    } break;
    case EXP_TY_PTR:
      fputc('*', fp);
      node_pp_(t, fp, e->as.ty_ptr, lvl);
      break;
    case EXP_FPTR: {
      fputc('(', fp);
      PP_VEC_COMMA_SEP(node_p, arg, e->as.fptr.args,
        node_pp_(t, fp, arg, lvl);
      )
      fprintf(fp, ") -> ");
      node_pp_(t, fp, e->as.fptr.ret, lvl);
    } break;
    case EXP_UVAR: fprintf(fp, "?%d", e->as.uvar); break;
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
    case EXP_CALL: {
      node_pp_(t, fp, e->as.call.f, lvl);
      fputc('(', fp);
      PP_VEC_COMMA_SEP(node_p, arg, e->as.call.args,
        node_pp_(t, fp, arg, lvl);
      )
      fputc(')', fp);
    } break;
    case EXP_RECORD: {
      fputc('{', fp);
      PP_VEC_COMMA_SEP(node_p, field, e->as.record,
        node_pp_id_e(t, fp, field, lvl, " ");
      )
      fputc('}', fp);
    } break;
    case EXP_VARIANT:
      fprintf(fp, "%s@(", stab_get(t, e->as.variant.name));
      node_pp_(t, fp, e->as.variant.e, lvl);
      fputc(')', fp);
      break;
    case EXP_MATCH: {
      fprintf(fp, "case ");
      node_pp_(t, fp, e->as.match.e, lvl);
      node_pp_newline(fp, lvl);
      VEC_FOREACH(node_p, arm, e->as.match.arms)
        node_pp_(t, fp, arm, lvl);
      fprintf(fp, "end");
      node_pp_newline(fp, lvl);
    } break;
    case EXP_ARM:
      fprintf(fp, "| %s@%s -> ", 
        stab_get(t, e->as.arm.ctr),
        stab_get(t, e->as.arm.x));
      node_pp_rhs(t, fp, e->as.arm.e, lvl, 0);
      break;
    case EXP_ID_E: assert(0);
    NODE_TODO
  }
}

#undef PP_VEC_COMMA_SEP

void node_pp(stab_t t, FILE *fp, node_p e) { node_pp_(t, fp, e, 0); }

int node_is_ty(node_p e) {
  switch (e->is) {
    case EXP_TY_RECORD:
    case EXP_TY_VARIANT:
    case EXP_TY_PTR:
    case EXP_FPTR:
    case EXP_UVAR: 
    case EXP_ID: 
      return 1;
    default:
      return 0;
    TY_TODO
  }
}

int node_is_tm(node_p e) {
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
