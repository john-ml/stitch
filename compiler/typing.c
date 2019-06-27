#include "typing.h"
#include "vec.h"
#include "interning.h"
#include <stdio.h>
#include <stdlib.h>

// HM-style type inference/checking
// Fully annotate prgm with type information
void infer_types(node_t prgm) {
  fprintf(stderr, "infer_types not implemented\n");
  exit(1);
}

// Simple check of fully annotated program

int check_ty(stab_t stab, node_t const t1, node_t const t2) {
  if (t1->is != t2->is) {
    fprintf(stderr, "Expected ");
    node_pp(stab, stderr, t2);
    fprintf(stderr, " but got ");
    node_pp(stab, stderr, t1);
    fprintf(stderr, "\n");
    return 1;
  }
  switch (t1->is) {
    case EXP_TY_RECORD:
      if (int l1 = vec_len(t1->as.ty_record),
          l2 = vec_len(t2->as.ty_record),
          l1 != l2) {
        fprintf(stderr,
          "Record width mismatch: expected %d but got %d\n",
          l1, l2);
        return 1;
      }
      VEC_FOREACH2(f1, t1->as.ty_record, f2, t2->as.ty_record) {
        if (f1->a != f2->a) {
          fprintf(stderr,
            "Field name mismatch: expected %s but got %s\n",
            stab_get(stab, (stab_t)(size_t)f1->a),
            stab_get(stab, (stab_t)(size_t)f2->a));
          return 1;
        }
      }
      VEC_FOREACH2(f1, t1->as.ty_record, f2, t2->as.ty_record)
        if (check_ty(stab, f1->b, f2->b))
          return 1;
    break;
    case EXP_TY_VARIANT:
      if (int l1 = vec_len(t1->as.ty_variant),
          l2 = vec_len(t2->as.ty_variant),
          l1 != l2) {
        fprintf(stderr,
          "Variant width mismatch: expected %d but got %d\n",
          l1, l2);
        return 1;
      }
      VEC_FOREACH2(f1, t1->as.ty_variant, f2, t2->as.ty_variant) {
        if (f1->a != f2->a) {
          fprintf(stderr,
            "Variant tag mismatch: expected %s but got %s\n",
            stab_get(stab, (stab_t)(size_t)f1->a),
            stab_get(stab, (stab_t)(size_t)f2->a));
          return 1;
        }
      }
      VEC_FOREACH2(f1, t1->as.ty_variant, f2, t2->as.ty_variant)
        if (check_ty(stab, f1->b, f2->b))
          return 1;
    break;
    case EXP_TY_PTR: return check_ty(stab, t1->as.ty_ptr, t2->as.ty_ptr);
    case EXP_FPTR:
      VEC_FOREACH2(arg1, t1->as.fptr.args, arg2, t2->as.fptr.args)
        if (check_ty(stab, arg1, arg2))
          return 1;
      return check_ty(t1->as.fptr.ret, t2->as.fptr.ret);
    break;
    case EXP_ID: return t1->as.id == t2->as.id;
    default: assert(0);
  }
  return 0;
}

void check_exp(stab_t stab, vec_t *env, node_t const exp, node_t const ty) {
  switch (e->is) {
    case EXP_BODY:
      // case EXP_LET:
      //   if (e->as.let.t)
      //     node_del(e->as.let.t);
      //   node_del(e->as.let.e);
      // break;
      // case EXP_SET:
      //   node_del(e->as.set.x);
      //   node_del(e->as.set.e);
      // break;
      vec_del(e->as.body.stmts, (del_t)node_del);
      node_del(e->as.body.ret);
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
    default: assert(0);
  } 
}

void check_func(stab_t stab, vec_t *env, node_t const func) {
  assert(func->is == EXP_FUNC);
  // Save old env + extend env with args
  vec_t clobbers = vec_create(vec_len(func->as.func.args));
  VEC_FOREACH(arg, func->as.func.args)
    vec_add(&clobbers, vec_put(env, arg->a, arg->b));
  // Typecheck the body against return type
  check_exp(stab, env, func->as.func.body, func->as.func.ret);
  // Restore old env
  VEC_FOR(arg, func->as.func.args, i, n)
    (void) vec_put(env, arg->a, clobbers[i]);
  vec_del(clobbers, no_del);
}

void check_types(stab_t stab, node_t const prgm) {
  assert(prgm->is == EXP_PRGM);
  vec_t env = vec_new();
  // Populate environment with all functions for mutual recursion
  VEC_FOREACH(func, e->as.prgm)
    (void) vec_put(&env, func->as.func.f, func);
  // Check each function
  VEC_FOREACH(func, e->as.prgm)
    check_func(stab, &env, func);
  vec_del(env, no_del);
}