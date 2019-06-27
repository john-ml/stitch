#include "typing.h"
#include "vec.h"
#include <stdio.h>
#include <stdlib.h>

// HM-style type inference/checking
// Fully annotate prgm with type information
void infer_types(node_t prgm) {
  fprintf(stderr, "infer_types not implemented\n");
  exit(1);
}

// Simple check of fully annotated program

void check_exp(vec_t *env, node_t const exp, node_t const ty) {
  switch (e->is) {
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
    default: assert(0);
  } 
}

void check_func(vec_t *env, node_t const func) {
  assert(func->is == EXP_FUNC);
  // Save old env + extend env with args
  vec_t clobbers = vec_create(vec_len(func->as.func.args));
  VEC_FOREACH(arg, func->as.func.args)
    vec_add(&clobbers, vec_put(env, arg->a, arg->b));
  // Typecheck the body against return type
  check_exp(env, func->as.func.body, func->as.func.ret);
  // Restore old env
  VEC_FOR(arg, func->as.func.args, i, n)
    (void) vec_put(env, arg->a, clobbers[i]);
  vec_del(clobbers, no_del);
}

void check_types(node_t const prgm) {
  assert(prgm->is == EXP_PRGM);
  vec_t env = vec_new();
  // Populate environment with all functions for mutual recursion
  VEC_FOREACH(func, e->as.prgm)
    (void) vec_put(&env, func->as.func.f, func);
  // Check each function
  VEC_FOREACH(func, e->as.prgm)
    check_func(&env, func);
  vec_del(env, no_del);
}
