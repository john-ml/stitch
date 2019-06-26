#include "vec.h"
#include <stdio.h>

vec_t vec_create(int cap) {
  vec_t v = malloc(sizeof(*v));
  v->len = 0;
  v->cap = cap;
  v->data = malloc(8 * cap);
  return v;
}

vec_t vec_new() { puts("vec_new"); return vec_create(8); }

vec_t vec_sing(void *x) {
  printf("vec_sing %p\n", x);
  vec_t v = vec_new();
  vec_add(v, x);
  return v;
}

int vec_len(vec_t v) { return v->len; }
int vec_cap(vec_t v) { return v->cap; }

void vec_realloc(vec_t v) { v->data = realloc(v->data, 8*v->cap); }

void vec_expand(vec_t v) { v->cap *= 2; vec_realloc(v); }
void vec_contract(vec_t v) { v->cap /= 2; vec_realloc(v); }

void vec_add(vec_t v, void *x) {
  printf("vec_add %p\n", x);
  if (v->len == v->cap)
    vec_expand(v);
  v->data[v->len++] = x;
}

void *vec_pop(vec_t v) {
  if (v->len < v->cap / 4)
    vec_contract(v);
  return v->data[--v->len];
}

void vec_free(vec_t v) {
  free(v->data);
  free(v);
}
