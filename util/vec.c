#include "vec.h"
#include <stdlib.h>
#include <stddef.h>

// Stuff length / capacity information in before the 'actual vector'
typedef struct raw_t {
  int len;
  int cap;
  any_t data[0];
} *raw_t;

// Convert to and from raw_t and vec_t
vec_t of_raw(raw_t v) { return (vec_t)&v->data; }
raw_t to_raw(vec_t v) { 
  return (raw_t)((char *)v - offsetof(struct raw_t, data)); 
}

vec_t vec_create(int cap) {
  raw_t v = malloc(sizeof(*v) + cap*sizeof(any_t));
  v->len = 0;
  v->cap = cap;
  return of_raw(v);
}

vec_t vec_new() { return vec_create(8); }

void vec_del(vec_t v, del_t f) {
  raw_t w = to_raw(v);
  for (int i = w->len; i--;)
    f(v[i]);
  free(w);
}

vec_t vec_sing(any_t x) {
  vec_t v = vec_new();
  vec_add(&v, x);
  return v;
}

int vec_len(vec_t v) { return to_raw(v)->len; }
int vec_cap(vec_t v) { return to_raw(v)->cap; }

void vec_realloc(vec_t *v) { 
  raw_t w = to_raw(*v);
  w = realloc(w, sizeof(*w) + w->cap*sizeof(any_t));
  *v = of_raw(w);
}

void vec_expand(vec_t *v) { to_raw(*v)->cap *= 2; vec_realloc(v); }
void vec_contract(vec_t *v) { to_raw(*v)->cap /= 2; vec_realloc(v); }

void vec_add(vec_t *v, any_t x) {
  if (vec_len(*v) == vec_cap(*v))
    vec_expand(v);
  raw_t w = to_raw(*v);
  w->data[w->len++] = x;
}

any_t vec_pop(vec_t *v) {
  if (vec_len(*v) < vec_cap(*v) / 4)
    vec_contract(v);
  raw_t w = to_raw(*v);
  return w->data[--w->len];
}

any_t vec_put(vec_t *v, int i, any_t x) {
  if (i >= to_raw(*v)->cap) {
    to_raw(*v)->cap = i + 1;
    vec_realloc(v);
  }
  raw_t w = to_raw(*v);
  any_t old = w->data[i];
  w->data[i] = x;
  if (i >= w->len)
    w->len = i + 1;
  return old;
}

any_t vec_put_(vec_t *v, int i, any_t x, any_t null) {
  int old_len = vec_len(*v);
  any_t res = vec_put(v, i, x);
  if (old_len < vec_len(*v));
  for (int i = old_len; i < vec_len(*v) - 1; ++i)
    (*v)[i] = null;
  return res;
}
