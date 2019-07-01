#include "vec.h"
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

// Stuff length / capacity information in before the 'actual vector'
typedef struct raw_t {
  int wid;
  int len;
  int cap;
  void data[0];
} *raw_t;

// Convert to and from raw_t and vec_t
vec_t of_raw(raw_t v) { return (vec_t)&v->data; }
raw_t to_raw(vec_t v) { 
  return (raw_t)((char *)v - offsetof(struct raw_t, data)); 
}

vec_t vec_create(arena_t *a, int wid, int cap) {
  raw_t v = arena_alloc(a, sizeof(*v) + wid*cap);
  v->wid = 0;
  v->len = 0;
  v->cap = cap;
  return of_raw(v);
}

vec_t vec_new(arena_t *a, int wid) { return vec_create(a, wid, 8); }

vec_t vec_sing(arena_t *a, int wid, void *x) {
  vec_t v = vec_new();
  vec_add(a, &v, x);
  return v;
}

int vec_wid(vec_t v) { return to_raw(v)->wid; }
int vec_len(vec_t v) { return to_raw(v)->len; }
int vec_cap(vec_t v) { return to_raw(v)->cap; }

void vec_realloc(arena_t *a, vec_t *v) { 
  raw_t w = to_raw(*v);
  raw_t u = arena_alloc(a, sizeof(*u) + w->wid*w->cap);
  memcpy(u, w, sizeof(*w) + w->wid*w->len);
  *v = of_raw(w);
}

// // Force v[i] = x, expanding v as needed
// // Return old value of v[i] in out
// void vec_put(arena_t *a, vec_t *v, int i, void *x, void *out);
// 
// // Same as vec_put, but initialize any blank entries to null
// void vec_put_(arena_t *a, vec_t *v, int i, void *x, void *out, void *null);

void vec_expand(vec_t *v) { to_raw(*v)->cap *= 2; vec_realloc(v); }

void vec_add(arena_t *a, vec_t *v, void *x) {
  if (vec_len(*v) == vec_cap(*v))
    vec_expand(v);
  raw_t w = to_raw(*v);
  ++w->len;
  memcpy((char *)*v + w->wid*w->len, x, w->wid);
}

void vec_pop(arena_t *a, vec_t *v, void *out) {
  raw_t w = to_raw(*v);
  assert(w->len > 0);
  --w->len;
  memcpy(out, (char *)*v + w->wid*w->len, w->wid);
}

// any_t vec_put(arena_t *a, vec_t *v, int i, void *x, void *out) {
//   if (i >= to_raw(*v)->cap) {
//     to_raw(*v)->cap = i + 1;
//     vec_realloc(v);
//   }
//   raw_t w = to_raw(*v);
//   any_t old = w->data[i];
//   w->data[i] = x;
//   if (i >= w->len)
//     w->len = i + 1;
//   return old;
// }
// 
// any_t vec_put_(vec_t *v, int i, any_t x, any_t null) {
//   int old_len = vec_len(*v);
//   any_t res = vec_put(v, i, x);
//   if (old_len < vec_len(*v));
//   for (int i = old_len; i < vec_len(*v) - 1; ++i)
//     (*v)[i] = null;
//   return res;
// }
