#include "vec.h"
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

// Stuff length / capacity information in before the 'actual vector'
typedef struct raw_t {
  size_t wid;
  size_t len;
  size_t cap;
  char data[0];
} *raw_p;

// Convert to and from raw_p and vec_p
vec_p of_raw(raw_p v) { return (vec_p)&v->data; }
raw_p to_raw(vec_p v) { 
  return (raw_p)((char *)v - offsetof(struct raw_t, data)); 
}

vec_p vec_create(arena_p *a, size_t wid, size_t cap) {
  raw_p v = arena_alloc(a, sizeof(*v) + wid*cap);
  v->wid = wid;
  v->len = 0;
  v->cap = cap;
  return of_raw(v);
}

vec_p vec_new(arena_p *a, size_t wid) { return vec_create(a, wid, 8); }

vec_p vec_sing(arena_p *a, size_t wid, void *x) {
  vec_p v = vec_new(a, wid);
  vec_add(a, &v, x);
  return v;
}

size_t vec_wid(vec_p v) { return to_raw(v)->wid; }
size_t vec_len(vec_p v) { return to_raw(v)->len; }
size_t vec_cap(vec_p v) { return to_raw(v)->cap; }

void vec_realloc(arena_p *a, vec_p *v) { 
  raw_p w = to_raw(*v);
  raw_p u = arena_alloc(a, sizeof(*u) + w->wid*w->cap);
  memcpy(u, w, sizeof(*w) + w->wid*w->len);
  *v = of_raw(u);
}

// // Force v[i] = x, expanding v as needed
// // Return old value of v[i] in out
// void vec_put(arena_p *a, vec_p *v, size_t i, void *x, void *out);
// 
// // Same as vec_put, but initialize any blank entries to null
// void vec_put_(arena_p *a, vec_p *v, size_t i, void *x, void *out, void *null);

void vec_expand(arena_p *a, vec_p *v) { to_raw(*v)->cap *= 2; vec_realloc(a, v); }

void vec_add(arena_p *a, vec_p *v, void *x) {
  if (vec_len(*v) == vec_cap(*v))
    vec_expand(a, v);
  raw_p w = to_raw(*v);
  memcpy((char *)*v + w->wid*w->len, x, w->wid);
  ++w->len;
}

void vec_pop(arena_p *a, vec_p *v, void *out) {
  raw_p w = to_raw(*v);
  assert(w->len > 0);
  --w->len;
  memcpy(out, (char *)*v + w->wid*w->len, w->wid);
}

// any_t vec_put(arena_p *a, vec_p *v, size_t i, void *x, void *out) {
//   if (i >= to_raw(*v)->cap) {
//     to_raw(*v)->cap = i + 1;
//     vec_realloc(v);
//   }
//   raw_p w = to_raw(*v);
//   any_t old = w->data[i];
//   w->data[i] = x;
//   if (i >= w->len)
//     w->len = i + 1;
//   return old;
// }
// 
// any_t vec_put_(vec_p *v, size_t i, any_t x, any_t null) {
//   size_t old_len = vec_len(*v);
//   any_t res = vec_put(v, i, x);
//   if (old_len < vec_len(*v));
//   for (size_t i = old_len; i < vec_len(*v) - 1; ++i)
//     (*v)[i] = null;
//   return res;
// }
