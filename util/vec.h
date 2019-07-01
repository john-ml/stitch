// Resizable array
#ifndef VEC_INCLUDED_H
#define VEC_INCLUDED_H

#include "arena.h"
#include <stddef.h>

// A vector 'magically' behaves like a normal pointer
// For any vec_t v, v[i] = ith element of v
typedef void *vec_t;

// Create a vector with the given initial capacity
// holding types of the given size
vec_t vec_create(arena_t *a, int wid, int cap);

// []
vec_t vec_new(arena_t *a, int wid);

// [x]
vec_t vec_sing(arena_t *a, int wid, void *x);

// The size (in bytes) of each element in v
int vec_wid(vec_t v);

// Number of elements in v
int vec_len(vec_t v);

// The storage space allocated in v
int vec_cap(vec_t v);

// vec_add, vec_pop, and vec_ins require *v because v may be moved to a
// different location during resizing

// Add to back
void vec_add(arena_t *a, vec_t *v, void *x);

// Remove from back
void vec_pop(arena_t *a, vec_t *v, void *out);

// Force v[i] = x, expanding v as needed
// Return old value of v[i] in out
void vec_put(arena_t *a, vec_t *v, int i, void *x, void *out);

// Same as vec_put, but initialize any blank entries to null
void vec_put_(arena_t *a, vec_t *v, int i, void *x, void *out, void *null);

#define VEC(ty) ty *

#define VEC_FOR(t, x, v, i, n) \
  t x; \
  for ( \
    int i = 0, n = vec_len(v); \
    x = (v)[i], i < n; \
    ++i)

#define VEC_REV_FOR(t, x, v, i) \
  t x; \
  for (int i = vec_len(v); x = (v)[i-1], i; --i)

#define VEC_FOREACH(t, x, v) \
  VEC_FOR(t, x, v, \
    __VEC_FOREACH_i##__LINE__, \
    __VEC_FOREACH_n##__LINE__)

#define VEC_REV_FOREACH(t, x, v) \
  VEC_REV_FOR(t, x, v, \
    __VEC_REV_FOREACH_i##__LINE__)

#define VEC_FOR2(t1, x1, v1, i1, n1, t2, x2, v2, i2, n2) \
  t1 x1; t2 x2; \
  for ( \
    int \
      i1 = 0, i2 = 0, \
      n1 = vec_len(v1), n2 = vec_len(v2); \
    x1 = (v1)[i1], x2 = (v2)[i2], \
      i1 < n1 && i2 < n2; \
    ++i1, ++i2)

#define VEC_REV_FOR2(t1, x1, v1, i1, t2, x2, v2, i2) \
  t1 x1; t2 x2; \
  for ( \
    int \
      i1 = vec_len(v1), i2 = vec_len(v2), \
    x1 = (v1)[i1-1], x2 = (v2)[i2-1], \
      i1 && i2; \
    --i1, --i2)

#define VEC_FOREACH2(t1, x1, v1, t2, x2, v2) \
  VEC_FOR2(t1, x1, v1, \
    __VEC_FOREACH_i1##__LINE__, \
    __VEC_FOREACH_n1##__LINE__, \
    t2, x2, v2, \
    __VEC_FOREACH_i2##__LINE__, \
    __VEC_FOREACH_n2##__LINE__)

#define VEC_REV_FOREACH2(t1, x1, v1, t2, x2, v2) \
  VEC_REV_FOR2( \
    t1, x1, v1, __VEC_REV_FOREACH_i1##__LINE__, \
    t2, x2, v2, __VEC_REV_FOREACH_i2##__LINE__)

#endif // VEC_INCLUDED_H
