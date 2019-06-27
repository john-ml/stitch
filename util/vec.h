// Non-owning resizable array
#ifndef VEC_INCLUDED_H
#define VEC_INCLUDED_H

#include "misc.h"
#include <stddef.h>

// A vector 'magically' behaves like a normal pointer
// For any vec_t v, v[i] = ith element of v
typedef any_t *vec_t;

// Create a vector with the given initial capacity
vec_t vec_create(int cap);

// []
vec_t vec_new();

// [x]
vec_t vec_sing(any_t x);

// Number of elements in v
int vec_len(vec_t v);

// The storage space allocated in v
int vec_cap(vec_t v);

// vec_add, vec_pop, and vec_ins require *v because v may be moved to a
// different location during resizing

// Add to back
void vec_add(vec_t *v, any_t x);

// Remove from back
any_t vec_pop(vec_t *v);

// Force v[i] = x, expanding v as needed
// Return old value of v[i]
any_t vec_put(vec_t *v, int i, any_t x);

// Same as vec_put, but initialize any blank entries to null
any_t vec_put_(vec_t *v, int i, any_t x, any_t null);

// f frees items
void vec_del(vec_t v, del_t f);

#define VEC_FOR(x, v, i, n) \
  for ( \
    any_t \
      i = (any_t)0, \
      n = (any_t)(size_t)vec_len(v), \
      x; \
    x = (v)[(size_t)i], (size_t)i < (size_t)n; \
    ++i)

#define VEC_REV_FOR(x, v, i) \
  for ( \
    any_t \
      i = (any_t)(size_t)vec_len(v), \
      x; \
    x = (v)[(size_t)i-1], (size_t)i; \
    --i)

#define VEC_FOREACH(x, v) \
  VEC_FOR(x, v, \
    __VEC_FOREACH_i##__LINE__, \
    __VEC_FOREACH_n##__LINE__)

#define VEC_REV_FOREACH(x, v) \
  VEC_REV_FOR(x, v, \
    __VEC_REV_FOREACH_i##__LINE__)

#define VEC_FOR2(x1, v1, i1, n1, x2, v2, i2, n2) \
  for ( \
    any_t \
      i1 = (any_t)0, \
      i2 = (any_t)0, \
      n1 = (any_t)(size_t)vec_len(v1), \
      n2 = (any_t)(size_t)vec_len(v2), \
      x1, x2; \
    x1 = (v1)[(size_t)i1], \
      x2 = (v2)[(size_t)i2], \
      (size_t)i1 < (size_t)n1 && (size_t)i2 < (size_t)n2; \
    ++i1, ++i2)

#define VEC_REV_FOR2(x1, v1, i1, x2, v2, i2) \
  for ( \
    any_t \
      i1 = (any_t)(size_t)vec_len(v1), \
      i2 = (any_t)(size_t)vec_len(v2), \
      x1, x2; \
    x1 = (v1)[(size_t)i1-1], \
      x2 = (v2)[(size_t)i2-1], \
      (size_t)i1 && (size_t)i2; \
    --i1, --i2)

#define VEC_FOREACH2(x1, v1, x2, v2) \
  VEC_FOR2(x1, v1, \
    __VEC_FOREACH_i1##__LINE__, \
    __VEC_FOREACH_n1##__LINE__, \
    x2, v2, \
    __VEC_FOREACH_i2##__LINE__, \
    __VEC_FOREACH_n2##__LINE__)

#define VEC_REV_FOREACH2(x1, v1, x2, v2) \
  VEC_REV_FOR2( \
    x1, v1, __VEC_REV_FOREACH_i1##__LINE__, \
    x2, v2, __VEC_REV_FOREACH_i2##__LINE__)

#endif // VEC_INCLUDED_H
