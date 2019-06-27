// Non-owning resizable array
#ifndef VEC_INCLUDED_H
#define VEC_INCLUDED_H

#include "misc.h"

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
void vec_ins(vec_t *v, int i, any_t x);

// f frees items
void vec_del(vec_t v, del_t f);

#endif // VEC_INCLUDED_H
