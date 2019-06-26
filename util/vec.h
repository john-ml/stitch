// Non-owning resizable array
#ifndef VEC_INCLUDED_H
#define VEC_INCLUDED_H

#include "misc.h"

typedef any_t *vec_t;

vec_t vec_create(int cap);

vec_t vec_new();
vec_t vec_sing(any_t x);

int vec_len(vec_t v);
int vec_cap(vec_t v);

void vec_add(vec_t *v, any_t x);
any_t vec_pop(vec_t *v);

// f frees items
void vec_free(vec_t v, free_t f);

#endif // VEC_INCLUDED_H
