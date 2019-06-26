// Non-owning resizable array
#ifndef VEC_INCLUDED_H
#define VEC_INCLUDED_H

#include <stdlib.h>

typedef struct {
  int len;
  int cap;
  void **data;
} *vec_t;

vec_t vec_create(int cap);

vec_t vec_new();
vec_t vec_sing(void *x);

int vec_len(vec_t v);
int vec_cap(vec_t v);

void vec_realloc(vec_t v);

void vec_expand(vec_t v);
void vec_contract(vec_t v);

void vec_add(vec_t v, void *x);
void *vec_pop(vec_t v);

// Does NOT free items
void vec_free(vec_t v);

#endif // VEC_INCLUDED_H
