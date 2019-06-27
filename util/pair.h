// Heap-allocated pair
#ifndef PAIR_INCLUDED_H
#define PAIR_INCLUDED_H

#include "misc.h"

typedef struct {
  any_t a;
  any_t b;
} *pair_t;

pair_t pair_new(any_t a, any_t b);
void pair_del(pair_t p, del_t f, del_t g);

#endif // PAIR_INCLUDED_H
