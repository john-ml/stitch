#include "pair.h"
#include <stdlib.h>
#include <stdio.h>

pair_t pair_new(any_t a, any_t b) {
  pair_t p = malloc(sizeof(*p));
  p->a = a;
  p->b = b;
  return p;
}

