#include "pair.h"
#include <stdlib.h>
#include <stdio.h>

pair_t pair_new(void *a, void *b) {
  pair_t p = malloc(sizeof(*p));
  p->a = a;
  p->b = b;
  return p;
}

