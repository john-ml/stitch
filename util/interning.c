#include "interning.h"
#include "vec.h"
#include <stdlib.h>
#include <string.h>

stab_t stab_new() { return (stab_t) { .strs = vec_new() }; }

sid_t stab_add(stab_t *t, char const *s) {
  for (int i = 0; i < vec_len(t->strs); ++i)
    if (strcmp(s, t->strs[i]) == 0)
      return i;
  vec_add(&t->strs, strdup(s));
}

char const *stab_get(stab_t t, sid_t i) { return t.strs[i]; }

void stab_del(stab_t t) { vec_del(t.strs, free); }
