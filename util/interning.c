#include "interning.h"
#include <stdlib.h>
#include <string.h>

stab_t stab_new(arena_p *a) {
  return (stab_t) { .strs = vec_new(a, sizeof(char *)) };
}

sid_t stab_add(arena_p *a, stab_t *t, char const *s) {
  int i = 0;
  for (; i < vec_len(t->strs); ++i)
    if (strcmp(s, t->strs[i]) == 0)
      return i;
  size_t n = strlen(s) + 1;
  char *s1 = arena_alloc(a, n);
  memcpy(s1, s, n);
  vec_add(a, (vec_p)&t->strs, &s1);
  return i;
}

char const *stab_get(stab_t t, sid_t i) { return t.strs[i]; }
