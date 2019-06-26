#include "uf.h"

#define OF_ID(x) ((void *)(size_t)(x))
#define TO_ID(x) ((uf_id_t)(size_t)(x))

uf_t uf_new() { return (uf_t) { .ids = vec_new(), .keys = vec_new() }; }

void uf_free(uf_t u, free_t f) { 
  vec_free(u.ids, no_free);
  vec_free(u.keys, f);
}

uf_id_t uf_fresh(uf_t u, any_t k) {
  uf_id_t r = vec_len(u.ids);
  vec_add(&u.ids, OF_ID(r));
  vec_add(&u.keys, k);
  return r;
}

uf_id_t uf_find(uf_t u, uf_id_t i) {
  while (OF_ID(i) != u.ids[i])
    i = TO_ID(u.ids[i] = OF_ID(u.ids[TO_ID(u.ids[i])]));
  return i;
}

void uf_union(uf_t u, uf_id_t i, uf_id_t j) {
  i = uf_find(u, i);
  j = uf_find(u, j);
  if (i != j)
    u.ids[i] = OF_ID(j);
}

any_t uf_get(uf_t u, uf_id_t i) { return u.keys[i]; }

#undef OF_ID
#undef TO_ID
