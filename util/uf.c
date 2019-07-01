#include "uf.h"

uf_t uf_new(arena_p *a) {
  return (uf_t) {
    .ids = vec_new(a, sizeof(uf_id_t)), 
    .keys = vec_new(a, sizeof(any_t))
  };
}

uf_id_t uf_fresh(arena_p *a, uf_t u, any_t k) {
  uf_id_t r = vec_len(u.ids);
  vec_add(a, (vec_p)&u.ids, &r);
  vec_add(a, (vec_p)&u.keys, &k);
  return r;
}

uf_id_t uf_find(uf_t u, uf_id_t i) {
  while (i != u.ids[i])
    i = u.ids[i] = u.ids[u.ids[i]];
  return i;
}

void uf_union(uf_t u, uf_id_t i, uf_id_t j) {
  i = uf_find(u, i);
  j = uf_find(u, j);
  if (i != j)
    u.ids[i] = j;
}

any_t uf_get(uf_t u, uf_id_t i) { return u.keys[i]; }
