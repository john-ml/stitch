uf_t uf_new() { return (uf_t) { .ids = vec_new(), .keys = vec_new() }; }

void uf_free(uf_t u, free_t f) { 
  vec_free(u.ids, no_free);
  vec_free(u.keys, f);
}

uf_id_t uf_fresh(uf_t u, any_t k) {
  int n = vec_len(u.ids);
  uf_id_t r = u.ids + vec_len(u.ids);
  vec_add(u.ids, r);
  vec_add(u.keys, k);
  return r;
}

uf_id_t uf_find(uf_t u, uf_id_t p) {
  while (p != *p)
    p = *p = **p;
  return p;
}

void uf_union(uf_t u, uf_id_t p, uf_id_t q) {
  p = uf_find(u, p);
  q = uf_find(u, q);
  if (p != q)
    *p = q;
}

any_t uf_get(uf_t u, uf_id_t p) { return u.keys[p - u.ids]; }
