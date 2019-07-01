// Union-find with path compression

#ifndef UF_INCLUDED_H
#define UF_INCLUDED_H

#include "misc.h"
#include "vec.h"
#include <stddef.h>

typedef int uf_id_t;

typedef struct uf_t {
  VEC(uf_id_t) ids; // ID tree
  VEC(any_t) keys;
} uf_t;

uf_t uf_new(arena_p *a);

uf_id_t uf_fresh(arena_p *a, uf_t u, any_t k);

uf_id_t uf_find(uf_t u, uf_id_t p);

void uf_union(uf_t u, uf_id_t p, uf_id_t q);

any_t uf_get(uf_t u, uf_id_t p);

#endif // UF_INCLUDED_H
