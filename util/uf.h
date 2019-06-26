// Union-find with path compression

#ifndef UF_INCLUDED_H
#define UF_INCLUDED_H

#include "misc.h"
#include "vec.h"

// IDs point to their parents
typedef void **uf_id_t;

typedef struct uf_t {
  vec_t ids; // ID tree
  vec_t keys;
} uf_t;

uf_t uf_new();

// f frees keys
void uf_free(uf_t u, free_t f);

uf_id_t uf_fresh(uf_t u, any_t k);

uf_id_t uf_find(uf_t u, uf_id_t p);

void uf_union(uf_t u, uf_id_t p, uf_id_t q);

any_t uf_get(uf_t u, uf_id_t p);

#endif // UF_INCLUDED_H
