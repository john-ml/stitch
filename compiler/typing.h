#ifndef TYPING_INCLUDED_H
#define TYPING_INCLUDED_H

#include "ast.h"
#include "uf.h"
#include "interning.h"
#include "arena.h"
#include <assert.h>

// HM-style type inference/checking
// A unification variable (EXP_UVAR) is created in place of any missing
// annotations. Their instantiations can be read out by calling uf_find
// on the returned union-find instance as needed.
uf_t infer_types(arena_p *a, node_p prgm);

#endif // TYPING_INCLUDED_H
