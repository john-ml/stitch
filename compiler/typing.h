#ifndef TYPING_INCLUDED_H
#define TYPING_INCLUDED_H

#include "ast.h"
#include "uf.h"
#include "interning.h"
#include <assert.h>

// HM-style type inference/checking
// A unification variable (EXP_UVAR) is created in place of any missing
// annotations. Their instantiations can be read out by calling uf_find
// on the returned union-find instance as needed.
// The returned union-find instance owns all keys.
uf_t infer_types(node_p prgm);

// Simple check of fully annotated program
void check_types(stab_t t, node_p const prgm);

#endif // TYPING_INCLUDED_H
