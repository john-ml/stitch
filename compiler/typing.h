#ifndef TYPING_INCLUDED_H
#define TYPING_INCLUDED_H

#include "ast.h"
#include "interning.h"
#include <assert.h>

// HM-style type inference/checking
// Fully annotate prgm with type information
void infer_types(node_t prgm);

// Simple check of fully annotated program
void check_types(stab_t t, node_t const prgm);

#endif // TYPING_INCLUDED_H
