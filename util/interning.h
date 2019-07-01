#ifndef INTERNING_INCLUDED_H
#define INTERNING_INCLUDED_H

#include "vec.h"
#include "arena.h"
#include <stddef.h>

// String ID
typedef int sid_t;

// String table
typedef struct { VEC(char *) strs; } stab_t;

stab_t stab_new(arena_p *a);

// Does not steal s
sid_t stab_add(arena_p *a, stab_t *t, char const *s);

char const *stab_get(stab_t t, sid_t i);

#endif // INTERNING_INCLUDED_H
