#ifndef INTERNING_INCLUDED_H
#define INTERNING_INCLUDED_H

#include "vec.h"
#include <stddef.h>

// String ID
typedef int sid_t;

// String table
typedef struct { vec_t strs; } stab_t;

stab_t stab_new();

// Does not steal s
sid_t stab_add(stab_t *t, char const *s);

char const *stab_get(stab_t t, sid_t i);

void stab_del(stab_t t);

#endif // INTERNING_INCLUDED_H
