#ifndef ARENA_INCLUDED_H
#define ARENA_INCLUDED_H

#include "vec.h"
#include <stddef.h>

// An arena is a vector of blocks
typedef vec_t arena_t;

arena_t arena_new();

void *arena_alloc(arena_t *a, int bytes);

void arena_del(arena_t a);

#endif // ARENA_INCLUDED_H
