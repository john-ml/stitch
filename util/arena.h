#ifndef ARENA_INCLUDED_H
#define ARENA_INCLUDED_H

#include <stddef.h>

typedef struct {
  size_t len;
  size_t cap;
  struct block_t *data[0];
} *arena_p;

arena_p arena_new();

void *arena_alloc(arena_p *a, size_t bytes);

void arena_del(arena_p a);

#endif // ARENA_INCLUDED_H
