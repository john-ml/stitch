#include "misc.h"
#include "arena.h"
#include "vec.h"
#include <stdlib.h>
#include <stdio.h>

#define BLOCK_SIZE (64 * 1024)

typedef struct block_t {
  size_t len;
  size_t cap;
  char data[0];
} *block_p;

block_p block_new(size_t bytes) {
  block_p b = malloc(sizeof(*b) + bytes);
  b->len = 0;
  b->cap = bytes;
  return b;
}

void block_del(block_p b) { free(b); }

size_t block_unused(block_p b) { return b->cap - b->len; }

void *block_alloc(block_p b, size_t bytes) {
  char *res = b->data + b->len;
  b->len += bytes; 
  return (void *)res;
}

arena_p arena_new() {
  const size_t cap = 8;
  arena_p a = malloc(sizeof(*a) + cap*sizeof(block_p));
  a->len = 1;
  a->cap = cap;
  a->data[0] = block_new(BLOCK_SIZE);
  return a;
}

block_p arena_add_block(arena_p *a, block_p b) {
  if ((*a)->len == (*a)->cap) {
    (*a)->cap *= 2;
    *a = realloc(*a, sizeof(*a) + (*a)->cap*sizeof(block_p));
  }
  (*a)->data[++(*a)->len] = b;
  return b;
}

void *arena_alloc(arena_p *a, size_t bytes) {
  block_p block = (*a)->data[(*a)->len - 1];
  if (block_unused(block) < bytes)
    block = arena_add_block(a, block_new(MAX(bytes, BLOCK_SIZE)));
  return block_alloc(block, bytes);
}

void arena_del(arena_p a) {
  for (size_t i = 0; i < a->len; ++i)
    free(a->data[i]);
  free(a);
}
