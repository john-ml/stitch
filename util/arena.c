#include "arena.h"
#include "vec.h"
#include <stdlib.h>
#include <stdio.h>

#define BLOCK_SIZE 1024

typedef struct block_t {
  int len;
  int cap;
  char data[0];
} *block_t;

block_t block_new(int bytes) {
  block_t b = malloc(sizeof(*b) + bytes);
  b->len = 0;
  b->cap = bytes;
  return b;
}

void block_del(block_t b) { free(b); }

int block_unused(block_t b) { return b->cap - b->len; }

void *block_alloc(block_t b, int bytes) {
  char *res = b->data + b->len;
  b->len += bytes; 
  return (void *)res;
}

arena_t arena_new() { return vec_sing(block_new(BLOCK_SIZE)); }

void *arena_alloc(arena_t *a, int bytes) {
  if (block_unused((*a)[vec_len(*a) - 1]) < bytes)
    vec_add(a, block_new(bytes > BLOCK_SIZE ? bytes : BLOCK_SIZE));
  return block_alloc((*a)[vec_len(*a) - 1], bytes);
}

void arena_del(arena_t a) { vec_del(a, (del_t)block_del); }
