// Heap-allocated pair
#ifndef PAIR_INCLUDED_H
#define PAIR_INCLUDED_H

typedef struct {
  void *a;
  void *b;
} *pair_t;

pair_t pair_new(void *a, void *b);

#endif // PAIR_INCLUDED_H
