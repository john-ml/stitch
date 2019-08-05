// Accumulate:
// - Free list
// - Queue of sources for tracing and the levels at which to trace them
//   (basically lazy suspensions of marking)
// - List of sinks eligible for deallocation and the levels at which to
//   sweep them (basically lazy suspensions of sweeping)
// - Whenever bind to lval that might get aliased by future levels, enqueue
//   source at current level or update existing level with min(new, old)
// - Whenever intermediate value falling out of scope/dying that might alias
//   past levels, add sink at current level or update existing with
//   min(new, old)
// - Whenever intermediate value falling out of scope/dying that doesn't alias,
//   add to free list
// - Whenever request new memory:
//     - If free list non-empty, return head + add its children to their free
//       lists
//     - If sinks available, force some of them
//         - For sink at level l: 
//              - Mark all sources at level l' < l
//                (you have to somehow know their types/locations of pointers!)
//              - Delete sources at level l' not< l
//              - If sink is marked at level < l, delete from sink list and
//                go to next sink (this one is still live)
//              - Otherwise, return sink and resume lazy sweep (i.e. mark all of
//                sink's children as sinks at level l)
//                (again, somehow have to know the type/layout of the sink!)
//     - If no free cell found in sinks, bump_pointer++
// < on levels should form a partial order and not a total one. Possible
// executions form a tree where each node is a level; l1 < l2 if l1 ~> l2
// in the tree.

// Automatic memory management
// The heap is a bunch of mmap'ed size classes.
// Each size class has 3 lists: free, dying, alive.
// - free is just a regular free list.
// - dying's transitive closure contains all memory unreachable by the main
//   program.
// - alive's transitive closure contains all memory reachable by both the main
//   program and `dying`.

#ifndef MEM_INCLUDED_H
#define MEM_INCLUDED_H

// TODO:
// - Maintain n_free_, n_alive_, n_dying_
// - Type-directed mark/prune
// - If ever !n_alive_, prune everything in dying_
// - If ever !n_dying_, alive_ = nullptr?

#include "typing.h"

#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <sys/mman.h>

namespace mem {

constexpr size_t alignment = sizeof(size_t);

constexpr size_t aligned_size(size_t w)
{ return w + (alignment - w%alignment); }

template<typename T>
constexpr size_t size_class_of = aligned_size(sizeof(T));

template<typename R, typename T>
R cast(T x) { return reinterpret_cast<R>(x); }

// Arena for items of size w where W = aligned_size(w)
template<size_t W>
class size_class {
public:
  // max is the capacity in bytes
  static size_class<W>* init(size_t max);

  static void* alloc(size_class<W>*);
  static void free(size_class<W>*, void* p);

  // Check if a pointer was allocated inside this heap
  static bool exists(size_class<W>*, void* p);

public:
  struct block {
    block* next_free;
    block* next_dying;
    block* next_alive;
    bool free : 1;
    bool dying : 1;
    bool alive : 1;
    bool mark : 1;
    bool dying_skip : 1;
    bool alive_skip : 1;
    size_t _ : 8*sizeof(size_t) - 4;
    uint8_t data[W];
  };
  static_assert(sizeof(block) % alignment == 0);
  static_assert(offsetof(block, data) % alignment == 0);

  static void* of_block(block* p) { return cast<void*>(&p->data); }

  static block* to_block(void* p)
  { return cast<block*>(cast<char*>(p) - offsetof(block, data)); }

public:
  block* free_;
  block* alive_;
  block* dying_;
  size_t n_free_;
  size_t n_alive_;
  size_t n_dying_;
  block* end_;
  block* cap_;
  block data_[0];
};

// Hacky constructor.
template<size_t W_>
size_class<W_>* size_class<W_>::init(size_t max) {
  using C = size_class<W_>;
  auto m = cast<C*>(mmap(
    nullptr, sizeof(C) + max,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANONYMOUS,
    -1, 0));
  m->free_ = m->alive_ = m->dying_ = nullptr;
  m->end_ = &m->data_[0];
  m->cap_ = cast<typename C::block*>(max + cast<char*>(&m->data_[0]));
  return m;
}

template<size_t W_>
void* size_class<W_>::alloc(size_class<W_>* m) {
  block* res;
  if (m->free_ == nullptr) {
    if (m->end_ + sizeof(block) > m->cap_)
      return nullptr;
    res = m->end_++;
  } else {
    res = m->free_;
    m->free_ = m->free_->next_free;
  }
  res->free = res->dying = res->alive = res->mark = false;
  return of_block(res);
}

template<size_t W_>
void size_class<W_>::free(size_class<W_>* m, void* p) {
  block* q = to_block(p);
  q->free = true;
  q->next_free = m->free_;
  m->free_ = q;
}

template<size_t W_>
bool size_class<W_>::exists(size_class<W_>* m, void* p) {
  return &m->data_[0] <= p && p < m->end_;
}

// ----------------------------------------

// Specialization will generate a global arena for every size class
template<size_t W>
auto arena = size_class<W>::init(1 << 30);

// size_class methods + automatically dispatch to proper arena by type

template<typename T>
T* alloc() {
  constexpr size_t w = size_class_of<T>;
  return cast<T*>(size_class<w>::alloc(arena<w>));
}

template<typename T>
void free(T* p) {
  constexpr size_t w = size_class_of<T>;
  size_class<w>::free(arena<w>, p);
}

template<typename T>
bool exists(T* p) {
  constexpr size_t w = size_class_of<T>;
  return size_class<w>::exists(arena<w>, p);
}

template<typename T>
bool get_flags(T* p) { return size_class<size_class_of<T>>::to_block(p)->flags; }

} // mem

#endif // MEM_INCLUDED_H
