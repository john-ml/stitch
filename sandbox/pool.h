#ifndef POOL_INCLUDED_H
#define POOL_INCLUDED_H

#include <cstddef>
#include <cstdint>
#include <sys/mman.h>

template<size_t W_>
class size_class {
public:
  // max is the capacity in bytes, not block units
  template<size_t W>
  friend size_class<W>* init(size_t max);

  void* alloc();
  void free(void* p);

public:
  static constexpr size_t alignment = 8;
 
  struct header {
    bool mark;
    bool free;
  }; // Assume sizeof(header) < alignment

  // Force alignment and W >= alignment
  static constexpr size_t W = []() constexpr {
    auto W = W_ + alignment;
    return W + (alignment - W%alignment);
  }();

  using block = uint8_t[W];

  // Conversions block <-> heap pointer

  static void* of_block(block* p)
  { return reinterpret_cast<void*>(reinterpret_cast<char*>(p) + alignment); }

  static block* to_block(void* p)
  { return reinterpret_cast<block*>(reinterpret_cast<char*>(p) - alignment); }

  // Updating block flags

  static block* set_mark(block* p, bool b)
  { reinterpret_cast<header*>(p)->mark = b; return p; }

  static block* set_free(block* p, bool b)
  { reinterpret_cast<header*>(p)->free = b; return p; }

  struct free_list { free_list* next; };

public:
  free_list free_;
  block* end_;
  block* cap_;
  block data_[0];
  // x in free <=> reinterpret_cast<header*>(to_block(x))->free
  // end in [&data .. cap)
  // x in free ==> x in [&data .. cap) /\ x < end

}; // size_class

template<size_t W>
size_class<W>* size_class_init(size_t max) {
  using C = size_class<W>;
  auto m = reinterpret_cast<size_class<W>*>(mmap(
    nullptr, sizeof(size_class<W>) + max,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANONYMOUS,
    -1, 0));
  m->free_.next = nullptr;
  m->end_ = reinterpret_cast<typename C::block*>(&m->data_);
  m->cap_ = reinterpret_cast<typename C::block*>(
    max + reinterpret_cast<char*>(&m->data_));
  return m;
}

template<size_t W>
void* size_class<W>::alloc() {
  if (free_.next == nullptr)
    return end_ + sizeof(block) > cap_
      ? nullptr
      : of_block(set_free(end_++, false));
  auto res = of_block(set_free(to_block(free_.next), false));
  free_ = *(free_.next);
  return res;
}

template<size_t W>
void size_class<W>::free(void* p) {
  (void) set_free(to_block(p), true);
  auto q = reinterpret_cast<free_list*>(p);
  *q = free_;
  free_.next = q;
}

template<size_t W>
auto arena = size_class_init<W>(1 << 30);

#define ALLOC(T) (reinterpret_cast<T*>(arena<sizeof(T)>->alloc()))
#define FREE(T, p) (arena<sizeof(T)>->free(p))

#endif // POOL_INCLUDED_H
