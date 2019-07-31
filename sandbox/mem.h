#ifndef MEM_INCLUDED_H
#define MEM_INCLUDED_H

#include <cstddef>
#include <cstdint>
#include <sys/mman.h>

namespace mem {

static constexpr size_t alignment = 8;

// Force alignment + ensure that W >= sizeof(void*)
// (need that space for free list's next pointers)
constexpr size_t chunk_size_of(size_t w) {
  auto aligned = w + (alignment - w%alignment);
  return sizeof(void*) > aligned ? sizeof(void*) : aligned;
}

struct free_list { free_list* next; };

// Arena for items of size w where W_ = chunk_size_of(w)
template<size_t W_>
class size_class {
public:
  // max is the capacity in bytes
  template<size_t W>
  friend size_class<W>* init(size_t max);

  void* alloc();
  void free(void* p);

public:
  struct header {
    bool mark;
    bool free;
  }; // Assume sizeof(header) < alignment

  // Reserve space for header
  static constexpr size_t W = W_ + alignment;

  using block = uint8_t[W];

  // block <-> heap pointer

  static void* of_block(block* p)
  { return reinterpret_cast<void*>(reinterpret_cast<char*>(p) + alignment); }

  static block* to_block(void* p)
  { return reinterpret_cast<block*>(reinterpret_cast<char*>(p) - alignment); }

  // Updating block flags

  static block* set_mark(block* p, bool b)
  { reinterpret_cast<header*>(p)->mark = b; return p; }

  static block* set_free(block* p, bool b)
  { reinterpret_cast<header*>(p)->free = b; return p; }
 
public:
  free_list free_;
  block* end_;
  block* cap_;
  block data_[0];
  // x in free <=> reinterpret_cast<header*>(to_block(x))->free
  // end in [&data .. cap)
  // x in free ==> x in [&data .. cap) /\ x < end
};

// Hacky constructor.
// TODO find a way to do this nicely
// TODO is it even ok to call alloc/free as member functions?
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

// Specialization will generate a global arena for every size class
template<size_t W>
auto arena = size_class_init<W>(1 << 30);

// Allocate in the proper size class
template<typename T> T* alloc()
{ return reinterpret_cast<T*>(arena<chunk_size_of(sizeof(T))>->alloc()); }

// Free in the proper size class
template<typename T> void free(T* p)
{ arena<chunk_size_of(sizeof(T))>->free(p); }

} // mem

#endif // MEM_INCLUDED_H
