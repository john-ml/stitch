#ifndef MEM_INCLUDED_H
#define MEM_INCLUDED_H

#include <cstddef>
#include <cstdint>
#include <algorithm>
#include <sys/mman.h>

namespace mem {

constexpr size_t alignment = 8;

struct header {
  bool mark;
  bool free;
}; // Assume sizeof(header) < alignment

// Force alignment + ensure that W >= sizeof(void*)
// (need that space for free list's next pointers)
constexpr size_t chunk_size_of(size_t w) {
  auto aligned = w + (alignment - w%alignment);
  return std::max(sizeof(void*), w + alignment - w%alignment);
}

template<typename T>
constexpr size_t size_class_of = chunk_size_of(sizeof(T));

struct free_list { free_list* next; };

// Arena for items of size w where W_ = chunk_size_of(w)
template<size_t W_>
class size_class {
public:
  // max is the capacity in bytes
  static size_class<W_>* init(size_t max);

  static void* alloc(size_class<W_>*);
  static void free(size_class<W_>*, void* p);

  // Check if a pointer was allocated inside this heap
  static bool exists(size_class<W_>*, void* p);

public:
  // Reserve space for header
  static constexpr size_t W = W_ + alignment;

  // block <-> heap pointer <-> header
  using block = uint8_t[W];

  static void* of_block(block* p)
  { return reinterpret_cast<void*>(reinterpret_cast<char*>(p) + alignment); }

  static block* to_block(void* p)
  { return reinterpret_cast<block*>(reinterpret_cast<char*>(p) - alignment); }

  static header* to_header(block* p) { return reinterpret_cast<header*>(p); }
  static header* to_header(void* p) { return to_header(to_block(p)); }
 
public:
  free_list* free_;
  block* end_;
  block* cap_;
  block data_[0];
  // Assuming x in [&data .. end),
  //   x in free <=> reinterpret_cast<header*>(to_block(x))->free
  //   end in [&data .. cap)
};

// Hacky constructor.
template<size_t W_>
size_class<W_>* size_class<W_>::init(size_t max) {
  using C = size_class<W_>;
  auto m = reinterpret_cast<C*>(mmap(
    nullptr, sizeof(C) + max,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANONYMOUS,
    -1, 0));
  m->free_ = nullptr;
  m->end_ = &m->data_[0];
  m->cap_ = reinterpret_cast<typename C::block*>(
    max + reinterpret_cast<char*>(&m->data_[0]));
  return m;
}

template<size_t W_>
void* size_class<W_>::alloc(size_class<W_>* m) {
  if (m->free_ == nullptr) {
    if (m->end_ + sizeof(block) > m->cap_)
      return nullptr;
    to_header(m->end_)->free = false;
    return of_block(m->end_++);
  }
  free_list* res = m->free_;
  to_header(res)->free = false;
  m->free_ = res->next;
  return reinterpret_cast<void*>(res);
}

template<size_t W_>
void size_class<W_>::free(size_class<W_>* m, void* p) {
  to_header(p)->free = true;
  auto q = reinterpret_cast<free_list*>(p);
  q->next = m->free_;
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
  return reinterpret_cast<T*>(size_class<w>::alloc(arena<w>));
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

// Read flags

template<typename T>
bool get_free(T* p) { return size_class<size_class_of<T>>::to_header(p)->free; }

template<typename T>
bool get_mark(T* p) { return size_class<size_class_of<T>>::to_header(p)->mark; }

// Write mark flag

template<typename T>
void set_mark(T* p, bool b)
{ size_class<size_class_of<T>>::to_header(p)->mark = b; }

} // mem

#endif // MEM_INCLUDED_H
