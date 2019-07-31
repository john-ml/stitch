#ifndef BITMAP_INCLUDED_H
#define BITMAP_INCLUDED_H

#include <cstdint>
#include <cstddef>
#include <vector>
#include <array>
#include <algorithm>
#include <utility>
#include <iostream>

namespace util {

uint64_t bit(int i) { return (uint64_t)1 << i; }

int bsf(uint64_t x) { return __builtin_ctz(x); }

int bsr(uint64_t x) { return __builtin_clz(x); }

bool get(uint64_t x, int i) { return 1 & x >> i; }

uint64_t hi(uint64_t x, int i) { return x | bit(i); }

uint64_t lo(uint64_t x, int i) { return x & ~bit(i); }

uint64_t take(uint64_t x, int i) { return x & bit(i) - 1; }

uint64_t drop(uint64_t x, int i) { return x >> i; }

uint64_t substr(uint64_t x, int start, int len) {
  return take(drop(x, start), len); 
}

// Force xs[i] to be valid
template<typename T>
void expand(T& xs, size_t i) { if (i >= xs.size()) xs.resize(i + 1); }

}

template<size_t W>
class bitmap {
public:
  bitmap() : height_(0) {}

  bool operator[](size_t i);
  void hi(size_t n);
  void lo(size_t n);
  void set(size_t n, bool p) { if (p) hi(n); else lo(n); }

  void dump(std::ostream& o) const;

private:
  static const int width = W + 6;
  int height_;

  struct tree {
    std::array<uint64_t, 1ul << W> occ;
    std::vector<tree> subs;

    tree() : occ{0} {}
    tree(tree&& t, bool _) : occ{0}, subs{std::move(t)} {
      auto nonzero = [](uint64_t x) { return x != 0; };
      occ[0] |= static_cast<uint64_t>(
        std::any_of(t.occ.begin(), t.occ.end(), nonzero));
    }

    bool get(size_t i) const { return util::get(occ[i / 64], i % 64); }
    void dump(std::ostream&, int, int) const; // Helper for bitmap::dump
    void hi(size_t i) { occ[i / 64] = util::hi(occ[i / 64], i % 64); }
    void lo(size_t i) { occ[i / 64] = util::lo(occ[i / 64], i % 64); }
    void lo_(size_t n, int i); // Helper for bitmap::lo
  } t_;
};

// Chopping n into width-bit-long segments yields indices for each level of
// the tree:
//   MSB first index, second index, .. LSB
//       ------------- n -------------
// To retrieve a bit, just follow the indices.
template<size_t W>
bool bitmap<W>::operator[](size_t n) {
  tree* t = &t_;
  for (int i = width * height_; i != 0; i -= width) {
    size_t m = util::substr(n, i, width);
    if (!t->get(m))
      // Bail early if any index points to empty subtree
      return false;
    t = &t->subs[m];
  }
  return t->get(util::take(n, width));
}

// To set a bit high, follow the indices and set subtrees high along the way.
// Add a new layer if n > current capacity.
template<size_t W>
void bitmap<W>::hi(size_t n) {
  int new_height = util::bsr(n) / width;
  for (; height_ < new_height; ++height_)
    t_ = tree(std::move(t_), true);
  tree* t = &t_;
  for (int i = width * height_; i != 0; i -= width) {
    size_t m = util::substr(n, i, width);
    t->hi(m);
    util::expand(t->subs, m); // Ensure subs[m] OK
    t = &t->subs[m];
  }
  t->hi(util::take(n, width));
}

// To set a bit low, right recursion is necessary: we need to check whether
// all subtrees are empty before marking the current tree as empty.
template<size_t W>
void bitmap<W>::lo(size_t n) { t_.lo_(n, width * height_); }

// Helper for bitmap<W>::lo.
template<size_t W>
void bitmap<W>::tree::lo_(size_t n, int i) {
  if (i == 0)
    return lo(util::take(n, width));
  size_t m = util::substr(n, i, width);
  if (!get(m))
    // Bail early if tree is already empty
    return;
  util::expand(subs, m); // Ensure subs[m] OK
  subs[m].lo_(n, i - width);
  auto is_zero = [](uint64_t x) { return x == 0; };
  if (std::all_of(subs[m].occ.begin(), subs[m].occ.end(), is_zero))
    lo(m);
}

// Pretty-print to the output stream o.
template<size_t W>
void bitmap<W>::dump(std::ostream& o) const { t_.dump(o, 0, height_); }

// Helper for bitmap<W>::dump.
template<size_t W>
void bitmap<W>::tree::dump(std::ostream& o, int indent, int height) const {
  auto mk_indent = [&]() {
    for (int i = 0; i < indent; ++i)
      o << " ";
  };
  if (height == 0) {
    mk_indent();
    for (uint64_t x : occ) {
      for (int i = 0; i < 64; i += 4) {
        int c = util::substr(x, i, 4);
        o << static_cast<char>(c + (c < 10 ? '0' : 'a' - 10));
      }
      o << " ";
    }
    o << std::endl;
  }
  else {
    for (int i = 0; i < occ.size() * 64; ++i) {
      if (get(i)) {
        mk_indent();
        o << i << std::endl;
        subs[i].dump(o, indent + 1, height - 1);
      }
    }
  }
}

#endif // BITMAP_INCLUDED_H
