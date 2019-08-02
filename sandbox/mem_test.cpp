#include "mem.h"
#include <iostream>

template<typename T> void pretty_type() { puts(__PRETTY_FUNCTION__); }

int main() {
  using namespace std;
  #define pp_ptr(p) cout << reinterpret_cast<void*>(p) << " -> " << *p << endl;

  int* p = mem::alloc<int>(); *p = 10;
  pp_ptr(p);
  mem::free(p);

  char *q = mem::alloc<char>(); *q = 'c';
  pp_ptr(q);
  mem::free(q);

  using namespace ty;
  Struct<I<8>, Struct<Ptr<U<16>>, Unit>>::T x;
  pretty_type<decltype(x.a)>();
  pretty_type<decltype(x.b)>();
  pretty_type<decltype(*x.b.a)>();
  pretty_type<decltype(x.b.b)>();
}
