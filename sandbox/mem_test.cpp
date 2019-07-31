#include "mem.h"
#include <iostream>

int main() {
  using namespace std;
  #define pp_ptr(p) cout << reinterpret_cast<void*>(p) << " -> " << *p << endl;

  int* p = mem::alloc<int>(); *p = 10;
  pp_ptr(p);
  mem::free(p);

  char *q = mem::alloc<char>(); *q = 'c';
  pp_ptr(q);
  mem::free(q);
}
