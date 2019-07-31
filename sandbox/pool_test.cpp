#include "pool.h"
#include <iostream>

int main() {
  using namespace std;
  int* p = ALLOC(int); *p = 10;
  cout << *p << " at " << p << endl;
  FREE(int, p);
  int *q = ALLOC(int); *q = 11;
  cout << *q << " at " << q << endl;
  FREE(int, q);
}
