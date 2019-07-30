#include "bitmap.h"
#include <iostream>

int main() {
  using namespace std;

  bitmap<2> m;
  cout << m[0] << endl;
  cout << m[100] << endl;
  m.hi(100);
  cout << m[100] << endl;

  cout << "--------------------" << endl;
  m.hi(4194304);
  m.hi(1073741824);
  m.hi(18236711);
  m.hi(29375);
  m.hi(39476239);
  m.hi(928319238);
  for (int i = 0; i < 512; ++i)
    m.hi(i);
  m.dump(cout);

  cout << "--------------------" << endl;
  for (int i = 8; i < 504; ++i)
    m.lo(i);
  m.lo(4194304);
  m.lo(1073741824);
  m.lo(18236711);
  m.lo(29375);
  m.lo(39476239);
  m.lo(928319238);
  m.dump(cout);
}
