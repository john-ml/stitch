#include <iostream>
#include <chrono>
#include <thread>
#include <sys/mman.h>

template<typename T>
T *gimme(size_t n) {
  return reinterpret_cast<T*>(mmap(
    nullptr, n,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANONYMOUS,
    -1, 0));
}

int main() {
  size_t const ARENAS = 1024;
  size_t *xss[ARENAS];
  for (size_t i = 0; i < ARENAS; ++i)
    xss[i] = gimme<size_t>(1 << 30);
  for (size_t i = 0;; i += 1024) {
    using namespace std;
    cout << i << endl;
    this_thread::sleep_for(chrono::milliseconds(1000));
    for (size_t j = 0; j < ARENAS; ++j)
      xss[j][i] = 1;
  }
}
