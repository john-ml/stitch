#include <iostream>
#include <vector>

int main() {
  std::vector<int> v{2};
  std::cout << v.size() << std::endl;
  bool p = true;
  bool q = false;
  std::cout << (int)p << " " << (int)q << std::endl;
}
