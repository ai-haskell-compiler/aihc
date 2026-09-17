// A C++ source with a dependency on the C++ standard library: the vector
// allocates through operator new, so the object links only with it.
#include <numeric>
#include <vector>

extern "C" int aihc_cxx_triangle(int count) {
  std::vector<int> values(static_cast<std::size_t>(count));
  std::iota(values.begin(), values.end(), 1);
  return std::accumulate(values.begin(), values.end(), 0);
}
