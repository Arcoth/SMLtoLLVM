#include <cstddef>
#include <cstdlib>

void* allocate(std::size_t i){
  return std::malloc(i);
}
