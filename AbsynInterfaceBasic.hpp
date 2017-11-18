#include "dynamic_wrapper.hpp"
#include "LabelledVariant.hpp"

#include <cstdint>

#include <map>
#include <optional>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

namespace SMLNJInterface {

using std::optional;
using std::string;
using std::vector;
using std::tuple;
using std::pair;
using std::map;

using word = std::size_t;
using word32 = std::uint32_t;
using word8 = std::uint8_t;
using maxint = std::intmax_t; // replacement for InfInt.int

}
