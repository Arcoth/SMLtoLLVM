#pragma once

#include "Misc/dynamic_wrapper.hpp"
#include "Misc/LabelledVariant.hpp"

#include <cstdint>

#include <map>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>
#include <cxxabi.h>

namespace SMLNJInterface {

using std::optional;
using std::string;
using std::vector;
using std::tuple;
using std::pair;
using std::map;
using std::unordered_map;

using word = std::size_t;
using word32 = std::uint32_t;
using word8 = std::uint8_t;
using maxint = std::intmax_t; // replacement for InfInt.int

template <std::size_t I, typename... Args>
decltype(auto) emplace_by_value(std::variant<Args...>& v, std::variant_alternative_t<I, std::variant<Args...>> x) {
  return v.template emplace<I>(std::move(x));
}

template<typename T>
string type_name()
{
    int status;
    string tname = typeid(T).name();
    char *demangled_name = abi::__cxa_demangle(tname.c_str(), NULL, NULL, &status);
    if(status == 0) {
        tname = demangled_name;
        std::free(demangled_name);
    }
    return tname;
}

}
