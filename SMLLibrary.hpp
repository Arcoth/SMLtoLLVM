#pragma once

#include "Compiler/basic.hpp"

#include <boost/container/map.hpp>
#include <boost/container/vector.hpp>
#include <boost/unordered_map.hpp>

namespace SMLCompiler::LibraryImpl {

using Ptr = genericPointerTypeNative;
using Int = genericIntTypeNative;

extern void*(* allocate_small_ptr)(std::uint64_t);
extern void*(* allocate_large_ptr)(std::uint64_t);

extern const boost::unordered_map<std::string, boost::container::map<boost::container::vector<int>, Ptr>> libraryIdMap;

}

