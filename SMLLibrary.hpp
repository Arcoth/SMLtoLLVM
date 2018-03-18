#pragma once

#include "Compiler/basic.hpp"

#include <boost/container/map.hpp>
#include <boost/unordered_map.hpp>

#include <chrono>

namespace SMLCompiler::LibraryImpl {

void*(*allocate_ptr)(std::uint64_t);

namespace Timer {
  using clock_type = std::chrono::high_resolution_clock;
  inline genericPointerTypeNative startRealTimer(genericPointerTypeNative, genericPointerTypeNative[]) {
    return boxNative(clock_type::now().time_since_epoch().count());
  }
  inline genericPointerTypeNative checkRealTimer(genericPointerTypeNative arg, genericPointerTypeNative[]) {
    return boxNative(clock_type::now().time_since_epoch().count() - unboxNative(arg));
  }

}

namespace Time {
  inline genericPointerTypeNative toMilliseconds(genericPointerTypeNative arg, genericPointerTypeNative[]) {
    return boxNative(unboxNative(arg)/(Timer::clock_type::duration::period::den/1000));
  }
}

namespace List {
  inline genericPointerTypeNative hd(genericPointerTypeNative arg, genericPointerTypeNative[]) {
    return ((genericPointerTypeNative*)arg)[1];
  }
}

inline const boost::unordered_map<std::string, boost::container::map<boost::container::vector<int>, genericPointerTypeNative>> libraryIdMap {{
  {"0AAB32C474C86764F4EDD7B5F7975050", {{{{86, 2}}, (genericPointerTypeNative)new genericFunctionTypeNative*[1]{(genericFunctionTypeNative*)0xDEADBEEF}},
                                        {{std::initializer_list<int>{112}}, (genericPointerTypeNative)new genericFunctionTypeNative*[1]{List::hd}}}},
  {"15264C47F5ED8119799A5101E44495E0", {{{{0, 6}}, (genericPointerTypeNative)new genericFunctionTypeNative*[1]{Time::toMilliseconds}}}},
  {"A44C25B0AB637462740A552AFDE72D60", {{{{0, 5}}, (genericPointerTypeNative)new genericFunctionTypeNative*[1]{Timer::startRealTimer}},
                                        {{{0, 7}}, (genericPointerTypeNative)new genericFunctionTypeNative*[1]{Timer::checkRealTimer}}}},
}};

}

