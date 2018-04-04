#pragma once

#include "GCPlugin/GCBasicConstants.hpp"

#include <exception>
#include <ostream>
#include <set>
#include <string>

#include <llvm/IR/LLVMContext.h>

#define LIKELY(x)       __builtin_expect((x),1)
#define UNLIKELY(x)     __builtin_expect((x),0)

namespace SMLCompiler{

inline thread_local llvm::LLVMContext context; // the global LLVM context for the compiler

using genericPointerTypeNative = char*;
using genericIntTypeNative = std::intptr_t;
using tagTypeNative = std::int32_t;

static_assert(sizeof(void*) == sizeof(std::int64_t), "This project currently requires 64-bit words.");

static_assert(2 * sizeof(tagTypeNative) == sizeof(genericIntTypeNative)); // sanity

using genericFunctionTypeNative =
  genericPointerTypeNative(genericPointerTypeNative, genericPointerTypeNative[]);

inline genericPointerTypeNative boxNative(genericIntTypeNative i) {
  return (genericPointerTypeNative)((i << GC::valueFlagLength) | GC::intTag);
}
inline genericIntTypeNative unboxNative(genericPointerTypeNative i) {
  return ((genericIntTypeNative)i) >> GC::valueFlagLength;
}

inline auto boxNativeTag(tagTypeNative t) {
  return (t << GC::recordFlagLength) | GC::lengthTag;
}

class CompileFailException : public std::runtime_error {
  using std::runtime_error::runtime_error;
};

class UnsupportedException : public std::domain_error {
  using std::domain_error::domain_error;
};

template <typename Set, typename C>
Set set_subtract(Set s, C const& t) {
  for (auto& x : t)
    s.erase(x);
  return s;
}
template <typename Set1, typename T>
Set1 set_subtract(Set1 s, std::initializer_list<T> t) {
  return set_subtract<Set1, decltype(t)>(move(s), t);
}

template <typename Set, typename C>
Set set_union(Set s, C const& c) {
  s.insert(std::begin(c), std::end(c));
  return s;
}

template <typename T, typename U>
std::ostream& operator<<(std::ostream& os, std::tuple<T, U> const& p);
template <typename T, typename U>
std::ostream& operator<<(std::ostream& os, std::pair<T, U> const& p) {
  return os << p.first << " -> " << p.second;
}
template <typename T, typename U>
std::ostream& operator<<(std::ostream& os, std::tuple<T, U> const& p) {
  return os << std::get<0>(p) << " -> " << std::get<1>(p);
}

template <typename Range>
auto operator<<(std::ostream& os, Range&& r)
  -> std::enable_if_t<!std::is_same_v<std::decay_t<decltype(*std::begin(r))>, char>, std::ostream&> {
  os << '{';
  bool first = true;
  for (auto& x : r) {
    if (first)
      first = false;
    else
      os << ", ";
    os << x;
  }
  os << '}';
  return os;
}



}
