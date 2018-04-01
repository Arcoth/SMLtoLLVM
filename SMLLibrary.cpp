#include "SMLLibrary.hpp"

#include <chrono>
#include <iostream>

#include <cinttypes>

namespace SMLCompiler::LibraryImpl {

template <typename F, typename... Args>
inline auto func(F f, Args... args) {
  return (Ptr)new genericFunctionTypeNative*[sizeof...(args) + 1]
         {(genericFunctionTypeNative*)f, args...};
}

constexpr Int unboxNoCast(Int n) {
  return n >> 1;
}
constexpr Int boxNoCast(Int n) {
  return (n << 1) | 1;
}

void*(*allocate_small_ptr)(std::uint64_t);
void*(*allocate_large_ptr)(std::uint64_t);

namespace Timer {
  using clock_type = std::chrono::high_resolution_clock;
  inline Ptr startRealTimer(Ptr, Ptr[]) {
    return boxNative(clock_type::now().time_since_epoch().count());
  }
  inline Ptr checkRealTimer(Ptr arg, Ptr[]) {
    return boxNative(clock_type::now().time_since_epoch().count() - unboxNative(arg));
  }

}

namespace Time {
  inline Ptr toMilliseconds(Ptr arg, Ptr[]) {
    return boxNative(unboxNative(arg)/(Timer::clock_type::duration::period::den/1000));
  }
}

namespace List {
  inline Ptr hd(Ptr arg, Ptr[]) {
    return ((Ptr*)arg)[1];
  }
}

//namespace Vector {
//  inline Ptr tabulate(Ptr* args, Ptr[]) {
//    const auto fun = (Ptr(*)(Int))args[1];
//    const auto len = unboxNoCast(args[0])+1;
//    const auto begin = (Int*)allocate_large_ptr(len);
//    *begin = ((n+1) << 2) | 1;
//    for (std::size_t i = 1; i < len; ++i)
//      begin[i] = fun(boxNoCast(i-1));
//    return begin;
//  }
//
//  inline Ptr tabulate(Int n, Ptr[]) {
//    return func([] (Ptr(*arg)(Int), Int* n) {
//      const auto l = unboxNoCast(n[1])+1;
//      const auto begin = (Int*)allocate_large_ptr(l);
//      *begin = ((n+1) << 2) | 1;
//      for (std::size_t i = 1; i < l; ++i)
//        begin[i] = arg(boxNoCast(i-1));
//      return begin;
//    }, n);
//  }
//}

namespace String {
  inline auto length(tagTypeNative* str, Ptr[] = nullptr) {
    return str[1];
  }
  inline auto concat(tagTypeNative** record, Ptr[]) {
    auto len = 8 + length(record[1]) + length(record[2]);
    auto alloc = (tagTypeNative*)allocate_large_ptr((len+7)/8);
    alloc[0] = boxNativeTag((len+7)/8);
    alloc[1] = len - 8;
    auto str_ptr = (char*)alloc + 8;
    std::memcpy(str_ptr                    , record[1] + 2, length(record[1]));
    std::memcpy(str_ptr + length(record[1]), record[2] + 2, length(record[2]));
    return alloc;
  }
}

namespace Core {
  inline Ptr i32ToInt(Ptr arg, Ptr[]) {
    return arg;
  }

  inline Ptr compareInfInt(Int* rcd, Ptr[]) {
    return boxNative(rcd[1] < rcd[2]);
  }

  inline Ptr print(tagTypeNative* str, Ptr[]) {
    std::cout.write((char const*)(str + 2), String::length(str, nullptr)).flush();
    return nullptr;
  }
}

namespace LargeInt {

  inline tagTypeNative* toString(Int i, Ptr[]) {
    auto alloc = (tagTypeNative*)allocate_large_ptr(32);
    alloc[0] = boxNativeTag(4);
    alloc[1] = std::sprintf((char*)(alloc + 2), "%" PRId64, i >> GC::valueFlagLength);
    return alloc;
  }

}

const boost::unordered_map<std::string, boost::container::map<boost::container::vector<int>, Ptr>> libraryIdMap {{
  {"0AAB32C474C86764F4EDD7B5F7975050", {{{86, 1}, (Ptr)0xDEADBEEF},
                                        {{86, 2}, (Ptr)0xDEADBEEF},
                                        {{std::initializer_list<int>{85}}, func(String::concat)},
                                        {{86, 38}, func(Core::i32ToInt)},
                                        {{33, 30}, func(Core::compareInfInt)},
                                        {{std::initializer_list<int>{112}}, func(List::hd)},
                                        {{std::initializer_list<int>{130}}, func(Core::print)}}},
  {"6959FE45225D4BD55C2D2CCD9C538D66", {{{0, 25}, func(LargeInt::toString)}}},
  {"15264C47F5ED8119799A5101E44495E0", {{{0, 6}, func(Time::toMilliseconds)}}},
  {"A44C25B0AB637462740A552AFDE72D60", {{{0, 5}, func(Timer::startRealTimer)},
                                        {{0, 7}, func(Timer::checkRealTimer)}}},
//  {"F6EF4B2BCBE3E725BB9EB04ACA2B6DCC", {{{0, 2}, func(Vector::tabulate}},
//                                        {{0, 13}, func()}}
}};

}

