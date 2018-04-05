#include "SMLLibrary.hpp"

#include <cmath>
#include <chrono>
#include <iostream>

#include <cinttypes>

#include <boost/math/constants/constants.hpp>

namespace SMLCompiler::LibraryImpl {

template <typename F, typename... Args>
auto func(F f, Args... args) {
  return (Ptr)new genericFunctionTypeNative*[sizeof...(args) + 1]
         {(genericFunctionTypeNative*)f, args...};
}

Int boxReal(double d) {
  Int i;
  std::memcpy(&i, &d, sizeof d);
  return i | 1;
}

double unboxReal(Int i) {
  i &= ~1; // unbox float
  char c[sizeof(double)];
  std::memcpy(c, &i, sizeof c);
  return reinterpret_cast<double&>(c);
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
  Ptr startRealTimer(Ptr, Ptr[]) {
    return boxNative(clock_type::now().time_since_epoch().count());
  }
  Ptr checkRealTimer(Ptr arg, Ptr[]) {
    return boxNative(clock_type::now().time_since_epoch().count() - unboxNative(arg));
  }

}

namespace Time {
  Ptr toMilliseconds(Ptr arg, Ptr[]) {
    return boxNative(unboxNative(arg)/(Timer::clock_type::duration::period::den/1000));
  }
}

//namespace Vector {
//  Ptr tabulate(Ptr* args, Ptr[]) {
//    const auto fun = (Ptr(*)(Int))args[1];
//    const auto len = unboxNoCast(args[0])+1;
//    const auto begin = (Int*)allocate_large_ptr(len);
//    *begin = ((n+1) << 2) | 1;
//    for (std::size_t i = 1; i < len; ++i)
//      begin[i] = fun(boxNoCast(i-1));
//    return begin;
//  }
//
//  Ptr tabulate(Int n, Ptr[]) {
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
  auto length(tagTypeNative* str, Ptr[] = nullptr) {
    return str[1];
  }
  auto concat(tagTypeNative** record, Ptr[]) {
    auto len = 8 + length(record[1]) + length(record[2]);
    auto alloc = (tagTypeNative*)allocate_large_ptr(len);
    alloc[0] = boxNativeTag((len+7)/8);
    alloc[1] = len - 8;
    auto str_ptr = (char*)alloc + 8;
    std::memcpy(str_ptr                    , record[1] + 2, length(record[1]));
    std::memcpy(str_ptr + length(record[1]), record[2] + 2, length(record[2]));
    return alloc;
  }
}

namespace Core {
  Ptr i32ToInt(Ptr arg, Ptr[]) {
    return arg;
  }

  Ptr compareInfInt(Int* rcd, Ptr[]) {
    return boxNative(rcd[1] < rcd[2]);
  }

  Ptr print(tagTypeNative* str, Ptr[]) {
    std::cout.write((char const*)(str + 2), String::length(str, nullptr)).flush();
    return nullptr;
  }
}

namespace Real {

  tagTypeNative* toString(Int i, Ptr[]) {
    auto f = unboxReal(i);
    auto alloc = (tagTypeNative*)allocate_large_ptr(32);
    alloc[0] = boxNativeTag(4);
    alloc[1] = std::sprintf((char*)(alloc + 2), "%.12f", f);
    return alloc;
  }

  Int realFloor(Int i, Ptr[]) {
    return boxReal(std::floor(unboxReal(i)));
  }

  Int intFloor(Int i, Ptr[]) {
    return boxNoCast((Int)std::floor(unboxReal(i)));
  }

}

namespace LargeInt {

  tagTypeNative* toString(Int i, Ptr[]) {
    auto alloc = (tagTypeNative*)allocate_large_ptr(32);
    alloc[0] = boxNativeTag(4);
    alloc[1] = std::sprintf((char*)(alloc + 2), "%" PRId64, i >> GC::valueFlagLength);
    return alloc;
  }

}

namespace Math {

  Int cosh(Int i, Ptr[]) {
    return boxReal(std::cosh(unboxReal(i)));
  }
  Int sinh(Int i, Ptr[]) {
    return boxReal(std::sinh(unboxReal(i)));
  }
  Int ln(Int i, Ptr[]) {
    return boxReal(std::log(unboxReal(i)));
  }
  Int atan(Int i, Ptr[]) {
    return boxReal(std::atan(unboxReal(i)));
  }
  Int exp(Int i, Ptr[]) {
    return boxReal(std::exp(unboxReal(i)));
  }
  const Int pi = boxReal(boost::math::constants::pi<double>());

}

namespace List {

  Ptr hd(Ptr arg, Ptr[]) {
    assert(arg);
    return ((Ptr*)arg)[1];
  }

  Int length(Ptr* record, Ptr[]) {
    Int l = 0;
    for (; record; ++l)
      record = (Ptr*)record[2];
    return boxNoCast(l);
  }

  Ptr* drop(Ptr* record, Ptr[]) {
    auto k = unboxNative(record[2]);
    record = (Ptr*)record[1];
    while (k--) {
      record = (Ptr*)record[2];
      if (UNLIKELY(!record)) {
        std::cerr << "List::drop: not enough elements in list!\n";
        std::abort();
      }
    }
    return record;
  }

}

const boost::unordered_map<std::string, boost::container::map<boost::container::vector<int>, Ptr>> libraryIdMap {{
  {"0AAB32C474C86764F4EDD7B5F7975050", {{{86, 1}, (Ptr)0xDEADBEEF},
                                        {{86, 0, 1}, (Ptr)0xDEADBEEF},
                                        {{86, 2}, (Ptr)0xDEADBEEF},
                                        {{79}, (Ptr)0xDEADBEEF},
                                        {{std::initializer_list<int>{85}}, func(String::concat)},
                                        {{86, 38}, func(Core::i32ToInt)},
                                        {{33, 30}, func(Core::compareInfInt)},
                                        {{std::initializer_list<int>{112}}, func(List::hd)},
                                        {{std::initializer_list<int>{130}}, func(Core::print)}}},
  {"96B2FB8000CFA053281A20BA159C7DD2",  {{{0, 8}, func(List::drop)},
                                         {{0, 9}, func(List::length)},
                                         {{0, 2}, func(List::hd)}}},
  {"6959FE45225D4BD55C2D2CCD9C538D66", {{{0, 25}, func(LargeInt::toString)}}},
  {"CAD9434B073913D9EA0BF5829FBCA04E", {{{0, 25}, func(LargeInt::toString)}}},
  {"15264C47F5ED8119799A5101E44495E0", {{{0, 6}, func(Time::toMilliseconds)}}},
  {"A44C25B0AB637462740A552AFDE72D60", {{{0, 5}, func(Timer::startRealTimer)},
                                        {{0, 7}, func(Timer::checkRealTimer)}}},
  {"E1310EABC08F1107773C2652358627F1", {{{0, 37}, func(Real::toString)},
                                        {{0, 47}, func(Real::intFloor)},
                                        {{0, 51}, func(Real::realFloor)}}},
  {"F4360A6CAB125CD5827C4328E911CBCF", {{{0, 15}, func(Math::cosh)},
                                        {{0, 14}, func(Math::sinh)},
                                        {{0, 12}, func(Math::ln)},
                                        {{0, 0}, (Ptr)Math::pi},
                                        {{0, 10}, func(Math::exp)},
                                        {{0, 8}, func(Math::atan)},

                                        }}
//  {"F6EF4B2BCBE3E725BB9EB04ACA2B6DCC", {{{0, 2}, func(Vector::tabulate}},
//                                        {{0, 13}, func()}}
}};

}

