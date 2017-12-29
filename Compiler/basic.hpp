#pragma once

#include <exception>
#include <ostream>
#include <set>
#include <string>

namespace SMLCompiler{

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
  return move(s);
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
