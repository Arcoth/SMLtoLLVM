#pragma once

#include "AbsynInterfaceBasic.hpp"
#include "Lty.hpp"
#include "Misc/StringSwitchHash.hpp"

#include <iomanip>
#include <iostream>
#include <string>
#include <cxxabi.h>

#define DEBUG_PRINT(...) std::cerr << __FILE__ << '/' << __LINE__ << ": " << __VA_ARGS__ << '\n';


namespace SMLNJInterface::Parser {

inline bool starts_with(std::string_view a, std::string_view b) {
  return a.substr(0, b.length()) == b;
}
template <typename T, typename U>
bool ends_with(T const& s, U u) {
  return std::empty(s)? false : *std::prev(std::end(s)) == u;
}

template <typename Pred>
std::string extract_cond(std::istream& is, Pred pred) {
  std::string s;
  is >> std::ws;
  int c;
  while ((c = is.get()) != EOF) {
    if (pred(s, c))
      s += c;
    else {
      is.putback(c);
      break;
    }
  }

  if (c == EOF)
    is.clear(is.rdstate() & ~std::ios::failbit);

  return s;
}

// yields a potentially empty string! Matches ([a-zA-z_][a-zA-z0-9_]*)?
inline std::string parse_identifier(std::istream& is) {
  return extract_cond(is, [] (auto& s, char c) {
    return std::isalpha(c) || (std::isdigit(c) && !s.empty()) || c == '_';});
}

template <typename... T>
std::istream& on_error(std::istream& is, T&&... t) {
  (std::cerr << ... << std::forward<T>(t)) << '\n';
  std::cerr << "Remaining buffer at position "<< is.tellg() <<": " << is.rdbuf() << '\n';
  is.setstate(std::ios::failbit); // perform this after any logging, might throw
  return is;
}

inline void expect(std::istream& is, char C) {
  char c;
  if (!(is >> c))
    on_error(is, "Failed to read ", C);
  else if (c != C) {
    is.putback(c);
    on_error( is, "char_ expected ", C, " instead of ", c );
  }
}

template <char C>
std::istream& char_(std::istream& is) {
  expect(is, C);
  return is;
}

struct string_ : std::string_view {
  string_(std::string_view s) : std::string_view{s} {}
};
inline std::istream& operator>>(std::istream& is, string_ s) {
  auto t = extract_cond(is, [&s] (auto& str, char) {
    return str.length() < s.length()
        && str.back() == s[str.length()-1];
  });
  if (t.length() != s.length())
    on_error(is, "Expected ", s, ", got ", t);
  return is;
}

template <typename T>
T parenthesized(std::istream& is) {
  T t;
  is >> char_<'('> >> t >> char_<')'>;
  return t;
}

template <typename T, char Start, char Sep, char Delim>
std::vector<T> parse_sequence(std::istream& is) {
  std::vector<T> vec;
  is >>  char_<Start> >> std::ws;
  if (is.peek() == Delim) {
    is.ignore();
    return vec;
  }
  for(;;) {
    vec.emplace_back();
    char c;
    is >> vec.back() >> c;
    if (c != Sep) {
      if(c != Delim)
        on_error(is, "parse_sequence interrupted by ", c);
      break;
    }
  }
  return vec;
}


template <typename T, char Sep>
std::vector<T> parse_nonempty_sequence(std::istream& is) {
  std::vector<T> vec;
  is >> std::ws;
  for(;;) {
    vec.emplace_back();
    is >> vec.back() >> std::ws;
    if (is.peek() != Sep)
      break;
    is.ignore();
  }
  return vec;
}

template <typename T>
std::vector<T> parse_list(std::istream& is) {
  auto v = parse_sequence<T, '[', ',', ']'>(is);
  return v;
}

inline unsigned parse_var(std::istream& is) {
  unsigned i;
  is >> char_<'v'> >> i;
  return i;
}

template<typename T>
std::string type_name()
{
    int status;
    std::string tname = typeid(T).name();
    char *demangled_name = abi::__cxa_demangle(tname.c_str(), NULL, NULL, &status);
    if(status == 0) {
        tname = demangled_name;
        std::free(demangled_name);
    }
    return tname;
}

template <typename T>
struct var_tag{using type = T;}; // Used to signal to parse that the integer to extract is expressed as v[...].
template <typename> constexpr bool is_var = false;
template <typename T> constexpr bool is_var<var_tag<T>> = true;

template <typename T, typename... Ts>
auto parse(std::istream& is, T t, Ts&&... ts);
template <typename T, typename... Ts>
auto parse(std::istream& is, vector<T> v, Ts&&... ts) {
  v = parse_list<T>(is);
  return std::tuple_cat(std::tuple{move(v)}, parse(is, std::forward<Ts>(ts)...));
}
template <typename T, typename... Ts>
auto parse(std::istream& is, T t, Ts&&... ts) {
  auto rest = [&] {
    if constexpr (sizeof...(ts) > 0)
      return parse(is, std::forward<Ts>(ts)...);
    else
      return std::tuple{};
  };
  if constexpr (std::is_same_v<T, char>) {
    expect(is, t);
    return rest();
  }
  else if constexpr (std::is_convertible_v<T&, std::string_view>) {
    is >> string_{std::string_view{t}};
    return rest();
  }
  else if constexpr (is_var<T>) {
    typename T::type t = parse_var(is);
    return std::tuple_cat(std::tuple{t}, rest());
  }
  else {
    is >> t;
    return std::tuple_cat(std::tuple{t}, rest());
  }
}
template <auto I, typename... Args, typename... Ts>
void parse_into(std::variant<Args...>& v, std::istream& is, Ts&&... ts) {
  auto tup = parse(is, std::forward<Ts>(ts)...);
  if (!is)
    std::cerr << "\n\tFailed parsing " << I << ", "
              << type_name<std::variant_alternative_t<I, std::variant<Args...>>>() << '\n';
  else
    std::apply([&v] (auto&&... x) {v.template emplace<I>(std::forward<decltype(x)>(x)...);}, move(tup));
}

}

