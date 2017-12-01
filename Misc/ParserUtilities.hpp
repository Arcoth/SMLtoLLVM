#pragma once

#include "AbsynInterfaceBasic.hpp"
#include "StringSwitchHash.hpp"

#include <iostream>
#include <string>

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

// yields a potentially empty string! Matches ([a-zA-z][a-zA-z0-9]*)?
inline std::string parse_identifier_nounsc(std::istream& is) {
  return extract_cond(is, [] (auto& s, char c) {
    return std::isalpha(c) || (std::isdigit(c) && !s.empty());});
}

template <typename... T>
std::istream& on_error(std::istream& is, T&&... t) {
  (std::cerr << ... << std::forward<T>(t)) << '\n';
  std::cerr << "Remaining buffer: " << is.rdbuf() << '\n';
  is.setstate(std::ios::failbit); // perform this after any logging, might throw
  return is;
}

template <char C>
std::istream& char_(std::istream& is) {
  char c;
  is >> c;
  using namespace std::literals;
  if (c != C) {
    is.putback(c);
    on_error( is, "char_ expected "s, C, " instead of ", c );
  }

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
    is >> vec.back() >> std::ws >> c;
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

}
