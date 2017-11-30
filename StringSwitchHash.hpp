#pragma once

#include <cstdint>

#include <string_view>

constexpr std::uint64_t fnv(std::string_view view, std::uint64_t h = 0xcbf29ce484222325) {
  return view.empty()? h : fnv(view.substr(1), (h ^ view.front()) * 0x00000100000001b3);
}

constexpr std::uint64_t operator "" _fnv( char const* str, std::size_t len ) {
  return fnv(str, len);
}
