#pragma once

#include <cstdint>

#include <string_view>

constexpr std::uint64_t fnv(std::string_view view) {
  std::uint64_t h = 0xcbf29ce484222325;
  for (auto c : view)
    h = (h ^ c) * 0x00000100000001b3;
  return h;
}

constexpr std::uint64_t operator "" _fnv( char const* str, std::size_t len ) {
  return fnv(std::string_view{str, len});
}
