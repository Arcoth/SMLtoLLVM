#pragma once

#include <set>
#include <utility>

namespace SMLCompiler::GC {

enum class Heap {
  Invalid,
  Young, Old, Mutable
};

constexpr bool canPointInto(Heap from, Heap to) {
  if (from == Heap::Young)
    return true;

  return to != Heap::Young;
}

constexpr bool isSingleUnitHeap(Heap h) {
  return h == Heap::Mutable;
}

const uint64_t recordTagLen = 2,
               valueTagLen = 1;

// These constants represent the values of the two least significant
// bits for each corresponding kind of object.
const uint64_t pointerTag          = 0, // for pointers into the heap.
               intTag              = 0b1, // for unboxed integers saved with pointer type.
               floatTag            = 0b1, // for an unboxed float saved with pointer type.
               lengthTag           = 0b01, // for the length field in a heap record tag.
               relocatedPointerTag = 0b11; // for a relocation reference during garbage collection.

}
