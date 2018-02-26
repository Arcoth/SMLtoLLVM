#pragma once

#include <set>
#include <utility>

namespace SMLCompiler::GC {

enum class Heap {
  Invalid,
  Young, Old, Mutable
};

bool canPointInto(Heap from, Heap to) {
  if (from == Heap::Young)
    return true;

  return to != Heap::Young;
}

//inline const std::set<std::pair<Heap, Heap>> canPointInto = {
//  {Heap::Young, Heap::Young},
//  {Heap::Young, Heap::Old},
//  {Heap::Young, Heap::Mutable},
//  {Heap::Old, Heap::Mutable},
//  {Heap::Old, Heap::Old},
//  {Heap::Mutable, Heap::Old}
//  {Heap::Mutable, Heap::Mutable},
//};

inline bool isSingleUnitHeap(Heap h) {
  return h == Heap::Mutable;
}

// These constants represent the values of the two least significant
// bits for each corresponding kind of object.
const int pointerTag          = 0b00, // for pointers into the heap.
          intTag              = 0b01, // for unboxed integers saved with pointer type.
          lengthTag           = 0b01, // for the length field in a heap record tag.
          floatTag            = 0b10, // for an unboxed float saved with pointer type.
          relocatedPointerTag = 0b11; // for a relocation reference during garbage collection.

}
