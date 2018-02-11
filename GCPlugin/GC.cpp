#include "GCBasicConstants.hpp"

#include "llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h"

#include <cstddef>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <cinttypes>

#include <algorithm>
#include <functional>
#include <iostream>
#include <unordered_map>
#include <vector>

#define GC_DEBUG 1

using namespace SMLCompiler::GC;

void* StackMapPtr;
statepoint_table_t* table;

using heapUnit = uint64_t;

const uint64_t heapSize = 24;
uint64_t sizeLeft = heapSize;
heapUnit *heapBase,
         *heapPtr; // points at the first free spot in the heap

uint64_t largeHeapSize = 512;
uint64_t largeSizeLeft = heapSize;
heapUnit *largeHeapBase,
         *largeHeapPtr,
         *auxLargeHeap; // points at the first free spot in the large heap

const uint64_t referenceHeapSize = 24;
uint64_t referenceSizeLeft = referenceHeapSize;
heapUnit *referenceHeapBase,
         *referenceHeapPtr,
         *auxReferenceHeap; // first free spot in the mutator heap

const std::unordered_map<Heap, uint64_t> heapSizes = {
  {Heap::Young  , heapSize},
  {Heap::Mutable, referenceHeapSize},
  {Heap::Old    , largeHeapSize}
};

const std::unordered_map<Heap, std::reference_wrapper<heapUnit*>> heapBases = {
  {Heap::Young  , heapBase},
  {Heap::Mutable, referenceHeapBase},
  {Heap::Old    , largeHeapBase}
};

std::unordered_map<void*, std::size_t> const* closureLengths;

extern "C" void init() {
  heapBase = heapPtr = (heapUnit*)malloc(heapSize * sizeof(heapUnit));
  referenceHeapBase = (heapUnit*)malloc(referenceHeapSize * sizeof(heapUnit));
  largeHeapBase = largeHeapPtr = (heapUnit*)malloc(largeHeapSize * sizeof(heapUnit));
  auxLargeHeap = (heapUnit*)malloc(largeHeapSize * sizeof(heapUnit));
#ifdef GC_DEBUG
  std::cout << "Initialised GC!\n";
#endif // GC_DEBUG
}

bool is_in(heapUnit* base, uint64_t len, heapUnit* p) {
  return base <= p && p < base+len;
}

bool is_in(Heap h, heapUnit* p) {
  switch (h) {
    case Heap::Young: return is_in(heapBase, heapSize, p);
    case Heap::Old: return is_in(largeHeapBase, largeHeapSize, p);
    case Heap::Mutable: return is_in(referenceHeapBase, referenceHeapSize, p);
    default: __builtin_unreachable();
  }
}

Heap determineSource(heapUnit* ptr) {
  const bool in_small_heap = is_in(Heap::Young, ptr),
             in_mut_heap   = is_in(Heap::Mutable, ptr),
             in_large_heap = is_in(Heap::Old, ptr);
  assert(in_small_heap || in_mut_heap || in_large_heap);
  return in_small_heap? Heap::Young
       : in_large_heap? Heap::Old
       : Heap::Mutable;
}

uint64_t getRecordLength(uint64_t tag) {
  switch (tag & 0b11) {
  case 0:
    return closureLengths->at((void*)tag);
    break;
  case 0b01:
    return (tag & UINT32_MAX) >> 1;
    break;
  case 0b11:
    return 0;
  default: assert(0);
  }
}

void cleanup_heap(uint8_t* stackPtr, Heap heap);

void relocate(uint8_t* const stackPtr, heapUnit** slot,
              heapUnit*& newHeapPtr, std::size_t& units_left,
              Heap cleanup_target, Heap reloc_target)
{
  auto origin = determineSource(*slot);
  const uint64_t tag = **slot;

#if GC_DEBUG >= 2
  std::cout << "relocating an object of tag " << std::hex << tag << std::endl;
#endif // GC_DEBUG

  uint64_t len = isSingleUnitHeap(origin)? 1 : getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.
  if (len == 0) { // we're revisiting an already relocated record...
    *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
    return;
  }

  if (is_in(cleanup_target, *slot)) {
    if (len > units_left) // target heap ful?
      cleanup_heap(stackPtr, reloc_target);
    assert(len <= units_left); // we can fit this into the target heap?
    std::copy_n(*slot, len, newHeapPtr); // perform the relocation
    **slot = (heapUnit)newHeapPtr | 0b11; // assign the old spot a relocation forward reference to the target
    *slot = newHeapPtr; // replace old pointer value
    newHeapPtr += len; // bump heap pointer
    units_left -= len;
  }

  for (int i = isSingleUnitHeap(origin)? 0 : 1; i < len; ++i) {
    auto element = (*slot)[i];
    if (element & 0b11) // not a pointer
      continue;
    auto elem_as_ptr = (heapUnit**)*slot + i;
    if (!*elem_as_ptr)
      continue;
    // if this pointee could indirectly reference the relocated heap..
    auto origin = determineSource(*elem_as_ptr);
    if (canPointInto.count({origin, cleanup_target}) && elem_as_ptr != slot)
      relocate(stackPtr, elem_as_ptr, newHeapPtr, units_left, cleanup_target, reloc_target);
  }
}

extern "C" void __attribute__((naked)) cleanupSmallHeap() {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}
extern "C" void __attribute__((naked)) cleanupMutableHeap() {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}

template <typename F>
void walkStack(uint8_t* stackPtr, F slotHandler) {
  intptr_t retAddr = *((intptr_t*)stackPtr);
  stackPtr += sizeof(void*); // step into frame
  frame_info_t* frame = lookup_return_address(table, retAddr);

  while(frame != NULL) {
    for(uint16_t i = 0; i < frame->numSlots; i++) {
      pointer_slot_t ptrSlot = frame->slots[i];
      if(ptrSlot.kind >= 0) {
        // we do not use derived pointers
        assert(false && "unexpected derived pointer\n");
      }

      heapUnit** ptr = (heapUnit**)(stackPtr + ptrSlot.offset);
      slotHandler(ptr);
    }

    // move to next frame. seems we have to add one pointer size to
    // reach the next return address? NOTE
    stackPtr += frame->frameSize;

    // grab return address of the frame
    retAddr = *((intptr_t*)stackPtr);
    stackPtr += sizeof(void*); // step into frame
    frame = lookup_return_address(table, retAddr);
  }
}

// cleanup the small heap
void cleanup_heap(uint8_t* stackPtr, Heap heap, Heap target, heapUnit*& newheapptr, uint64_t& size_left) {
  const auto originalStackPtr = stackPtr;
  if (!table) {
    table = generate_table(StackMapPtr, 0.5);
    print_table(stdout, table, true);
  }

  walkStack(stackPtr, [&] (heapUnit** ptr) {
    if (((intptr_t)*ptr & 0b11) == 0) {
      relocate(originalStackPtr, ptr, newheapptr, size_left, heap, target);
    }
  });

  // Overwrite the cleaned memory to recognise wrong reads.
  memset(heapBases.at(heap), 0x7F, heapSizes.at(heap) * sizeof(heapUnit));
}

// cleanup the small heap
void cleanup_heap(uint8_t* stackPtr, Heap heap) {
#if GC_DEBUG >= 1
  std::cout << "Cleaning " << (int)heap << '\n';
#endif // GC_DEBUG
  switch(heap) {
    case Heap::Young:
      cleanup_heap(stackPtr, Heap::Young, Heap::Old, largeHeapPtr, largeSizeLeft);
      heapPtr = heapBase;
      sizeLeft = heapSize;
      break;

    case Heap::Mutable: {
      auto aux_ptr = auxReferenceHeap; // this is the bump pointer for relocation
      cleanup_heap(stackPtr, Heap::Mutable, Heap::Invalid, aux_ptr, referenceSizeLeft = referenceHeapSize);
      std::swap(referenceHeapBase, auxReferenceHeap); // swap the heap bases
      referenceHeapPtr = aux_ptr; // and assign the bump pointer
    }
    break;

    case Heap::Old: {
      auto aux_ptr = auxLargeHeap; // this is the bump pointer for relocation
      cleanup_heap(stackPtr, Heap::Old, Heap::Invalid, aux_ptr, largeSizeLeft = largeHeapSize);
      std::swap(largeHeapBase, auxLargeHeap); // swap the heap bases
      largeHeapPtr = aux_ptr; // and assign the bump pointer
    }
    break;

    default:
      assert(false && "Cannot clean invalid heap! Exceeded fixed heap size?");
  }
}

extern "C" void cleanup_small_heap(uint8_t* stackPtr) {
  return cleanup_heap(stackPtr, Heap::Young);
}
extern "C" void cleanup_mutable_heap(uint8_t* stackPtr) {
  return cleanup_heap(stackPtr, Heap::Mutable);
}
