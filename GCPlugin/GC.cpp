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
#include <queue>
#include <unordered_map>
#include <vector>

#include <boost/circular_buffer.hpp>

// #define GC_MEMSET
#define GC_DEBUG_LOG_LVL 0

using namespace SMLCompiler::GC;

void* StackMapPtr;
statepoint_table_t* table;
uint8_t* currentStackPtr;

using heapUnit = uint64_t;

const uint64_t heapSize = 8000; // 8KB
uint64_t sizeLeft = heapSize;
heapUnit *heapBase,
         *heapPtr; // points at the first free spot in the heap

const double increaseFactor = 4;
uint64_t largeHeapSize = 500'000'000;
uint64_t largeSizeLeft = largeHeapSize;
uint64_t nextLargeHeapSize = largeHeapSize;
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
  referenceHeapBase = referenceHeapPtr = (heapUnit*)malloc(referenceHeapSize * sizeof(heapUnit));
  largeHeapBase = largeHeapPtr = (heapUnit*)malloc(largeHeapSize * sizeof(heapUnit));
#ifdef GC_MEMSET
  memset(largeHeapBase, 0x77, largeHeapSize * sizeof(heapUnit));
  memset(heapBase, 0x77, heapSize * sizeof(heapUnit));
#endif
  std::cout << "Initialised GC!\n";
}

bool is_in(heapUnit* base, uint64_t len, heapUnit* p) {
  return base <= p && p < base+len;
}

bool is_in(Heap h, heapUnit* p) {
  switch (h) {
    case Heap::Young: return is_in(heapBase, heapSize, p);
    case Heap::Old: return is_in(largeHeapBase, largeHeapSize, p);
    case Heap::Mutable: return is_in(referenceHeapBase, referenceHeapSize, p);
    default: assert(false);
  }
}

Heap determineSource(heapUnit* ptr) {
  const bool in_small_heap = is_in(Heap::Young, ptr),
             in_mut_heap   = is_in(Heap::Mutable, ptr),
             in_large_heap = is_in(Heap::Old, ptr),
             in_aux_large  = auxLargeHeap && is_in(auxLargeHeap, largeHeapSize, ptr),
             in_aux_ref    = auxReferenceHeap && is_in(auxReferenceHeap, referenceHeapSize, ptr);
  // assert(in_small_heap || in_mut_heap || in_large_heap || in_aux_ref || in_aux_large);
  return in_small_heap? Heap::Young
       : in_large_heap? Heap::Old
       : in_mut_heap || in_aux_ref? Heap::Mutable
       : Heap::Invalid;
}

uint64_t getRecordLength(uint64_t tag) {
  switch (tag & 0b11) {
  case 0:
    return closureLengths->at((void*)tag);
    break;
  case 0b01:
    return (tag & UINT32_MAX) >> 2;
    break;
  case 0b11:
    return 0;
  default: assert(0);
  }
}

void cleanup_heap(uint8_t* stackPtr, Heap heap);

template <typename Queue>
__attribute__((hot))
void performRelocation(heapUnit** slot,
                       heapUnit*& newHeapPtr, std::size_t& units_left,
                       Heap cleanup_target, Heap reloc_target,
                       Queue& queue) {
  if (*slot == nullptr) // e.g. empty records, untagged data constructors, etc.
    return;

  auto ptrTarget = determineSource(*slot);

  if (!canPointInto(ptrTarget, cleanup_target))
    return;

  const auto tag = **slot;

  uint64_t len = isSingleUnitHeap(ptrTarget)? 1 : getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.

  if (len == 0) { // we're revisiting an already relocated record...
    *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
    return;
  }

  if (ptrTarget == cleanup_target) {
//    if (len > units_left) // target heap full?
//      cleanup_heap(currentStackPtr, reloc_target);
    assert(len <= units_left); // we can fit this into the target heap?
    std::copy_n(*slot, len, newHeapPtr); // perform the relocation
    **slot = (heapUnit)newHeapPtr | 0b11; // assign the old spot a relocation forward reference to the target
    *slot = newHeapPtr; // replace old pointer value
    newHeapPtr += len; // bump heap pointer
    units_left -= len;
  }

  #pragma unroll 4
  for (int i = isSingleUnitHeap(ptrTarget)? 0 : 1; i < len; ++i) {
    auto ptr_to_elem = (heapUnit**)*slot + i;
    auto element = (heapUnit)*ptr_to_elem;
    if ((element & 1) == 0 && ptr_to_elem != slot)
      queue.push_back(ptr_to_elem);
  }
}

void relocateStackPtr(
  heapUnit** slot,
  heapUnit*& newHeapPtr, std::size_t& units_left,
  Heap cleanup_target, Heap reloc_target)
{
  boost::circular_buffer<heapUnit**> queue(1000);
  queue.push_back(slot);
  while (!queue.empty()) {
    performRelocation(queue.front(), newHeapPtr, units_left, cleanup_target, reloc_target, queue);
    queue.pop_front();
  }
}

extern "C" [[gnu::naked]] void cleanupSmallHeap()   {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}
extern "C"  [[gnu::naked]] void cleanupMutableHeap()  {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}

template <typename F>
void walkStack(uint8_t* stackPtr, F slotHandler) {
  intptr_t retAddr = *((intptr_t*)stackPtr);
  stackPtr += sizeof(void*); // step into frame
  frame_info_t* frame = lookup_return_address(table, retAddr);
  std::vector<heapUnit*> lastPointers;


  while(frame != NULL) {
    for(uint16_t i = 0; i < frame->numSlots; i++) {
      pointer_slot_t ptrSlot = frame->slots[i];
      heapUnit** ptr = (heapUnit**)(stackPtr + ptrSlot.offset);
      std::ptrdiff_t diff = 0;
      if(ptrSlot.kind >= 0) {
        diff = *ptr - lastPointers.at(ptrSlot.kind);
        // we do not use derived pointers
        // assert(false && "unexpected derived pointer\n");
      }
      lastPointers.push_back(*ptr);
      *ptr -= diff;
      slotHandler(ptr);
      *ptr += diff;
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
  if (!table)
    table = generate_table(StackMapPtr, 0.5);

  walkStack(stackPtr, [&] (heapUnit** ptr) {
    if (((intptr_t)*ptr & 0b11) == 0)
      relocateStackPtr(ptr, newheapptr, size_left, heap, target);
  });
}

// cleanup the small heap
void cleanup_heap(uint8_t* stackPtr, Heap heap) {
  currentStackPtr = stackPtr;
#if GC_DEBUG_LOG_LVL >= 1
  static int counter;
  std::cout << "\nCleaning " << (int)heap << "; " << ++counter << "th collection.\n";
#endif // GC_DEBUG_LOG_LVL
  switch(heap) {
    case Heap::Young:
      if (largeSizeLeft <= 2*heapSize)
        cleanup_heap(stackPtr, Heap::Old);
      assert(largeSizeLeft > heapSize && "Cannot relocate into insufficient large heap");
      cleanup_heap(stackPtr, Heap::Young, Heap::Old, largeHeapPtr, largeSizeLeft);
      heapPtr = heapBase;
      sizeLeft = heapSize;
      break;

    case Heap::Mutable: {
      auto aux_ptr = auxReferenceHeap; // this is the bump pointer for relocation
      cleanup_heap(stackPtr, Heap::Mutable, Heap::Invalid, aux_ptr, referenceSizeLeft = referenceHeapSize);
      std::swap(referenceHeapBase, auxReferenceHeap); // swap the heap bases
      referenceHeapPtr = aux_ptr; // and assign the bump pointer
//      std::free(auxReferenceHeap);
//      auxReferenceHeap = nullptr;
    }
    break;

    case Heap::Old: {
      auxLargeHeap = (heapUnit*)std::malloc(nextLargeHeapSize * sizeof(heapUnit));
      auto aux_ptr = auxLargeHeap; // this is the bump pointer for relocation
      largeSizeLeft = largeHeapSize;
      cleanup_heap(stackPtr, Heap::Old, Heap::Invalid, aux_ptr, largeSizeLeft);

      largeSizeLeft += nextLargeHeapSize - largeHeapSize;
      std::swap(largeHeapBase, auxLargeHeap); // swap the heap bases
      largeHeapPtr = aux_ptr; // and assign the bump pointer
      largeHeapSize = nextLargeHeapSize; // assign the new size

      std::free(auxLargeHeap);
      auxLargeHeap = nullptr;

      if (largeSizeLeft < 2 * heapSize) { // if less than two young heaps is left,
        nextLargeHeapSize = largeHeapSize * increaseFactor; // scale the heap
        std::cout << "Resizing heap to " << nextLargeHeapSize << '\n';
      }
    }
    break;

    default:
      assert(false && "Cannot clean invalid heap! Exceeded fixed heap size?");
  }

#if GC_DEBUG_LOG_LVL >= 1
  std::cout << "Finished " << counter << "\n\n";
#endif // GC_DEBUG_LOG_LVL
}

extern "C" void cleanup_small_heap(uint8_t* stackPtr) {
  cleanup_heap(stackPtr, Heap::Young);

  // Overwrite the cleaned memory to recognise wrong reads.
#ifdef GC_MEMSET
  memset(heapBase, 0x7F, heapSize * sizeof(heapUnit));
#endif
}
extern "C" void cleanup_mutable_heap(uint8_t* stackPtr) {
  cleanup_heap(stackPtr, Heap::Mutable);

  // Overwrite the cleaned memory to recognise wrong reads.
#ifdef GC_MEMSET
  memset(referenceHeapBase, 0x7F, referenceHeapSize * sizeof(heapUnit));
#endif
}
