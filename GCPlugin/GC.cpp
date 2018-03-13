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

// #define GC_MEMSET
#define GC_DEBUG_LOG_LVL 0

using namespace SMLCompiler::GC;

void* StackMapPtr;
statepoint_table_t* table;
uint8_t* currentStackPtr;

using heapUnit = uint64_t;

const uint64_t heapSize = 128'000; // 8KB
uint64_t sizeLeft = heapSize;
heapUnit *heapBase,
         *heapPtr; // points at the first free spot in the heap

heapUnit* allocate(std::size_t n) {
  return (heapUnit*)malloc(n * sizeof(heapUnit));
}


struct SelfRelocatingHeap {
  heapUnit*& ptr;
  uint64_t& sizeLeft;

  Heap heap;
  heapUnit *base; // points at the first free spot in the large heap
  double increaseFactor;
  uint64_t size;
  uint64_t nextSize;

  SelfRelocatingHeap(Heap heap, heapUnit*& ptr, std::size_t& sizeLeft, double incFactor, std::size_t size)
   : ptr(ptr), sizeLeft(sizeLeft), heap(heap), increaseFactor(incFactor), size(size), nextSize(size) {}

  void increaseNextSize() {
    nextSize = size * increaseFactor; // scale the heap
    std::cout << "Resizing heap to " << nextSize << '\n';
  }

  void cleanup(uint8_t* stackPtr);

  void init() {
    base = ptr = allocate(size);
  }
};

struct OldHeapT : SelfRelocatingHeap {
private:
  heapUnit* heap_ptr;
  uint64_t heap_size_left;

public:
  OldHeapT() : SelfRelocatingHeap(Heap::Old, heap_ptr, heap_size_left, 4.0, 1'000'000),
               heap_size_left(size) {}
} OldHeap;

heapUnit *referenceHeapPtr; // first free spot in the mutator heap
uint64_t referenceSizeLeft = 1'000;

SelfRelocatingHeap MutableHeap{
  Heap::Mutable,
  referenceHeapPtr,
  referenceSizeLeft,
  4.0,
  1'000
};

std::unordered_map<void*, std::size_t> const* closureLengths;

extern "C" void init() {
  heapBase = heapPtr = allocate(heapSize);
  MutableHeap.init();
  OldHeap.init();
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
    case Heap::Old: return is_in(OldHeap.base, OldHeap.size, p);
    case Heap::Mutable: return is_in(MutableHeap.base, MutableHeap.size, p);
    default: assert(false);
  }
}

Heap determineSource(heapUnit* ptr) {
  const bool in_small_heap = is_in(Heap::Young, ptr),
             in_mut_heap   = is_in(Heap::Mutable, ptr),
             in_large_heap = is_in(Heap::Old, ptr);
  return in_small_heap? Heap::Young
       : in_large_heap? Heap::Old
       : in_mut_heap? Heap::Mutable
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

// Return: successful? Or queue full?
template <typename Queue>
__attribute__((hot))
bool performRelocation(heapUnit** slot,
                       heapUnit*& newHeapPtr, std::size_t& units_left,
                       Heap cleanup_target, Heap reloc_target,
                       Queue& queue, std::size_t& queueLeft) {
  if (*slot == nullptr) // e.g. empty records, untagged data constructors, etc.
    return true;

  auto ptrTarget = determineSource(*slot);

  if (!canPointInto(ptrTarget, cleanup_target))
    return true;

  const auto tag = **slot;

  uint64_t len = isSingleUnitHeap(ptrTarget)? 1 : getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.

  if (len == 0) { // we're revisiting an already relocated record...
    *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
    return true;
  }
  else if (len > queueLeft)
    return false;

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
    if ((element & 1) == 0
     && (ptrTarget == cleanup_target || (heapUnit*)element != *slot)) // if we didn't relocate, avoid infinite recursion
    {
      *queue++ = ptr_to_elem;
      --queueLeft;
    }
  }

  return true;
}

/* Relocate a single pointer using a FIFO queue, modelling breadth-first traversal of the reachable object graph. */
void relocatePointer(
  heapUnit** slot,
  heapUnit*& newHeapPtr, std::size_t& units_left,
  Heap cleanup_target, Heap reloc_target)
{
  std::size_t maximumBreadth = 1'000,
              queue_1_left = maximumBreadth,
              queue_2_left = maximumBreadth;
  using heapUnitPtrPtr = heapUnit**;
  heapUnitPtrPtr queue_1[maximumBreadth], queue_2[maximumBreadth];
  auto base_1 = queue_1, base_2 = queue_2,
       end_1 = base_1, end_2 = base_2;
  *end_1++ = slot;
  do {
    for (; base_1 < end_1; base_1++)
      if (!performRelocation(*base_1, newHeapPtr, units_left, cleanup_target, reloc_target, end_2, queue_2_left)) {
        for (; base_1 < end_1; base_1++)
          relocatePointer(*base_1, newHeapPtr, units_left, cleanup_target, reloc_target);
        break;
      }
    base_1 = end_1 = queue_1;
    queue_1_left = maximumBreadth;

    for (; base_2 < end_2; base_2++)
      if (!performRelocation(*base_2, newHeapPtr, units_left, cleanup_target, reloc_target, end_1, queue_1_left)) {
        for (; base_2 < end_2; base_2++)
            relocatePointer(*base_2, newHeapPtr, units_left, cleanup_target, reloc_target);
        break;
      }
    base_2 = end_2 = queue_2;
    queue_2_left = maximumBreadth;
  } while (base_1 < end_1);
}

extern "C" [[gnu::naked]] void cleanupSmallHeap()   {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}
extern "C" [[gnu::naked]] void cleanupMutableHeap()  {
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
  walkStack(stackPtr, [&] (heapUnit** ptr) {
    if (((heapUnit)*ptr & 0b11) == 0)
      relocatePointer(ptr, newheapptr, size_left, heap, target);
  });
}

void SelfRelocatingHeap::cleanup(uint8_t* stackPtr) {
  auto aux_base = allocate(nextSize),
       aux_ptr = aux_base; // this is the bump pointer for relocation

  sizeLeft = size;
  cleanup_heap(stackPtr, heap, Heap::Invalid, aux_ptr, sizeLeft);

  sizeLeft += nextSize - size;
  std::swap(base, aux_base); // swap the heap bases
  ptr = aux_ptr; // and assign the bump pointer
  size = nextSize; // assign the new size

  std::free(aux_base);
}

// cleanup the small heap
void cleanup_heap(uint8_t* stackPtr, Heap heap) {
  if (!table)
    table = generate_table(StackMapPtr, 0.5);
  currentStackPtr = stackPtr;
#if GC_DEBUG_LOG_LVL >= 1
  static int counter;
  std::cout << "\nCleaning " << (int)heap << "; " << ++counter << "th collection.\n";
#endif // GC_DEBUG_LOG_LVL
  switch(heap) {
    case Heap::Young:
      if (OldHeap.sizeLeft <= 2*heapSize)
        cleanup_heap(stackPtr, Heap::Old);
      assert(OldHeap.sizeLeft > heapSize && "Cannot relocate into insufficient large heap");
      cleanup_heap(stackPtr, Heap::Young, Heap::Old, OldHeap.ptr, OldHeap.sizeLeft);

      // Traverse the mutable heap conservatively.
      for (auto ptr = MutableHeap.base; ptr < MutableHeap.ptr; ++ptr)
        if ((*ptr & 0b11) == 0)
          relocatePointer((heapUnit**)ptr, OldHeap.ptr, OldHeap.sizeLeft, Heap::Young, Heap::Old);

      heapPtr = heapBase;
      sizeLeft = heapSize;
      break;

    case Heap::Mutable:
      MutableHeap.cleanup(stackPtr);

      if (MutableHeap.sizeLeft < 0.02 * MutableHeap.size) { // if less than two percent of space is left,
        MutableHeap.increaseNextSize(); // increase the next allocation.
        MutableHeap.cleanup(stackPtr);
      }
    break;

    case Heap::Old:
      OldHeap.cleanup(stackPtr);

      if (OldHeap.sizeLeft < 2 * heapSize) // if less than two young heaps are left,
        OldHeap.increaseNextSize(); // increase the next allocation.
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
