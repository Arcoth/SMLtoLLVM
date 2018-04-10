#include "GC.h"

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

const uint64_t smallHeapSize = 64000; // 8KB
uint64_t smallSizeLeft = smallHeapSize;
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

  SelfRelocatingHeap(Heap heap,
                     heapUnit*& ptr,
                     std::size_t& sizeLeft,
                     double incFactor,
                     std::size_t size)
   : ptr(ptr), sizeLeft(sizeLeft), heap(heap), increaseFactor(incFactor),
     size(size), nextSize(size) {}

  void increaseNextSize() {
    nextSize = size * increaseFactor; // scale the heap
#if GC_DEBUG_LOG_LVL >= 1
    std::cout << "Resizing heap " << (int) heap << " to " << nextSize << '\n';
#endif
  }

  void cleanup(uint8_t* stackPtr);

  void init() {
    base = ptr = allocate(size);
  }
};

heapUnit *referenceHeapPtr; // first free spot in the mutator heap
uint64_t referenceSizeLeft = 1'000;

heapUnit* largeHeapPtr;
uint64_t largeSizeLeft = 1000000;

SelfRelocatingHeap OldHeap {
  Heap::Old,
  largeHeapPtr,
  largeSizeLeft,
  4.0,
  largeSizeLeft
};

SelfRelocatingHeap MutableHeap{
  Heap::Mutable,
  referenceHeapPtr,
  referenceSizeLeft,
  4.0,
  referenceSizeLeft
};

std::unordered_map<void*, std::size_t> closureLengths;

extern "C" void init() {
  heapBase = heapPtr = allocate(smallHeapSize);
  MutableHeap.init();
  OldHeap.init();
#if GC_DEBUG_LOG_LVL >= 1
  std::cout << "Initialised GC!\n";
#endif
}

bool is_in(heapUnit* base, uint64_t len, heapUnit* p) {
  return base <= p && p < base+len;
}

bool is_in(Heap h, heapUnit* p) {
  switch (h) {
    case Heap::Young: return is_in(heapBase, smallHeapSize, p);
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
    try {
      return closureLengths.at((void*)tag);
    } catch(std::out_of_range const&) { // Library closures?
      std::cerr << "\nGC: Function tag not found: " << std::hex << tag << std::dec << '\n';
      throw;
    }
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

// For
template <typename Queue>
__attribute__((hot))
bool performRelocation(heapUnit** slot,
                       heapUnit*& newHeapPtr, std::size_t& units_left,
                       Heap cleanup_target,
                       Queue& queue, std::size_t& queueLeft) {
  auto ptrTarget = determineSource(*slot);

  if (!canPointInto(ptrTarget, cleanup_target)) {
    //  std::cout << "Aborting: " << *slot << " from " << (int)ptrTarget << " cannot point to " << (int)cleanup_target << std::endl;
    return true;
  }

  const auto tag = **slot;

  uint64_t len = isFixedContentHeap(ptrTarget)? 1: getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.

  if (len > queueLeft)
        return false;

  if (ptrTarget == cleanup_target) {
    if (len == 0) { // we're revisiting an already relocated record...
      *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
      return true;
    }


    assert(len <= units_left); // we can fit this into the target heap?
    std::copy_n(*slot, len, newHeapPtr); // perform the relocation
    **slot = (heapUnit)newHeapPtr | relocatedPointerTag; // assign the old spot a relocation forward reference to the target
    *slot = newHeapPtr; // replace old pointer value
    newHeapPtr += len; // bump heap pointer
    units_left -= len;
  }

  if (isFixedContentHeap(ptrTarget) == 1) {
    auto element = (heapUnit)**slot;
    if ((element & 1) == 0)
      *queue++ = (heapUnit**)*slot;
    return true;
  }
  assert(isFixedContentHeap(ptrTarget) == 0); // Don't support other atm

  #pragma unroll 4
  for (unsigned i = 1; i < len; ++i) {
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

// Return: successful? Or queue full?
template <typename Queue>
__attribute__((hot))
bool performRelocation(heapUnit** slot,
                       heapUnit*& newHeapPtr, std::size_t& units_left,
                       Heap cleanup_target,
                       Queue& queue, std::size_t& queueLeft,
                       heapUnit* base,
                       std::vector<bool>& is_relocated) {

  auto ptrTarget = determineSource(*slot);

  if (!canPointInto(ptrTarget, cleanup_target))
    return true;

  const auto tag = **slot;

  if (ptrTarget == cleanup_target) {
    if (is_relocated[*slot - base]) { // we're revisiting an already relocated record...
      *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
      return true;
    }
    else if (queueLeft == 0)
      return false;

    assert(units_left); // we can fit this into the target heap?
    *newHeapPtr = **slot; // relocation
    **slot = (heapUnit)newHeapPtr | 0b11; // assign the old spot a relocation forward reference to the target
    is_relocated[*slot - base] = true; // enter in relocation table
    *slot = newHeapPtr; // replace old pointer value
    newHeapPtr += 1; // bump heap pointer
    units_left -= 1;

    // inspect the element
    auto element = (heapUnit)**slot;
    if ((element & 1) == 0)
      *queue++ = (heapUnit**)*slot;
    return true;
  }

  assert(isFixedContentHeap(ptrTarget) == 0); // Don't support other atm
  uint64_t len = getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.
  if (len > queueLeft)
    return false;

  #pragma unroll 4
  for (unsigned i = 1; i < len; ++i) {
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
template <typename... Args>
void relocatePointer(
  heapUnit** slot,
  heapUnit*& newHeapPtr, std::size_t& units_left,
  Heap cleanup_target, Args&... args)
{
  std::size_t maximumBreadth = 1'000,
              queue_1_left = maximumBreadth,
              queue_2_left = maximumBreadth;
  using heapUnitPtrPtr = heapUnit**;
  heapUnitPtrPtr queue_1[maximumBreadth], queue_2[maximumBreadth];
  auto base_1 = queue_1, base_2 = queue_2,
       end_1 = base_1, end_2 = base_2;
  *end_1++ = slot;

  int i = 0;
  do {
    ++i;
    for (; base_1 < end_1; base_1++) {
      // std::cout << std::string(i, ' ') << " " << *base_1 << "("<<(int)determineSource((heapUnit*)*base_1)<<") points to " << **base_1 << " in " << (int)determineSource(**base_1) << std::endl;
      if (!performRelocation(*base_1, newHeapPtr, units_left, cleanup_target, end_2, queue_2_left, args...)) {
        for (; base_1 < end_1; base_1++)
          relocatePointer(*base_1, newHeapPtr, units_left, cleanup_target, args...);
        break;
      }
    }

    base_1 = end_1 = queue_1;
    queue_1_left = maximumBreadth;
    ++i;

    for (; base_2 < end_2; base_2++) {
      // std::cout << std::string(i, ' ') << " " << *base_2 << "("<<(int)determineSource((heapUnit*)*base_2)<<") points to " << **base_2 << " in " << (int)determineSource(**base_2) << std::endl;
      if (!performRelocation(*base_2, newHeapPtr, units_left, cleanup_target, end_1, queue_1_left, args...)) {
        for (; base_2 < end_2; base_2++)
            relocatePointer(*base_2, newHeapPtr, units_left, cleanup_target, args...);
        break;
      }
    }

    base_2 = end_2 = queue_2;
    queue_2_left = maximumBreadth;
  } while (base_1 < end_1);
}

template <typename F>
void walkStack(uint8_t* stackPtr, F slotHandler) {
  intptr_t retAddr = *((intptr_t*)stackPtr);
  stackPtr += sizeof(void*); // step into frame
  frame_info_t* frame = lookup_return_address(table, retAddr);

  while(frame != NULL) {
    std::vector<heapUnit*> lastPointers;
    for(uint16_t i = 0; i < frame->numSlots; i++) {
      pointer_slot_t ptrSlot = frame->slots[i];
      heapUnit** ptr = (heapUnit**)(stackPtr + ptrSlot.offset);
      if(ptrSlot.kind >= 0) {
        auto diff = *ptr - lastPointers.at(ptrSlot.kind);
        assert(diff == 0 && "Derived pointers with non-zero diffs are not supposed to occur!");
      }
      lastPointers.push_back(*ptr);
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
template <typename... Args>
void cleanup_heap(uint8_t* stackPtr, Heap heap, heapUnit*& newheapptr, uint64_t& size_left, Args&... args) {
  walkStack(stackPtr, [&] (heapUnit** ptr) {
    if (((heapUnit)*ptr & 0b11) == 0)
      relocatePointer(ptr, newheapptr, size_left, heap, args...);
  });
}

void SelfRelocatingHeap::cleanup(uint8_t* stackPtr) {
  auto aux_base = allocate(nextSize),
       aux_ptr = aux_base; // this is the bump pointer for relocation

  sizeLeft = size;
  if (isFixedContentHeap(heap)) {
    std::vector<bool> vec(size);
    cleanup_heap(stackPtr, heap, aux_ptr, sizeLeft, base, vec);
  }
  else
    cleanup_heap(stackPtr, heap, aux_ptr, sizeLeft);

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

  int counter_current = ++counter;
    std::cout << "\nCleaning " << (int)heap << "; " << counter_current << "th collection.\n";

#endif // GC_DEBUG_LOG_LVL
  switch(heap) {
    case Heap::Young:
      if (OldHeap.sizeLeft <= 2*smallHeapSize)
        cleanup_heap(stackPtr, Heap::Old);
      assert(OldHeap.sizeLeft > smallHeapSize && "Cannot relocate into insufficient large heap");
      cleanup_heap(stackPtr, Heap::Young, OldHeap.ptr, OldHeap.sizeLeft);

      // Traverse the mutable heap conservatively.
      for (auto ptr = MutableHeap.base; ptr < MutableHeap.ptr; ++ptr)
        if ((*ptr & 0b11) == 0)
          relocatePointer((heapUnit**)ptr, OldHeap.ptr, OldHeap.sizeLeft, Heap::Young);

      heapPtr = heapBase;
      smallSizeLeft = smallHeapSize;
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

      if (OldHeap.sizeLeft < 2 * smallHeapSize) { // if less than two young heaps are left,
        OldHeap.increaseNextSize();
        if (OldHeap.sizeLeft < smallHeapSize)
          OldHeap.cleanup(stackPtr);
      }
    break;

    default:
      assert(false && "Cannot clean invalid heap! Exceeded fixed heap size?");
  }

#if GC_DEBUG_LOG_LVL >= 1
    std::cout << "Finished " << counter_current << "\n\n";
#endif // GC_DEBUG_LOG_LVL
}

extern "C" void cleanup_small_heap(uint8_t* stackPtr) {
  cleanup_heap(stackPtr, Heap::Young);

  // Overwrite the cleaned memory to recognise wrong reads.
#ifdef GC_MEMSET
  memset(heapBase, 0x7F, smallHeapSize * sizeof(heapUnit));
#endif
}
extern "C" void cleanup_old_heap(uint8_t* stackPtr) {
  cleanup_heap(stackPtr, Heap::Old);
}
extern "C" void cleanup_mutable_heap(uint8_t* stackPtr) {
  cleanup_heap(stackPtr, Heap::Mutable);
}

extern "C" [[gnu::naked]] void cleanupSmallHeap()   {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_small_heap");
}
extern "C" [[gnu::naked]] void cleanupLargeHeap()   {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_old_heap");
}
extern "C" [[gnu::naked]] void cleanupMutableHeap()  {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup_mutable_heap");
}
