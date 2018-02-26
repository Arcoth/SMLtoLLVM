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

// #define GC_MEMSET
#define GC_DEBUG_LOG_LVL 0

using namespace SMLCompiler::GC;

void* StackMapPtr;
statepoint_table_t* table;

using heapUnit = uint64_t;

const uint64_t heapSize = 8000; // 8KB
uint64_t sizeLeft = heapSize;
heapUnit *heapBase,
         *heapPtr; // points at the first free spot in the heap

uint64_t largeHeapSize = 500'000'000;
uint64_t largeSizeLeft = largeHeapSize;
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
  auxLargeHeap = (heapUnit*)malloc(largeHeapSize * sizeof(heapUnit));
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
             in_large_heap = is_in(Heap::Old, ptr);
  const bool in_aux_large = is_in(auxLargeHeap, largeHeapSize, ptr);
  const bool in_aux_ref = is_in(auxReferenceHeap, referenceHeapSize, ptr);
  // assert(in_small_heap || in_mut_heap || in_large_heap || in_aux_ref || in_aux_large);
  return in_small_heap? Heap::Young
       : in_large_heap || in_aux_large? Heap::Old
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

__attribute__((hot))
void relocate(uint8_t* const stackPtr, heapUnit** slot,
              heapUnit*& newHeapPtr, std::size_t& units_left,
              Heap cleanup_target, Heap reloc_target
#if GC_DEBUG_LOG_LVL >= 2
              , std::string indent = ""
#endif // GC_DEBUG_LOG_LVL
              )
{
  if (*slot == nullptr) // e.g. empty records, untagged data constructors, etc.
    return;

  auto origin = determineSource(*slot);

  if (!canPointInto(origin, cleanup_target)) {
#if GC_DEBUG_LOG_LVL >= 2
    std::cout << indent << "Aborting: pointer to " << (int)origin << " isn't considered\n";
#endif // GC_DEBUG_LOG_LVL
    return;
  }

  const uint64_t tag = **slot;

  uint64_t len = isSingleUnitHeap(origin)? 1 : getRecordLength(tag); // This is the total number of slots of size 64 bytes occupied.

#if GC_DEBUG_LOG_LVL >= 2
  std::cout << indent << "relocating " << slot << " from " << (int)determineSource((heapUnit*)slot) << " into " << (int)origin << " of tag/len " << std::hex << tag << "/" << len << std::endl;
#endif // GC_DEBUG_LOG_LVL

  if (len == 0) { // we're revisiting an already relocated record...
    *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
#if GC_DEBUG_LOG_LVL >= 2
    std::cout << indent << "Assigning relocated " << *slot << std::endl;
#endif // GC_DEBUG_LOG_LVL
    return;
  }

  if (origin == cleanup_target) {
    if (len > units_left) // target heap ful?
      cleanup_heap(stackPtr, reloc_target);
    assert(len <= units_left); // we can fit this into the target heap?
    std::copy_n(*slot, len, newHeapPtr); // perform the relocation
    **slot = (heapUnit)newHeapPtr | 0b11; // assign the old spot a relocation forward reference to the target
    *slot = newHeapPtr; // replace old pointer value
#if GC_DEBUG_LOG_LVL >= 2
    std::cout << indent << "Assigning " << *slot << std::endl;
#endif // GC_DEBUG_LOG_LVL
    newHeapPtr += len; // bump heap pointer
    units_left -= len;
  }

  //! Most records are up to four elements in length.
  #pragma unroll 4
  for (int i = isSingleUnitHeap(origin)? 0 : 1; i < len; ++i) {
    auto ptr_to_elem = (heapUnit**)*slot + i;
    auto element = (heapUnit)*ptr_to_elem;
#if GC_DEBUG_LOG_LVL >= 2
    std::cout << indent << "\tElem " << i << " of " << *slot << " is " << std::hex << element << ';' << std::endl;
#endif // GC_DEBUG_LOG_LVL
    if (element & 0b11) // not a pointer
      continue;
    if (ptr_to_elem != slot)
      relocate(stackPtr, ptr_to_elem, newHeapPtr, units_left, cleanup_target, reloc_target
#if GC_DEBUG_LOG_LVL >= 2
      , indent + "\t"
#endif // GC_DEBUG_LOG_LVL
      );
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
  const auto originalStackPtr = stackPtr;
  if (!table)
    table = generate_table(StackMapPtr, 0.5);

  walkStack(stackPtr, [&] (heapUnit** ptr) {
    if (((intptr_t)*ptr & 0b11) == 0)
      relocate(originalStackPtr, ptr, newheapptr, size_left, heap, target);
  });
}

// cleanup the small heap
void cleanup_heap(uint8_t* stackPtr, Heap heap) {
#if GC_DEBUG_LOG_LVL >= 1
  static int counter;
  std::cout << "\nCleaning " << (int)heap << "; " << ++counter << "th collection.\n";
#endif // GC_DEBUG_LOG_LVL
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
