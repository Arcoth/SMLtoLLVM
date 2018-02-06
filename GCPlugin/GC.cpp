#include <cstddef>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <cinttypes>

#include <iostream>
#include <algorithm>
#include <vector>

#include <unordered_map>

#include "llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h"

#define PRINT_STUFF 1

void* StackMapPtr;
statepoint_table_t* table;

using heapUnit = uint64_t;

uint64_t heapSize = 24;
heapUnit* heapBase;

heapUnit* heapPtr; // points at the first free spot in the heap
heapUnit* auxHeap;

std::unordered_map<void*   , std::size_t> const* closureLengths;

extern "C" void init() {
  heapBase = heapPtr = (heapUnit*)malloc(heapSize * sizeof(heapUnit));
  std::cout << "Initialised GC!\n";
}

heapUnit* relocate(heapUnit** slot, heapUnit* newHeapPtr) {
  const uint64_t tag = **slot;

  std::cout << "relocating an object of tag " << std::hex << tag << ", ";

  uint64_t len; // This is the total number of slots of size 64 bytes occupied.

  switch (tag & 0b11) {
  case 0:
    len = closureLengths->at((void*)tag);
    std::cout << "relocating a closure ";
    break;
  case 0b01:
    len = (tag & UINT32_MAX) >> 1;
    std::cout << "relocating a record ";
    break;
  case 0b11:
    std::cout << "revisit during relocation.";
    *slot = (heapUnit*)(tag & ~(heapUnit)0b11);
    return newHeapPtr;
  default: assert(0);
  }
  std::cout << " of length " << len << '\n';


  assert(len < heapSize-(newHeapPtr-auxHeap));
  std::copy_n(*slot, len, newHeapPtr);
  **slot = (heapUnit)newHeapPtr | 0b11;
  *slot = newHeapPtr;
  newHeapPtr += len;

  auto is_in = [] (auto p1, auto x, auto p2) {return p1 <= x && x < p2;};
  for (int i = 1; i < len; ++i) {
    auto element = (*slot)[i];
    if (element & 1)
      continue;
    auto elem_as_ptr = (heapUnit**)*slot + i;
    // verify these pointers point into the old heap
    assert(is_in(heapBase, *elem_as_ptr, heapBase + heapSize));
    newHeapPtr = relocate(elem_as_ptr, newHeapPtr);
  }

  return newHeapPtr;
}

extern "C" void __attribute__((naked)) _enterGC() {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup");
}

extern "C" void cleanup(uint8_t* stackPtr) {
  if (!table) {
    table = generate_table(StackMapPtr, 0.5);

    std::cout << "\n\nHeap size: " << heapSize << '\n'
              << "Printing stackmap hash table:\n";
    print_table(stdout, table, true);
    auxHeap = (heapUnit*)malloc(heapSize * sizeof(heapUnit));
  }

  std::cout << "Stack pointer: " << (void*)stackPtr << '\n'
            << "Heap base: " << heapBase << " heap: " << heapPtr << " end of heap: " << heapBase + heapSize << '\n';

  intptr_t retAddr = *((intptr_t*)stackPtr);
  stackPtr += sizeof(void*); // step into frame
  frame_info_t* frame = lookup_return_address(table, retAddr);

  // we'll be moving live stuff to the current aux heap
  auto *newBase = auxHeap,
       *newHeapPtr = auxHeap;

#ifdef PRINT_STUFF
  printf("\n\n--- starting to scan the stack for gc ---\n");
  printf("frame return address: 0x%" PRIX64 "\n", retAddr);
#endif

  while(frame != NULL) {
    uint16_t counter = 0;
    for(uint16_t i = 0; i < frame->numSlots; i++) {
      pointer_slot_t ptrSlot = frame->slots[i];
      if(ptrSlot.kind >= 0) {
        // we do not use derived pointers
        assert(false && "unexpected derived pointer\n");
      }

      heapUnit** ptr = (heapUnit**)(stackPtr + ptrSlot.offset);
      // check whether this is a heap pointer or an unboxed integer:
      if (((intptr_t)*ptr & 0b11) == 0) {
        newHeapPtr = relocate(ptr, newHeapPtr);
        counter++;
      }
    }

#ifdef PRINT_STUFF
    printf("\trelocated %" PRIu16 " pointer(s).\n", counter);
#endif

    // move to next frame. seems we have to add one pointer size to
    // reach the next return address? NOTE
    stackPtr += frame->frameSize;

    // grab return address of the frame
    retAddr = *((intptr_t*)stackPtr);
    stackPtr += sizeof(void*); // step into frame
    frame = lookup_return_address(table, retAddr);

#ifdef PRINT_STUFF
    printf("frame return address: 0x%" PRIX64 "\n", retAddr);
#endif
  }
//
//  if (newHeapPtr - newBase < 24)
//    abort();

#ifdef PRINT_STUFF
  printf("Reached the end of the stack.\n\n");
#endif

  // swap spaces
  auxHeap = heapBase;
  heapBase = newBase;
  heapPtr = newHeapPtr;

  // overwrite old space with 1's to
  // cause weird results if something's wrong.
  memset(auxHeap, 0x7F, heapSize * sizeof(heapUnit));
}
