#include <cstddef>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <cinttypes>

#include <iostream>
#include <vector>

#include "llvm-statepoint-utils/dist/llvm-statepoint-tablegen.h"

#define PRINT_STUFF 1

void* StackMapPtr;
statepoint_table_t* table;

uint64_t heapSize = 256;
uint32_t* heapBase;

uint32_t* heapPtr = heapBase; // points at the first free spot in the heap
uint32_t* auxHeap;

extern "C" void init() {
  heapBase = heapPtr = (uint32_t*)malloc(heapSize);
  auxHeap = (uint32_t*)malloc(heapSize);

  table = generate_table(StackMapPtr, 0.5);

  std::cout << "\n\nHeap size: " << heapSize << '\n'
            << "Printing stackmap hash table:\n";
  print_table(stdout, table, true);
  std::cout << "Initialised GC!\n";
}

uint32_t* relocate_uint32star(uint32_t** slot, uint32_t* heapPtr) {
  uint32_t val = **slot;
  *heapPtr = val;
  *slot = heapPtr;
  return heapPtr + 1;
}

extern "C" void __attribute__((naked)) _enterGC() {
  asm("mov %rsp, %rdi\n"
      "jmp cleanup");
}

extern "C" void cleanup(uint8_t* stackPtr) {
  std::cout << "Stack pointer: " << (void*)stackPtr << '\n';

  intptr_t retAddr = *((intptr_t*)stackPtr);
  stackPtr += sizeof(void*); // step into frame
  frame_info_t* frame = lookup_return_address(table, retAddr);

  // we'll be moving live stuff to the current aux heap
  uint32_t *newBase = auxHeap,
           *newHeapPtr = auxHeap;

#ifdef PRINT_STUFF
  printf("\n\n--- starting to scan the stack for gc ---\n");
  printf("frame return address: 0x%" PRIX64 "\n", retAddr);
#endif

  while(frame != NULL) {
    uint16_t i = 0;
    for(; i < frame->numSlots; i++) {
      pointer_slot_t ptrSlot = frame->slots[i];
      if(ptrSlot.kind >= 0) {
        // we do not use derived pointers
        assert(false && "unexpected derived pointer\n");
      }

      uint32_t** ptr = (uint32_t**)(stackPtr + ptrSlot.offset);
      // Check whether this is an unboxed integer...
      if (((intptr_t)ptr & 1) == 0)
        newHeapPtr = relocate_uint32star(ptr, newHeapPtr);
    }

#ifdef PRINT_STUFF
    printf("\trelocated %" PRIu16 " pointer(s).\n", i);
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

#ifdef PRINT_STUFF
  printf("Reached the end of the stack.\n\n");
#endif

  // swap spaces
  auxHeap = heapBase;
  heapBase = newBase;
  heapPtr = newHeapPtr;

  // overwrite old space with 1's to
  // cause weird results if something's wrong.
  memset(auxHeap, 0x7F, heapSize);
}
