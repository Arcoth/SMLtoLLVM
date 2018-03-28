#include "GCBasicConstants.hpp"

#include <unordered_map>

using heapUnit = uint64_t;

extern void* StackMapPtr;
extern std::unordered_map<void*, std::size_t> closureLengths;
extern "C" void init();

extern "C" [[gnu::naked]] void cleanupSmallHeap();
extern "C" [[gnu::naked]] void cleanupLargeHeap();
extern "C" [[gnu::naked]] void cleanupMutableHeap();

extern heapUnit *referenceHeapPtr;
extern uint64_t referenceSizeLeft;

extern uint64_t smallSizeLeft;
extern heapUnit *heapPtr;

extern uint64_t largeSizeLeft;
extern heapUnit *largeHeapPtr;
