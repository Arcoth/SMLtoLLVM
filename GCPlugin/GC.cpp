#include <cstddef>
#include <cstdlib>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <cassert>

#include <iostream>
#include <vector>

void* StackMapPtr;

/* Header {
  uint8  : Stack Map Version (current version is 3)
  uint8  : Reserved (expected to be 0)
  uint16 : Reserved (expected to be 0)
}
uint32 : NumFunctions
uint32 : NumConstants
uint32 : NumRecords
StkSizeRecord[NumFunctions] {
  uint64 : Function Address
  uint64 : Stack Size
  uint64 : Record Count
}
Constants[NumConstants] {
  uint64 : LargeConstant
}
StkMapRecord[NumRecords] {
  uint64 : PatchPoint ID
  uint32 : Instruction Offset
  uint16 : Reserved (record flags)
  uint16 : NumLocations
  Location[NumLocations] {
    uint8  : Register | Direct | Indirect | Constant | ConstantIndex
    uint8  : Reserved (expected to be 0)
    uint16 : Location Size
    uint16 : Dwarf RegNum
    uint16 : Reserved (expected to be 0)
    int32  : Offset or SmallConstant
  }
  uint32 : Padding (only if required to align to 8 byte)
  uint16 : Padding
  uint16 : NumLiveOuts
  LiveOuts[NumLiveOuts]
    uint16 : Dwarf RegNum
    uint8  : Reserved
    uint8  : Size in Bytes
  }
  uint32 : Padding (only if required to align to 8 byte)
}
 */

struct StackMapHeader {
  uint8_t version;
  uint8_t : 8; uint16_t : 16;
  uint32_t numFunctions;
  uint32_t numConstants;
  uint32_t numRecords;
};
struct StkSizeRecord {
  uint64_t funcAddress;
  uint64_t stackSize;
  uint64_t recordCount;
};
struct Constants {
  uint64_t largeConstant;
};

struct StkMapRecordHeader {
  uint64_t patchpointID;
  uint32_t instructionOffset;
  uint16_t : 16;
  uint16_t numLocations;
};
struct StkMapRecordLocation {
  enum : uint8_t {Register = 1, Direct, Indirect, Constant, ConstantIndex} type;
  uint8_t : 8;
  uint16_t locationSize;
  uint16_t DwarfRegNum;
  uint16_t : 16;
  int32_t offset; // or SmallConstant
};
struct StkMapNumLiveOuts {
  uint16_t : 16;
  uint16_t numLiveOuts;
};
struct StkMapRecordLiveOuts {
  uint16_t DwarfRegNum;
  uint8_t : 8;
  uint8_t size;
};

struct StkMapRecord {
  StkMapRecordHeader header;
  std::vector<StkMapRecordLocation> locations;
  StkMapNumLiveOuts numLiveOuts;
  std::vector<StkMapRecordLiveOuts> liveOuts;
};

struct StackMap {
  StackMapHeader header;
  std::vector<StkSizeRecord> sizeRecords;
  std::vector<Constants> constants;
  std::vector<StkMapRecord> records;
};

std::ostream& operator<<(std::ostream& os, StackMap const& map) {
  return os << "Version: " << (int)map.header.version << '\n'
            << "Functions: " << map.header.numFunctions << '\n'
            << "Records: " << map.header.numRecords << '\n';
}

template <typename T>
uint8_t* parseInto(T& dest, uint8_t* src) {
  std::memcpy(&dest, src, sizeof(T));
  return src + sizeof(T);
}

template <typename T>
uint8_t* parseArrayInto(std::vector<T>& vec, uint8_t* src, size_t num) {
  for (size_t i = 0; i < num; ++i) {
    T rec;
    src = parseInto(rec, src);
    vec.push_back(rec);
  }
  return src;
}

StackMap parse(uint8_t* stackmap) {
  StackMap map;
  stackmap = parseInto(map.header, stackmap);
  stackmap = parseArrayInto(map.sizeRecords, stackmap, map.header.numFunctions);
  stackmap = parseArrayInto(map.constants, stackmap, map.header.numConstants);
  auto alignTo8 = [&stackmap] {
    if ((uintptr_t)stackmap % 8 != 0)
      stackmap += 4;
    assert((uintptr_t)stackmap % 8 == 0);
  };
  for (size_t i = 0; i < map.header.numRecords; ++i) {
    StkMapRecord rec;
    stackmap = parseInto(rec.header, stackmap);
    stackmap = parseArrayInto(rec.locations, stackmap, rec.header.numLocations);
    alignTo8();
    stackmap = parseInto(rec.numLiveOuts, stackmap);
    stackmap = parseArrayInto(rec.liveOuts, stackmap, rec.numLiveOuts.numLiveOuts);
    alignTo8();
    map.records.push_back(rec);
  }
  return map;
}

StackMap stackMap;
bool mapInitialised;

void* allocate(std::size_t i){
  if (!mapInitialised) {
    stackMap = parse((uint8_t*)StackMapPtr);
    std::cout << stackMap;
    mapInitialised = true;
  }
  return std::malloc(i);
}
