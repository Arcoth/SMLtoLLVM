#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/Support/Compiler.h"

using namespace llvm;

namespace {
  class LLVM_LIBRARY_VISIBILITY MyGC : public GCStrategy {
  public:
    MyGC() {}
  };

  GCRegistry::Add<MyGC>
  X("mygc", "My bespoke garbage collector.");
}
