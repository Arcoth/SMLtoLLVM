#include <llvm/IR/Module.h>

namespace SMLCompiler {

  // Perform optimisation and GC passes.
  void performOptimisationPasses(llvm::Module&);
  void performStatepointsPass(llvm::Module&);

  inline char const* const invokeSmallHeapCollection = "cleanupSmallHeap";
  inline char const* const invokeMutableHeapCollection = "cleanupMutableHeap";

  // Add the entities required for interaction with the GC runtime.
  void addGCSymbols(llvm::Module& mod);

  // Execute the first function in the module with some test parameter.
  // The pointer and length parameter designate the map that assigns each function its closures' length.
  int execute(llvm::Module* mod, std::size_t funIndex,
              void const* closureLengths, std::size_t rangeSize);
}
