#include <llvm/IR/Module.h>

namespace SMLCompiler {

  // Perform optimisation and GC passes.
  void performOptimisationPasses(llvm::Module&);
  void performStatepointsPass(llvm::Module&);

  inline char const* const invokeSmallHeapCollection = "cleanupSmallHeap";
  inline char const* const invokeLargeHeapCollection = "cleanupLargeHeap";
  inline char const* const invokeMutableHeapCollection = "cleanupMutableHeap";

  // Add the entities required for interaction with the GC runtime.
  void addGCSymbols(llvm::Module& mod);

  // Execute the function at index funIndex with some test parameter.
  int execute(std::size_t funIndex, SMLTranslationUnit& unit);
}
