#include <llvm/IR/Module.h>

namespace SMLCompiler {

  // Perform optimisation and GC passes.
  void performPasses(llvm::Module&);

  inline char const* const gcCleanupFunctionName = "_enterGC";

  // Add the entities required for interaction with the GC runtime.
  void addGCSymbols(llvm::Module& mod);

  // Execute the first function in the module with some test parameter.
  int execute(llvm::Module* mod);
}
