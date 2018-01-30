#include <llvm/IR/Module.h>

namespace SMLCompiler {

  // Perform optimisation and GC passes.
  void performPasses(llvm::Module&);

  // Execute the first function in the module with some test parameter.
  int execute(llvm::Module* mod);
}
