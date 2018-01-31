#include "Compiler/compile.hpp"
#include "REPL.h"

#include <llvm/IR/Module.h>

#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <llvm/ExecutionEngine/SectionMemoryManager.h>

#include <iostream>
#include <new>

namespace SMLCompiler {

using namespace llvm;

void performPasses(Module& mod) {
  auto& ctx = mod.getContext();

  // Create a new pass manager attached to it.
  legacy::FunctionPassManager fpm(&mod);

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  fpm.add(createInstructionCombiningPass());
  // Reassociate expressions.
  fpm.add(createReassociatePass());
  // Eliminate Common SubExpressions.
  fpm.add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  fpm.add(createCFGSimplificationPass());

  // Add a preliminary safepoint poller
  auto safepoint_poll =
    Function::Create(FunctionType::get(Type::getVoidTy(ctx), false),
                     GlobalValue::ExternalLinkage,
                     "gc.safepoint_poll",
                     &mod);

  IRBuilder<> safepoint_builder(BasicBlock::Create(ctx, "entry", safepoint_poll));
  safepoint_builder.CreateRetVoid();

  // Statepoints passes
  fpm.add(createPlaceSafepointsPass());

  fpm.doInitialization();

  for (auto& fun : mod) {
    if (fun.getName() == "gc.safepoint_poll")
      continue;
    fun.setGC("statepoint-example");
    outs() << "Performing optimisation and GC passes on " << fun.getName() << '\n';
    fpm.run(fun);
  }

  legacy::PassManager pm;
  pm.add(createRewriteStatepointsForGCPass());
  pm.run(mod);
}

class MemManager : public SectionMemoryManager {
  void* _stackmap;

public:
  auto stackMap() const {return _stackmap;}

  uint8_t* allocateDataSection(uintptr_t Size, unsigned Alignment, unsigned SectionID, StringRef SectionName,
                               bool readOnly) override {
    outs() << "Allocating " << Size << " bytes for " << SectionName << '\n';
    auto p = SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID, SectionName, readOnly);
    if (SectionName == ".llvm_stackmaps")
      _stackmap = p;
    return p;
  }
};

int execute(Module* mod) {
  InitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  auto memManagerUptr = std::make_unique<MemManager>();
  auto memManager = memManagerUptr.get();
  std::string err_str;
  auto EE = EngineBuilder{std::unique_ptr<Module>{mod}}
              .setErrorStr(&err_str)
              .setMemoryManager(move(memManagerUptr))
              .create();
  if (!EE) {
    errs() << "Could not create execution engine: " << err_str;
    return EXIT_FAILURE;
  }

//  for (;;) {
//    std::cout << " >";
//    std::string str;
//    getline(std::cin, str);
//    if (str == "exit")
//      return EXIT_SUCCESS;
//    out << str << std::endl;
//    auto exp = parseVerbosePlambda(in);
//
//
//

  sys::DynamicLibrary gclib = sys::DynamicLibrary::getPermanentLibrary("SMLtoLLVM/libSML_GC.so", &err_str);
  if (!gclib.isValid()){
    errs() << "Could not load GC library: " << err_str;
    return EXIT_FAILURE;
  }
  auto sym = gclib.getAddressOfSymbol("_Z8allocatem");
  if (!sym) {
    errs() << "Couldn't find allocator!";
    return EXIT_FAILURE;
  }

  EE->generateCodeForModule(mod);
  assert(memManager->stackMap());
  *(void**)gclib.getAddressOfSymbol("StackMapPtr") = memManager->stackMap();

  auto fce_ptr = (genericPointerTypeNative*(*)())EE->getFunctionAddress("export");
  auto lambda = fce_ptr();

  auto last_fnc = (genericPointerTypeNative*)lambda[0];
  auto res = ((genericFunctionTypeNative*) last_fnc[0])
               ((genericPointerTypeNative)5, last_fnc+1);

  outs() << "Result: " << (intptr_t)res << '\n';

  return EXIT_SUCCESS;
}

}
