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

  // we're not concurrent
  // fpm.add(createPlaceSafepointsPass());

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

/* Constructs the allocation function, and introduces the weak symbols corresponding to the
   heap base and size scalars linked in by the runtime. */
void addGCSymbols(Module& mod) {
  auto& ctx = mod.getContext();
  auto base = new GlobalVariable(mod, Type::getInt32PtrTy(ctx),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "heapPtr",
                     nullptr, // no predecessor
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised
  auto heap = new GlobalVariable(mod, Type::getInt32PtrTy(ctx),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "heapBase",
                     base,
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised
  auto size = new GlobalVariable(mod, Type::getInt64Ty(ctx),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "heapSize",
                     heap,
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised

  Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   "_enterGC", &mod);

  auto type = FunctionType::get(Type::getVoidTy(ctx)->getPointerTo(heapAddressSpace),
                                {Type::getInt64Ty(ctx)}, false);
  Function* fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   allocationFunctionName, &mod);
  auto size_arg = fun->arg_begin();

  auto afterCheck = BasicBlock::Create(ctx, "afterCheck", fun);
  IRBuilder<> after_builder(afterCheck);
  auto retval = after_builder.CreateLoad(heap, "alloc_slot");
  after_builder.CreateStore(after_builder.CreateGEP(after_builder.CreateLoad(heap, "heapptr"),
                                                    size_arg, "newheapptr"),
                            heap);
  after_builder.CreateRet(after_builder.CreatePointerCast(retval, fun->getReturnType()));

  auto gcCleanup = BasicBlock::Create(ctx, "gcCleanup", fun, afterCheck);
  IRBuilder<> cleanup_builder(gcCleanup);
  cleanup_builder.CreateCall(mod.getOrInsertFunction(gcCleanupFunctionName, Type::getVoidTy(ctx)));
  cleanup_builder.CreateBr(afterCheck);

  auto entry = BasicBlock::Create(ctx, "entry", fun, gcCleanup);
  IRBuilder<> builder(entry);

  auto storage_used = builder.CreatePtrDiff(builder.CreateLoad(heap, "heapptr"),
                                            builder.CreateLoad(base, "baseptr"), "spaceused");
  auto storage_left = builder.CreateSub(builder.CreateLoad(size, "heapsize"),
                                        storage_used, "spaceleft");
  builder.CreateCondBr(builder.CreateICmpULT(size_arg, storage_left, "checksize"),
                       afterCheck, gcCleanup);
}

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

  sys::DynamicLibrary gclib = sys::DynamicLibrary::getPermanentLibrary("SMLtoLLVM/libSML_GC.so", &err_str);
  if (!gclib.isValid()){
    errs() << "Could not load GC library: " << err_str;
    return EXIT_FAILURE;
  }

  EE->generateCodeForModule(mod);
  assert(memManager->stackMap());
  *(void**)gclib.getAddressOfSymbol("StackMapPtr") = memManager->stackMap();

  // Invoke the GC initialisation (allocate and set up the heap pointers)
  auto gc_init = (void(*)())gclib.getAddressOfSymbol("init");
  if (!gc_init){
    errs() << "Could not find GC initialisation function!";
    return EXIT_FAILURE;
  }
  gc_init();

  // Get the structured record
  auto fce_ptr = (genericPointerTypeNative*(*)())EE->getFunctionAddress("export");
  auto lambda = fce_ptr();

  // Invoke the function with the lexicographically least name
  auto last_fnc = (genericPointerTypeNative*)lambda[0];
  auto res = ((genericFunctionTypeNative*) last_fnc[0])
               ((genericPointerTypeNative)((5 << 1) + 1), last_fnc+1);

  outs() << "Result: " << (intptr_t)res << '\n';

  return EXIT_SUCCESS;
}

}
