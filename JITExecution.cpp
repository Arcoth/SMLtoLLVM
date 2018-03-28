#include "Compiler/compile.hpp"
#include "GCPlugin/GCBasicConstants.hpp"
#include "GCPlugin/GC.h"
#include "JITExecution.hpp"

#include "SMLLibrary.hpp"

#include <llvm/IR/Module.h>

#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include <llvm/ExecutionEngine/SectionMemoryManager.h>

#include <chrono>
#include <iostream>
#include <new>

namespace SMLCompiler {

using namespace llvm;

void performOptimisationPasses(Module& mod) {

  legacy::PassManager pm;
  pm.add(createFunctionInliningPass());
  pm.add(createInternalizePass());
  pm.run(mod);

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
  fpm.add(createPromoteMemoryToRegisterPass());
  fpm.add(createTailCallEliminationPass());

  fpm.doInitialization();

  outs() << "Performing optimisation passes\n";
  for (auto& fun : mod)
    fpm.run(fun);
}

void performStatepointsPass(Module& mod) {
  auto& ctx = mod.getContext();

  legacy::FunctionPassManager fpm(&mod);

  // Add a preliminary safepoint poller
  auto safepoint_poll =
    Function::Create(FunctionType::get(Type::getVoidTy(ctx), false),
                     GlobalValue::ExternalLinkage,
                     "gc.safepoint_poll",
                     &mod);

  IRBuilder<> safepoint_builder(BasicBlock::Create(ctx, "entry", safepoint_poll));
  safepoint_builder.CreateRetVoid();

  fpm.add(createPlaceSafepointsPass());
  fpm.doInitialization();

  outs() << "Performing GC passes\n";
  for (auto& fun : mod) {
    fun.setGC("statepoint-example");
    fpm.run(fun);
  }

  legacy::PassManager pm;
  pm.add(createRewriteStatepointsForGCLegacyPass());
  pm.run(mod);
}

class MemManager : public SectionMemoryManager {
  void* _stackmap = nullptr;
  size_t _len = 0;

public:
  auto stackMap() const {return _stackmap;}
  auto len() const {return _len;}

  uint8_t* allocateDataSection(uintptr_t Size, unsigned Alignment, unsigned SectionID, StringRef SectionName,
                               bool readOnly) override {
    auto p = SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID, SectionName, readOnly);
    if (SectionName == ".llvm_stackmaps") {
      _stackmap = p;
      _len = Size;
    }

    return p;
  }
};

Value* insertAllocation(LLVMContext& ctx, IRBuilder<>& builder, Function* fun, Function* collect_fun,
                        Value* heap, Value* left) {
  auto gcCleanup  = BasicBlock::Create(ctx, "gcCleanup" , fun),
       afterCheck = BasicBlock::Create(ctx, "afterCheck", fun);

  // Round the size argument up: we allocate in 8 byte chunks.
  auto size_arg = builder.CreateUDiv(builder.CreateAdd(fun->arg_begin(),
                                                       ConstantInt::get(fun->arg_begin()->getType(), 7)),
                                     ConstantInt::get(fun->arg_begin()->getType(), 8));

  builder.CreateCondBr(builder.CreateICmpULT(size_arg, builder.CreateLoad(left, "size_left"), "checksize"),
                       afterCheck, gcCleanup);

  builder.SetInsertPoint(gcCleanup);
  builder.CreateCall(collect_fun);
  builder.CreateBr(afterCheck);

  builder.SetInsertPoint(afterCheck);
  auto retval = builder.CreateLoad(heap, "alloc_slot");
  builder.CreateStore(builder.CreateGEP(retval, size_arg, "new_heap_ptr"),
                      heap);
  builder.CreateStore(builder.CreateSub(builder.CreateLoad(left, "updated_size_left"), size_arg, "left_after_alloc"),
                      left);
  return builder.CreatePointerCast(retval, fun->getReturnType(), "storage_pointer");
}

/* Constructs the allocation function, and introduces the weak symbols corresponding to the
   heap base and size scalars linked in by the runtime. */
void addGCSymbols(Module& mod) {
  auto& ctx = mod.getContext();
  auto global = [&] (auto type, auto name) {
    return new GlobalVariable(mod, type,
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     name,
                     nullptr, // no predecessor
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true);
  };

  auto heap_unit = Type::getInt64Ty(ctx);
  auto ptr_type = heap_unit->getPointerTo(heapAddressSpace);

  auto heap = global(ptr_type, "heapPtr");
  auto left = global(heap_unit, "smallSizeLeft");

  auto mut_heap = global(ptr_type, "referenceHeapPtr");
  auto mut_left = global(heap_unit, "referenceSizeLeft");

  auto large_heap = global(ptr_type, "largeHeapPtr");
  auto large_left = global(heap_unit, "largeSizeLeft");


  auto collect_small = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   invokeSmallHeapCollection, &mod);
  auto collect_large = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   invokeLargeHeapCollection, &mod);
  auto collect_mut = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   invokeMutableHeapCollection, &mod);

  auto type = FunctionType::get(genericPointerType(ctx),
                                {Type::getInt64Ty(ctx)}, false);

  auto small_fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   smallAllocFun, &mod),
       large_fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   largeAllocFun, &mod),
       mutable_fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   mutableAllocFun, &mod);

  {
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", small_fun));
    builder.CreateRet(insertAllocation(ctx, builder, small_fun, collect_small, heap, left));
  }
  {
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", large_fun));
    builder.CreateRet(insertAllocation(ctx, builder, large_fun, collect_large, large_heap, large_left));
  }
  {
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", mutable_fun));
    builder.CreateRet(insertAllocation(ctx, builder, mutable_fun, collect_mut, mut_heap, mut_left));
  }
}

int execute(std::size_t functionIndex, SMLTranslationUnit& unit) {
  InitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  auto memManagerUptr = std::make_unique<MemManager>();
  auto memManager = memManagerUptr.get();
  std::string err_str;
  auto module = unit.module;
  auto EE = EngineBuilder{std::unique_ptr<Module>(unit.module)}
              .setErrorStr(&err_str)
              .setMemoryManager(move(memManagerUptr))
              .create();
  if (!EE) {
    errs() << "Could not create execution engine: " << err_str;
    return EXIT_FAILURE;
  }

  EE->addGlobalMapping(module->getFunction("cleanupSmallHeap"), (void*)&cleanupSmallHeap);
  EE->addGlobalMapping(module->getFunction("cleanupMutableHeap"), (void*)&cleanupMutableHeap);
  EE->addGlobalMapping(module->getFunction("cleanupLargeHeap"), (void*)&cleanupLargeHeap);
  EE->addGlobalMapping(module->getGlobalVariable("referenceHeapPtr"), &referenceHeapPtr);
  EE->addGlobalMapping(module->getGlobalVariable("referenceSizeLeft"), &referenceSizeLeft);
  EE->addGlobalMapping(module->getGlobalVariable("heapPtr"), &heapPtr);
  EE->addGlobalMapping(module->getGlobalVariable("smallSizeLeft"), &smallSizeLeft);
  EE->addGlobalMapping(module->getGlobalVariable("largeHeapPtr"), &largeHeapPtr);
  EE->addGlobalMapping(module->getGlobalVariable("largeSizeLeft"), &largeSizeLeft);

  EE->generateCodeForModule(module);

  assert(memManager->stackMap());
  StackMapPtr = memManager->stackMap();

  for (auto [fun, len] : unit.closureLength)
    closureLengths[(void*)EE->getPointerToFunction(fun)] = len;

  std::vector<genericPointerTypeNative> imports{0}; // record tag takes one spot
  for (auto& [pid, indices] : unit.importTree)
    try {
      auto closure = LibraryImpl::libraryIdMap.at(pid.c_str()).at(indices);
      imports.push_back(closure);
    }
    catch (std::out_of_range const&) {
      throw CompileFailException{"Unresolved import: " + std::string{pid} + " " + std::to_string(indices[0])};
    }

  //! Start measuring at the GC initialisation.
  init();

  // Get the structured record
  auto exportFunction = (genericPointerTypeNative(*)(genericPointerTypeNative))EE->getFunctionAddress("export");
  auto lambda = (genericPointerTypeNative*)exportFunction((genericPointerTypeNative)imports.data());

  // Invoke the function with the given index. Indexing starts at 1.
  auto last_fnc = (genericPointerTypeNative*)lambda[functionIndex+1];

  auto start_time = std::chrono::high_resolution_clock::now();
  genericIntTypeNative arg = 1;
  auto res = (genericIntTypeNative)((genericFunctionTypeNative*) last_fnc[0])
               ((genericPointerTypeNative)((arg << GC::valueFlagLength) + GC::intTag), last_fnc);

  res &= ~GC::floatTag;
  double double_res;
  std::memcpy(&double_res, &res, sizeof res);
  res >>= 1;

  auto result_time = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = result_time-start_time;

  std::cout << "\nTime: " << (int)(elapsed_seconds.count()*1000) << "ms\n"
            << "Result: " << double_res << "(double), " << res << "(int)\n";

  return EXIT_SUCCESS;
}

}
