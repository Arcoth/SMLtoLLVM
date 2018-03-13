#include "Compiler/compile.hpp"
#include "GCPlugin/GCBasicConstants.hpp"
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

  for (auto& fun : mod) {
    outs() << "Performing optimisation passes on " << fun.getName() << '\n';
    fpm.run(fun);
  }

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

  for (auto& fun : mod) {
    fun.setGC("statepoint-example");
    outs() << "Performing GC pass on " << fun.getName() << '\n';
    fpm.run(fun);
  }

  legacy::PassManager pm;
  pm.add(createRewriteStatepointsForGCPass());
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
    outs() << "Allocating " << Size << " bytes for " << SectionName << '\n';
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
  builder.CreateStore(builder.CreateGEP(builder.CreateLoad(heap, "heapptr"),
                                        size_arg, "newheapptr"),
                      heap);
  builder.CreateStore(builder.CreateSub(builder.CreateLoad(left, "new_size_left"), size_arg, "left_after_alloc"),
                      left);
  return builder.CreatePointerCast(retval, fun->getReturnType());
}

/* Constructs the allocation function, and introduces the weak symbols corresponding to the
   heap base and size scalars linked in by the runtime. */
void addGCSymbols(Module& mod) {
  auto& ctx = mod.getContext();
  auto variable_type = Type::getInt64Ty(ctx)->getPointerTo(heapAddressSpace);
  auto heap = new GlobalVariable(mod, Type::getInt64Ty(ctx)->getPointerTo(heapAddressSpace),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "heapPtr",
                     nullptr, // no predecessor
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised
  auto left = new GlobalVariable(mod, Type::getInt64Ty(ctx),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "sizeLeft",
                     heap,
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised

  auto mut_heap = new GlobalVariable(mod, variable_type,
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "referenceHeapPtr",
                     nullptr,
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised
  auto mut_left = new GlobalVariable(mod, Type::getInt64Ty(ctx),
                     false, // constant?
                     GlobalVariable::ExternalLinkage,
                     nullptr, // no initialiser
                     "referenceSizeLeft",
                     mut_heap,
                     GlobalVariable::NotThreadLocal,
                     0, // not to be treated as a GC root!
                     true); // externally initialised

  auto collect = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   invokeSmallHeapCollection, &mod);
  auto collect_mut = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalVariable::ExternalLinkage,
                   invokeMutableHeapCollection, &mod);

  auto type = FunctionType::get(genericPointerType(ctx),
                                {Type::getInt64Ty(ctx)}, false);

  auto immutable_fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   immutableAllocFun, &mod),
       mutable_fun = Function::Create(type, GlobalVariable::InternalLinkage,
                                   mutableAllocFun, &mod);

  {
    IRBuilder<> builder(BasicBlock::Create(ctx, "entry", immutable_fun));
    builder.CreateRet(insertAllocation(ctx, builder, immutable_fun, collect, heap, left));
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

  sys::DynamicLibrary gclib = sys::DynamicLibrary::getPermanentLibrary("SMLtoLLVM/libSML_GC.so", &err_str);
  if (!gclib.isValid()){
    errs() << "Could not load GC library: " << err_str;
    return EXIT_FAILURE;
  }

  EE->generateCodeForModule(module);

  assert(memManager->stackMap());
  *(void**)gclib.getAddressOfSymbol("StackMapPtr") = memManager->stackMap();

  std::unordered_map<void*, std::size_t> closureLengths;
  for (auto [fun, len] : unit.closureLength)
    closureLengths[(void*)EE->getPointerToFunction(fun)] = len;
  *(void**)gclib.getAddressOfSymbol("closureLengths") = &closureLengths;

  // Invoke the GC initialisation (allocate and set up the heap pointers)
  auto gc_init = (void(*)())gclib.getAddressOfSymbol("init");
  if (!gc_init){
    errs() << "Could not find GC initialisation function!";
    return EXIT_FAILURE;
  }

  std::vector<genericPointerTypeNative> imports{0}; // record tag takes one spot
  for (auto& [pid, indices] : unit.importTree) {
    auto closure = LibraryImpl::libraryIdMap.at(pid.c_str()).at(indices);
    imports.push_back(closure);
  }

  //! Start measuring at the GC initialisation.
  gc_init();

  // Get the structured record
  auto exportFunction = (genericFunctionTypeNative*)EE->getFunctionAddress("export");
  auto start_time = std::chrono::high_resolution_clock::now();
  auto lambda = (genericPointerTypeNative*)exportFunction((genericPointerTypeNative)imports.data(), nullptr);

  // Invoke the function with the given index. Indexing starts at 1.
  auto last_fnc = (genericPointerTypeNative*)lambda[functionIndex+1];

  genericIntTypeNative arg = 100'000'000;
  auto res = (genericIntTypeNative)((genericFunctionTypeNative*) last_fnc[0])
               ((genericPointerTypeNative)((arg << GC::valueFlagLength) + GC::intTag), last_fnc);

  res &= ~GC::floatTag;
  double double_res;
  std::memcpy(&double_res, &res, sizeof res);
  res >>= 1;

  auto result_time = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed_seconds = result_time-start_time;

  std::cout << "Time: " << (int)(elapsed_seconds.count()*1000) << "ms\n"
            << "Result: " << double_res << "(double), " << res << "(int)\n";


  return EXIT_SUCCESS;
}

}
