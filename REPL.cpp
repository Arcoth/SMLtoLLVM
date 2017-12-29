#include "Compiler/compile.hpp"
#include "REPL.h"

#include <llvm/IR/Module.h>

#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include <llvm/Support/TargetSelect.h>

#include <iostream>

namespace SMLCompiler {

using namespace llvm;

int execute(Module* mod) {
  InitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  std::string err_str;
  auto EE = EngineBuilder{std::unique_ptr<Module>{mod}}.setErrorStr(&err_str).create();
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
//  }

  auto fce_ptr = (genericPointerTypeNative*(*)())EE->getFunctionAddress("export");
  auto lambda = fce_ptr();

  auto last_fnc = (genericPointerTypeNative*)lambda[0];
  auto res = ((genericFunctionTypeNative*) last_fnc[0])
               ((genericPointerTypeNative)5, last_fnc+1);

  outs() << "Result: " << (intptr_t)res << '\n';

  return EXIT_SUCCESS;
}

}
