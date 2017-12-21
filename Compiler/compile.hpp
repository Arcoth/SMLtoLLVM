#pragma once

#include "basic.hpp"

#include "SMLNJInterface/PLambda.hpp"

#include <llvm/IR/Module.h>

#include <set>

namespace SMLCompiler {

using namespace SMLNJInterface;
using namespace llvm;

std::set<PLambda::lvar> freeVars( PLambda::lexp const& exp );

inline auto genericPointerType(LLVMContext& c) {
  return Type::getInt8PtrTy(c);
}

// The default function type of any function in the PLC.
inline auto genericFunctionType(LLVMContext& ctx) {
  return FunctionType::get(genericPointerType(ctx),   // the return value
                           {genericPointerType(ctx),  // the argument
                            genericPointerType(ctx)->getPointerTo()}, // the environment (if needed).
                           false); // not variadic
}

class SMLTranslationUnit {
public:
  // OBtain the list of expressions in the chain of LET-expressions provided by
  // SML/NJ's intermediate PLC language.
  SMLTranslationUnit(PLambda::lexp const&);

  // All LET declarations enclosing the structure record contained in a PLC term denoting a program.
  std::vector<std::pair<PLambda::lvar, PLambda::lexp>> globalDecls;

  // Includes both the lambda variable and the name of the entity it denotes.
  std::map<PLambda::lvar, std::string> exportedDecls;
};

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit const& unit,
                 Module& module);

}

