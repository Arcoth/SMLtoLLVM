#pragma once

#include "basic.hpp"

#include "SMLNJInterface/PLambda.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>

#include <set>

namespace SMLCompiler {

using namespace SMLNJInterface;
using namespace llvm;

std::set<PLambda::lvar> freeVars( PLambda::lexp const& exp );

using genericPointerTypeNative = char*;
using genericFunctionTypeNative = genericPointerTypeNative(genericPointerTypeNative, genericPointerTypeNative[]);

inline auto genericPointerType(LLVMContext& c) {
  return Type::getInt8PtrTy(c);
}

inline auto genericIntType(Module& m) {
  return m.getDataLayout().getIntPtrType(m.getContext());
}

// The default function type of any function in the PLC.
inline auto genericFunctionType(LLVMContext& ctx) {
  return FunctionType::get(genericPointerType(ctx),   // the return value
                           {genericPointerType(ctx),  // the argument
                            genericPointerType(ctx)->getPointerTo()}, // the environment (if needed).
                           false); // not variadic
}

struct symbol_rep {
  enum {
    UNTAGGED, TAGGED, CONSTANT
  } type;
  ConstantInt* value;
};

class SMLTranslationUnit {
public:
  SMLTranslationUnit(PLambda::lexp const&);

  // The innermost closed let expression.
  PLambda::lexp exportedLetExpr;

  std::map<Symbol::symbol, symbol_rep> symbolRepresentation;

  // Includes both the lambda variable and the name of the entity it denotes.
  std::map<PLambda::lvar, std::string> exportedDecls;
};

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit const& unit, Module& module);

}

