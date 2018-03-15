#pragma once

#include "basic.hpp"

#include "SMLNJInterface/PLambda.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>

#include <set>
#include <map>

#include <boost/unordered_map.hpp>
#include <boost/container/string.hpp>
#include <boost/container/vector.hpp>

namespace SMLCompiler {

using namespace SMLNJInterface;
using namespace llvm;

std::set<PLambda::lvar> freeVars( PLambda::lexp const& exp );


struct Occurrence {
  PLambda::lexp* enclosing_exp; // the expression that immediately ENCLOSES the occurrence.
  PLambda::lexp* holding_exp; // the expression that IS the occurrence.
};

std::multimap<PLambda::lvar, Occurrence> freeVarOccurrences( PLambda::lexp& exp, PLambda::lexp* enclosing);

inline const int heapAddressSpace = 1;

inline char const* const immutableAllocFun = "allocateImmutable",
                 * const mutableAllocFun = "allocateMutable";

inline auto genericPointerType(LLVMContext& c) {
  return Type::getInt8Ty(c)->getPointerTo(heapAddressSpace);
}

inline auto closurePointerType(LLVMContext& c) {
  return genericPointerType(c)->getPointerTo(heapAddressSpace);
}

inline auto genericIntType(Module& m) {
  return m.getDataLayout().getIntPtrType(m.getContext());
}

// This is one half of an entire data tag, so has half the width of a machine word.
inline auto tagType(Module& m) {
  return IntegerType::get(m.getContext(), genericIntType(m)->getBitWidth()/2) ;
}

// The type of a function that takes an integer.
inline auto intFunctionType(Module& m) {
  auto& ctx = m.getContext();
  return FunctionType::get(genericPointerType(ctx),   // the return value
                           {genericIntType(m),  // the argument
                            closurePointerType(ctx)}, // the environment.
                           false); // not variadic
}

// The type of a function that takes an integer.
inline auto realFunctionType(Module& m) {
  return intFunctionType(m);
}

// The default function type of any function in the PLC.
inline auto genericFunctionType(LLVMContext& ctx) {
  return FunctionType::get(genericPointerType(ctx),   // the return value
                           {genericPointerType(ctx),  // the argument
                            closurePointerType(ctx)}, // the environment.
                           false); // not variadic
}

struct symbol_rep {
  enum {
    UNTAGGED, TAGGED, CONSTANT
  } type;
  ConstantInt* value;
};

using ImportsVector = boost::container::vector<std::pair<boost::container::string, boost::container::vector<int>>>;

class SMLTranslationUnit {
public:
  SMLTranslationUnit(PLambda::lexp const&, ImportsVector imports, llvm::Module*);

  // The imports by PID.
  ImportsVector importTree;

  // for each function, records the length of the closure.
  boost::container::vector<std::pair<Function*, std::size_t>> closureLength;

  llvm::Module* module;

  // The innermost closed let expression.
  PLambda::lexp exportedLexp;

  std::map<Symbol::symbol, symbol_rep> symbolRepresentation;

  // This map equates parameter index and the name of the LLVḾ function.
  std::map<PLambda::lvar, std::string> paramFuncs,
  // Includes both the lambda variable and the name of the entity it denotes.
                                       exportedDecls;
};

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit& unit);

}

