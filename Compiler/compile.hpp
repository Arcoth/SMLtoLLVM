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

struct Occurrence {
  PLambda::lexp* enclosing_exp; // the expression that immediately ENCLOSES the occurrence.
  PLambda::lexp* holding_exp; // the expression that IS the occurrence.
};

class SMLTranslationUnit;

std::multimap<PLambda::lvar, Occurrence> freeVarOccurrences( PLambda::lexp& exp, PLambda::lexp* enclosing, SMLTranslationUnit const& );

inline const int heapAddressSpace = 1;

inline char const* const smallAllocFun   = "allocateSmall",
                 * const largeAllocFun     = "allocateLarge",
                 * const mutableAllocFun = "allocateMutable";

inline const auto genericPointerType = Type::getInt8Ty(context)->getPointerTo(heapAddressSpace),
                  genericPtrToPtr = genericPointerType->getPointerTo(heapAddressSpace),
                  closurePointerType = genericPtrToPtr;

inline const auto genericIntType = Type::getInt64Ty(context);

inline const auto realType = Type::getDoubleTy(context);

// This is one half of an entire data tag, so has half the width of a machine word.
inline auto tagType(Module& m) {
  return IntegerType::get(m.getContext(), genericIntType->getBitWidth()/2) ;
}

// The default function type of any function in the PLC.
inline auto genericFunctionType(bool isCPS = false) {
  if (isCPS)
    return FunctionType::get(Type::getVoidTy(context),
                             {genericPointerType,  // the argument
                              closurePointerType,  // the environment.
                              genericPtrToPtr}, // the CPS pointer.
                             false); // not variadic
  return FunctionType::get(genericPointerType,   // the return value
                           {genericPointerType,  // the argument
                            closurePointerType}, // the environment.
                           false); // not variadic
}

inline auto allocationSizeOf(Module* module, Type* type) {
  return DataLayout{module}.getTypeAllocSize(type);
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

  using ConstantVariant = std::variant<Function*, ConstantData*>;

  std::unordered_map<PLambda::lvar, ConstantVariant> assignedConstant; // if a variable is assigned a global constant

  std::unordered_map<PLambda::lvar, std::map<std::size_t, ConstantVariant>> assignedSRecords;

  // This map equates parameter index and the name of the LLVḾ function.
  std::map<PLambda::lvar, std::string> paramFuncs,
  // Includes both the lambda variable and the name of the entity it denotes.
                                       exportedDecls;

  PLambda::lvar importVariable;
};

inline char const* nameForFunction(PLambda::lvar v, bool cps = false, bool rec = false) {
  const std::size_t len = 32;
  char* n = new char[len] {};
  auto written = std::snprintf(n, len, "%s%slambda.v%d", (cps? "cps_" : ""), (rec? "rec_" : ""), v);
  assert(written > 0 && (std::size_t)written < len);
  return n;
}

struct AstContext {
  struct FixPointInfo {
    PLambda::lvar variable;
    std::vector<Lty::tyc> const& paramType;
    Lty::tyc const& retType;
  };
  std::optional<FixPointInfo> fixPoint;
  bool isFunctionOfFixPoint;   // The immediate function of a fixpoint variable
  bool isSolelyApplied;        // Function only applied?
  bool isFinalExpression;      // expression whose value is immediately yielded by a function
  bool isCtorArgument;         // Argument to a construtor
  bool moduleExportExpression; // The top-most lambda expression
  bool isListCPSFunction;      // True if this function stores the resulting list in parameter %3
  std::size_t isRecordFunction;      // n > 0 if this function is an n-ary record function, otherwise 0
  PLambda::lexp* enclosingFunctionExpr; // If isListCPSFunction, this stores the expression for creating the second function definition
};

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit& unit);

}

