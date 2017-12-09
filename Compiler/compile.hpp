#pragma once

#include "basic.hpp"

#include "SMLNJInterface/PLambda.hpp"

#include <llvm/IR/Module.h>

#include <set>

namespace SMLCompiler {

using namespace SMLNJInterface;

std::set<PLambda::lvar> freeVars( PLambda::lexp const& exp );

class SMLTranslationUnit {
public:
  // OBtain the list of expressions in the chain of LET-expressions provided by
  // SML/NJ's intermediate PLambda language.
  SMLTranslationUnit(PLambda::lexp const&);

  std::vector<std::pair<PLambda::lvar, PLambda::lexp>> globalDecls;
  std::vector<PLambda::lvar> exportedDecls;
};

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(PLambda::lexp const& expression,
                 llvm::Module& module);

}

