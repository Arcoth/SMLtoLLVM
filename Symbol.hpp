#pragma once

#include "AbsynInterfaceBasic.hpp"

namespace SMLNJAbsynInterface::Symbol {
  word varInt = 0, sigInt = 1, strInt = 2, fsignInt = 3,
       fctInt = 4, tycInt = 5, labInt = 6, tyvInt = 7,
       fixInt = 8;

  using symbol = tuple<word, string>;

  enum Namespace {
    VALspace, TYCspace, SIGspace, STRspace, FCTspace,
    FIXspace, LABspace, TYVspace, FSIGspace
  };
}

namespace SMLNJAbsynInterface::SymPath {
  using path = vector<Symbol::symbol>;
}
