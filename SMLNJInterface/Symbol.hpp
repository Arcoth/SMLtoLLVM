#pragma once

#include "AbsynInterfaceBasic.hpp"

namespace SMLNJInterface::Symbol {
  const word varInt = 0, sigInt = 1, strInt = 2, fsignInt = 3,
             fctInt = 4, tycInt = 5, labInt = 6, tyvInt = 7,
             fixInt = 8;

  using symbol = tuple<word, string>;

  symbol parse_symbol(std::istream& is);

  enum Namespace {
    VALspace, TYCspace, SIGspace, STRspace, FCTspace,
    FIXspace, LABspace, TYVspace, FSIGspace
  };
}

namespace SMLNJInterface::SymPath {
  using path = vector<Symbol::symbol>;
}
