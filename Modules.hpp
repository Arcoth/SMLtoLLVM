#pragma once

#include "PropList.hpp"
#include "Stamps.hpp"
#include "Symbol.hpp"
#include "Types.hpp"

//! This is highly incomplete. But since the project doesn't implement modules, the completion is postponed.

namespace SMLNJAbsynInterface::Modules {

using sharespec = vector<SymPath::path>;

/* there are two forms of TYCspec. One for regular, explicitly defined signatures,
 * and the other for inferred signatures, where all the type info is always in the
 * realization. But we need some info for printing in the one case where a
 * realization is not available with the signature, namely an inferred result
 * signature for a functor. */
struct tycSpecInfo : LABELLED_VARIANT(
  (RegTycSpec, struct {
    Types::tycon spec;
    bool repl;
    int scope;
  })
  (InfTycSpec, struct {
    Symbol::symbol name;
    int arity;
  })
);

using elements = map<Symbol::symbol, spec>;

struct sigrec {
  Stamps::stamp stamp;
  optional<Symbol::symbol> name;
  bool closed;
  bool fctflag;
  elements elements;
  PropList.holder properties;

};


struct Signature : LABELLED_VARIANT(
  (SIG, sigrec)
  (ERRORsig)
);

}

