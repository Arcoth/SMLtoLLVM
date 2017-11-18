#pragma once

#include "Stamps.hpp"
#include "Symbol.hpp"

namespace SMLNJInterface {

namespace LambdaVar {
  using lvar = int; // lambda variable id number
}

namespace Access {
  using lvar = LambdaVar::lvar;
  using persstamp = PersStamps::persstamp;
  /*
   * access: how to find the dynamic value corresponding to a variable.
   * An LVAR is just a lambda-bound variable --- a temporary used to denote
   * a binding in the current compilation unit. EXTERN refers to a binding
   * defined externally (in other modules). PATH is an absolute address from
   * a lambda-bound variable (i.e. we find the value of the lambda-bound
   * variable, and then do selects from that). PATH's are kept in reverse
   * order. NO_ACCESS is used to denote built-in structures that do not
   * have corresponding dynamic objects (e.g., the built-in InLine is a
   * structure that declares all the built-in primitives --- it is likely
   * that NO_ACCESS will go away in the future once we have cleaned up the
   * bootstrap procedure. */
  struct access : LABELLED_VARIANT(
    (LVAR, lvar)
    (EXTERN, persstamp)
    (PATH, pair<dynamic_wrapper<access>, int>)
    (NO_ACCESS)
  );

  /*
   * conrep: how to decide the data representations for data constructors.
   * All true datatypes are divided into four categories, depending on the
   * pair of parameters (m,n) where m is the number of constant constructors
   * and n is the number of value carrying constructors. REF, EXN, SUSP
   * are special constructors for reference cells, exceptions, and suspensions;
   * treating them as data constructors simplifies the match compilation.
   * LISTCONS and LISTNIL are special conreps for unrolled lists. The process
   * of assigning conreps probably should be performed on the intermediate
   * language instead.
   */
  struct conrep : LABELLED_VARIANT(
    (UNTAGGED)        // 30 bit + 00; a pointer
    (TAGGED, int)     // a pointer; 1st field is the tag
    (TRANSPARENT)     // 32 bit value, singleton dcon dt
    (CONSTANT, int)   // should be int31
    (REF)
    (EXN, access)
    (SUSP, optional<pair<access, access>>)
    (LISTCONS)
    (LISTNIL)
  );

  struct consig : LABELLED_VARIANT(
    (CSIG, pair<int, int>)
    (CNIL)
  );
}


namespace EntPath {
  using entVar = Stamps::stamp;
  using entPath = vector<entVar>; // entPath has entVars in direct order, outer first
  using rEntPath = vector<entVar>; // reversed order; abstract
}

namespace InvPath {
  using path = vector<Symbol::symbol>;
}

namespace DebIndex {
  using depth = unsigned;
  using index = unsigned; // index >= 1

  constexpr unsigned top = 0;
  inline auto next(depth i) {return i+1;}
  inline auto prev(depth i) {
    if (i > 0)
      return i-1;
    throw std::invalid_argument{"negative depth in prev"};
  }
}

namespace TKind {
  struct tkind : LABELLED_VARIANT(
    (TKCint, int)
    (TKCfun, pair<vector<tkind>, dynamic_wrapper<tkind>>)
    (TKCseq, vector<tkind>)
  );
}

namespace SourceMap {
  using charpos = int;
  using region  = pair<charpos, charpos>;
  struct sourceloc {
    string fileName;
    int line;
    int column;
  };
}

namespace PropList {
  using holder = vector<std::exception>;
}

// ///////////////////////////////////

namespace Types {

struct ty;
struct tyckind;
struct tycon;

using label = Symbol::symbol;

using polysign = vector<bool>;

enum eqprob {
  YES, NO, IND, OBJ, DATA, ABS, UNDEF
};

enum litKind {
  INT, WORD, REAL, CHAR, STRING
};


//  datacon description used in dtmember
struct dconDesc {
  Symbol::symbol name;
  Access::conrep rep;
  optional<dynamic_wrapper<ty>> domain;
};

struct dtmember {
  Symbol::symbol tycname;
  int arity;
  eqprob eq;
  bool lazyp;
  vector<dconDesc> dcons;
  Access::consig sign;
};

struct dtypeFamily {
  Stamps::stamp mkey;
  vector<dtmember> members;
  PropList::holder properties;
};

struct stubinfo {
  PersStamps::persstamp owner;
  bool lib;
};

struct gtrec {
  Stamps::stamp stamp;
  int arity;
  eqprob eq;
  dynamic_wrapper<tyckind> kind;
  InvPath::path path;
  optional<stubinfo> stub;
};


struct ovldSource : LABELLED_VARIANT(
  (OVAR, tuple<Symbol::symbol, SourceMap::region>)
  (OLIT, tuple<litKind, maxint, SourceMap::region>)
);

struct openTvKind : LABELLED_VARIANT(
  (META)
  (FLEX, vector<tuple<label, ty>>)
);

struct tvKind : LABELLED_VARIANT(
  (INSTANTIATED, dynamic_wrapper<ty>)
  (OPEN, struct {
    int depth;
    bool eq;
    openTvKind kind;
  })
  (UBOUND, struct {
    int depth;
    bool eq;
    Symbol::symbol name;
  })
  (OVLD, struct {
    vector<ovldSource> sources;
    vector<ty> options;
  })
  (LBOUND, struct {
    int depth;
    bool eq;
    int index;
  })
);

using tyvar = tvKind;

struct tycpath : LABELLED_VARIANT(
  (TP_VAR, struct {
    DebIndex::depth tdepth;
    int num;
    TKind::tkind kind;
  })
  (TP_TYC, dynamic_wrapper<tycon>)
  (TP_FCT, pair<vector<tycpath>, vector<tycpath>>)
  (TP_APP, pair<dynamic_wrapper<tycpath>, vector<tycpath>>)
  (TP_SEL, pair<dynamic_wrapper<tycpath>, int>)
);

struct tyckind : LABELLED_VARIANT(
  (PRIMTIVE)
  (DATATYPE, struct {
    int index;
    vector<Stamps::stamp> stamps;
    optional<EntPath::entVar> root; // the root field used by type spec only
    vector<tycon> freetycs; // tycs derived from functor param
    dtypeFamily family;
    bool stripped; // true if datatype has matched a simple type spec
  })
  (ABSTRACT, dynamic_wrapper<tycon>)
  (FLEXTYC, tycpath)
  (FORMAL)
  (TEMP)
);

struct tyfun;

struct tycon : LABELLED_VARIANT(
  (GENtyc, gtrec)
  (DEFtyc, struct {
    Stamps::stamp stamp;
    dynamic_wrapper<tyfun> tyfun;
    vector<bool> strict;
    InvPath::path path;
  })
  (PATHtyc, struct { // used only inside signatures
    int arity;
    EntPath::entPath entPath;
    InvPath::path path;
  })
  (RECORDtyc, vector<label>)
  (RECtyc, int)              // used only in domain type of dconDesc
  (FREEtyc, int)             // used only in domain type of dconDesc
  (ERRORtyc)                 // for error recovery, and used as a dummy tycon in ElabMod.extractSig
);

struct ty : LABELLED_VARIANT(
  (VARty, tyvar)
  (IBOUND, int)
  (CONty, pair<tycon, vector<ty>>)
  (POLYty, struct {
    polysign sign;
    dynamic_wrapper<tyfun> tyfun;
  })
  (WILDCARDty)
  (UNDEFty)
  (MARKty, pair<dynamic_wrapper<ty>, SourceMap::region>)
);

struct tyfun {
  int arity;
  ty body;
};

// data constructors
struct datacon {
  Symbol::symbol name;
  ty typ;
  Access::conrep rep;
  bool lazyp; // LAZY: constructor belongs to lazy datatype?
  bool const_; // redundant, could be determined from typ
  Access::consig sign; // redundant, ditto
};

} // end namespace Types

} // end namespace SMLNJAbsynInterface
