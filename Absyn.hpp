#pragma once

//#include "Modules.hpp"
#include "Primop.hpp"
#include "Types.hpp"

namespace SMLNJAbsynInterface {

using region = pair<int, int>; // Ast.region

struct numberedLabel {
  Symbol::symbol name;
  int number;
};

namespace Fixity {
  struct fixity : LABELLED_VARIANT(
    (NONfix)
    (INfix, pair<int, int>)
  );
}


namespace PrimopBind {
  using primop_bind = tuple<string, Types::ty, Primop::primop>;
}

namespace PrimopId {
  struct prim_id : LABELLED_VARIANT(
    (Prim, PrimopBind::primop_bind)
    (NonPrim)
  );
}

namespace VarCon {
  struct var : LABELLED_VARIANT(
    (VALvar, struct {
      SymPath::path path;
      Types::ty typ;
      vector<Types::tyvar> btvs;
      Access::access access;
      PrimopId::prim_id prim;
    })
    (OVLDvar, struct {
      Symbol::symbol name;
    private:
      struct Options {
        Types::ty indicator;
        dynamic_wrapper<var> variant;
      };
    public:
      vector<Options> options;
      Types::tyfun scheme;
    })
    (ERRORvar)
  );

  using datacon = Types::datacon;

  struct value : LABELLED_VARIANT(
    (VAL, var)
    (CON, datacon)
  );
}

using namespace Types;

struct exp;
struct dec;
struct pat;

using dexp = dynamic_wrapper<exp>;
using dpat = dynamic_wrapper<pat>;

using rule = pair<dpat, dexp>;

using fnrules = pair<dynamic_wrapper<rule>, ty>;

struct exp : LABELLED_VARIANT(
  (VARexp, pair<VarCon::var, vector<tyvar>>)
  /* the 2nd arg is a type univar list used to capture the instantiation
     parameters for this occurence of var when its type is polymorphic.
     FLINT will use these to provide explicit type parameters for
     var if var is bound to a primop. These will then be used to specialize
     the primop. */
  (CONexp, pair<datacon, vector<tyvar>>) // ditto
  (INTexp, pair<maxint, ty>)
  (WORDexp, pair<maxint, ty>)
  (REALexp, string)
  (STRINGexp, string)
  (CHARexp, string)
  (RECORDexp, vector<pair<numberedLabel, dexp>>)
  (SELECTexp, pair<numberedLabel, dexp>) //  record selection
  (VECTORexp, pair<vector<dexp>, ty>)
  (PACKexp, tuple<dexp, ty, vector<tycon>>) //  abstraction packing
  (APPexp, pair<dexp, dexp>)
  (HANDLEexp, pair<dexp, fnrules>)
  (RAISEexp, pair<dexp, ty>)
  (CASEexp, tuple<dexp, vector<rule>, bool>) // true: match; false: bind
  (IFexp, struct{dexp exp,
                      thenCase,
                      elseCase;} )
  (ANDALSOexp, pair<dexp, dexp>)
  (ORELSEexp, pair<dexp, dexp>)
  (WHILEexp, struct {
    dexp test, expr;
  })
  (FNexp, fnrules)
  (LETexp, pair<dynamic_wrapper<dec>, dexp>)
  (SEQexp, vector<exp>)
  (CONSTRAINTexp, pair<dexp, ty>)
  (MARKexp, pair<dexp, region>)
);

struct pat : LABELLED_VARIANT(
  (WILDPAT)
  (VARpat, VarCon::var)
  (INTpat, pair<maxint, ty>)
  (REALpat, string)
  (STRINGpat, string)
  (CHARpat, string)
  (CONpat, pair<datacon, vector<tyvar>>) // see comment for VARexp
  (RECORDpat, struct {
    map<label, pat> fields;
    bool flex;
    ty typ;
  })
  (APPpat, tuple<datacon, vector<tyvar>, dpat>)
  (CONSTRAINTpat, pair<dpat, ty>)
  (LAYEREDpat, pair<dpat, dpat>)
  (ORpat, pair<dpat, dpat>)
  (VECTORpat, pair<vector<pat>, ty>)
  (MARKpat, pair<dpat, region>)
  (NOpat)
);

/*
 * Each value binding vb only binds one variable identifier. That is,
 * pat is always a simple VARpat (with type constraints) or it simply
 * does not contain any variable patterns; boundtvs gives the list of
 * type variables that are being generalized at this binding.
 */
struct vb {
  pat pat;
  exp exp;
  vector<tyvar> boundtvs;
  vector<tyvar> tyvars;
};

/*
 * Like value binding vb, boundtvs gives a list of type variables
 * being generalized at this binding. However, the mutually recursive
 * list of RVBs could share type variables, that is, the boundtvs sets
 * used in these RVBs could contain overlapping set of type variables.
 */
struct rvb {
  VarCon::var var;
  exp exp;
  vector<tyvar> boundtvs;
  optional<ty> resultty;
  vector<tyvar> tyvars;
};

struct eb : LABELLED_VARIANT(
  (EBgen, struct {
    datacon exn;
    optional<ty> etype;
    exp ident;
  })
  (EBdef, struct {
    datacon exn;
    datacon edef;
  })
);

//! Modules are not implemented.
/*  The following are omitted:
    - open declarations
    - structure declarations
    - abstract declarations
    - signature declarations
*/
struct dec : LABELLED_VARIANT(
  (VALdec, vector<vb>)
  (VALRECdec, vector<rvb>)
  (DOdec, exp)
  (TYPEdec, vector<tycon>)
  (DATATYPEdec, struct {
    vector<tycon> datatycs, withtycs;
  })
  (ABSTYPEdec, struct {
    vector<tycon> datatycs, withtycs;
    dynamic_wrapper<dec> body;
  })
  (EXCEPTIONdec, vector<eb>)
  //(STRdec, vector<strb>)
  //(ABSdec, vector<strb>)
  //(FCTdec, vector<fctb>)
  //(SIGdec, vector<Modules::Signature>)
  //(FSIGdec, vector<fctSig>)
  //(OPENdec, vector<SymPath::path, Modules::Structure>)
  (LOCALdec, pair<dynamic_wrapper<dec>, dynamic_wrapper<dec>>)
  (SEQdec, vector<dec>)
  (OVLDdec, VarCon::var)
  (FIXdec, struct {
    Fixity::fixity fixity;
    vector<Symbol::symbol> ops;
  })
  (MARKdec, pair<dynamic_wrapper<dec>, region>)
);

} // end namespace SMLNJAbsynInterface
