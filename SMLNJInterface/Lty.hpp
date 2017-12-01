#pragma once

#include "Types.hpp"

#include <unordered_map>

namespace SMLNJInterface::PrimTyc {

enum primtyc {
  PT_INT31,
  PT_INT32,
  PT_REAL,
  PT_STRING,
  PT_EXN,
  PT_ARRAY,
  PT_VECTOR,
  PT_REF,
  PT_CONT,
  PT_CCONT,
  PT_ARROW,
  PT_OBJ,
  PT_CFUN,
  PT_BARRAY,
  PT_RARRAY,
  PT_SLOCK,
  PT_INTINF,
  PT_ETAG,
  PT_VOID
};

extern const std::unordered_map<std::string_view, PrimTyc::primtyc> primtyc_names;

}

namespace SMLNJInterface::Lty {

struct tkind : LABELLED_VARIANT(
  (TK_MONO)
  (TK_BOX)
  (TK_SEQ, vector<tkind>)
  (TK_FUN, pair<vector<tkind>, dynamic_wrapper<tkind>>)
);

std::istream& operator>>(std::istream& is, tkind& tk);

using token = int;

struct fflag : LABELLED_VARIANT(
  (FF_VAR, pair<bool, bool>)
  (FF_FIXED)
);

enum rflag {RF_TMP}; // redundant (each tuple is a template)

using tvar = LambdaVar::lvar;

struct tyc;
using tycEnv = tyc;
using dtyc = dynamic_wrapper<tyc>;
struct tyc : LABELLED_VARIANT(
  (TC_VAR, tuple<DebIndex::index, int>)
  (TC_NVAR, tvar)
  (TC_PRIM, PrimTyc::primtyc)
  (TC_FN, tuple<vector<tkind>, dtyc>)
  (TC_APP, tuple<dtyc, vector<tyc>>)
  (TC_SEQ, vector<tyc>)
  (TC_PROJ, tuple<dtyc, int>)
  (TC_SUM, vector<tyc>)
  (TC_FIX, struct {
    struct {
      int size;
      vector<string> names;
      dtyc gen;
      vector<tyc> params;
      int index;
    } family;
  })
  (TC_TUPLE, vector<tyc>) // we omit rflag, as it's a singleton type
  (TC_ARROW, tuple<fflag, vector<tyc>, vector<tyc>>)
  (TC_PARROW, tuple<dtyc, dtyc>)
  (TC_BOX, dtyc)
  (TC_ABS, dtyc)
  (TC_TOKEN, tuple<token, dtyc>)
  (TC_CONT, vector<tyc>)
  (TC_IND, tuple<dtyc, dtyc>)
  (TC_ENV, tuple<dtyc, int, int, dynamic_wrapper<tycEnv>>)
);

std::istream& operator>>(std::istream& is, tyc& tyc);

struct lty : LABELLED_VARIANT(
  (LT_TYC, tyc)
  (LT_STR, vector<lty>)
  (LT_FCT, pair<vector<lty>, vector<lty>>)
  (LT_POLY, pair<vector<tkind>, vector<lty>>)
  (LT_CONT, vector<lty>)
  (LT_IND, pair<dynamic_wrapper<lty>, dynamic_wrapper<lty>>)
  (LT_ENV, tuple<dynamic_wrapper<lty>, int, int, tycEnv>)
);

std::istream& operator>>(std::istream& is, lty& lty);

}
