#pragma once

#include "Lty.hpp"
#include "Primop.hpp"

namespace SMLNJInterface::PLambda {

using Lty::tyc;
using Lty::tkind;
using Lty::lty;

using lvar = LambdaVar::lvar;

using dataconstr = tuple<Symbol::symbol, Access::conrep, lty>;

struct con : LABELLED_VARIANT(
  (DATAcon, tuple<dataconstr, vector<tyc>, lvar>)
  (INTcon, int)
  (INT32con, std::int32_t)
  (INTINFcon, maxint)
  (WORDcon, word32)
  (WORD32con, word)
  (REALcon, string)
  (STRINGcon, string)
  (VLEN, string)
);

struct lexp;
using dlexp = dynamic_wrapper<lexp>;

struct dict {
  dlexp default_;
  map<vector<tyc>, lexp> list;
};

struct lexp : LABELLED_VARIANT(
  (VAR, lvar)
  (INT, int)
  (INT32, std::int32_t)
  (WORD, word)
  (WORD32, word32)
  (REAL, string)
  (STRING, string)
  (PRIM, tuple<Primop::primop, lty, vector<tyc>>)
  (GENOP, tuple<dict, Primop::primop, lty, vector<tyc>>)

  (FN, tuple<lvar, lty, dlexp>)
  (FIX, tuple<vector<lvar>, vector<lty>, vector<lexp>, dlexp>)
  (APP, pair<dlexp, dlexp>)
  (LET, tuple<lvar, dlexp, dlexp>)

  (TFN, pair<vector<tkind>, dlexp>)
  (TAPP, pair<dlexp, vector<tyc>>)

  (RAISE, pair<dlexp, lty>)
  (HANDLE, pair<dlexp, dlexp>)
  (ETAG, pair<dlexp, lty>)

  (CON, tuple<dataconstr, vector<tyc>, dlexp>)
  (SWITCH, tuple<dlexp, Access::consig, map<con, lexp>, optional<dlexp>>)

  (VECTOR, pair<vector<lexp>, tyc>)
  (RECORD, vector<lexp>)
  (SRECORD, vector<lexp>)
  (SELECT, pair<int, dlexp>)

  (PACK, tuple<lty, vector<tyc>, vector<tyc>, dlexp>)
  (WRAP, tuple<tyc, bool, dlexp>)
  (UNWRAP, tuple<tyc, bool, dlexp>)
);

}
