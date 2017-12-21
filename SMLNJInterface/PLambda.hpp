#pragma once

#include "Lty.hpp"
#include "Primop.hpp"

namespace SMLNJInterface::PLambda {

using Lty::tyc;
using Lty::tkind;
using Lty::lty;

using lvar = LambdaVar::lvar;

using dataconstr = tuple<Symbol::symbol, Access::conrep, lty>;

dataconstr parse_dataconstr(std::istream& is);

struct con : LABELLED_VARIANT(
  //(DATAcon, tuple<dataconstr, vector<tyc>, lvar>)
  (DATAcon, tuple<Symbol::symbol, lvar>)
  (INTcon, int)
  (INT32con, std::int32_t)
  (INTINFcon, maxint)
  (WORDcon, word)
  (WORD32con, word32)
  (REALcon, string)
  (STRINGcon, string)
  (VLEN, int)
);

std::istream& operator>>(std::istream& is, con&);

struct lexp;
using dlexp = dynamic_wrapper<lexp>;

struct dict {
  dlexp default_;
  vector<pair<vector<tyc>, lexp>> list;
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

  // 9-12:
  (FN, tuple<lvar, lty, dlexp>)
  (FIX, tuple<vector<tuple<lvar, lty, lexp>>, dlexp>)
  (APP, pair<dlexp, dlexp>)
  (LET, tuple<lvar, dlexp, dlexp>)

  // 13-14:
  (TFN, pair<vector<tkind>, dlexp>)
  (TAPP, pair<dlexp, vector<tyc>>)

  // 15-17:
  (RAISE, pair<dlexp, lty>)
  (HANDLE, pair<dlexp, dlexp>)
  (ETAG, pair<dlexp, lty>)

  // 18-19:
  (CON, tuple<dataconstr, vector<tyc>, dlexp>)
  (SWITCH, tuple<dlexp, vector<pair<con, lexp>>, optional<dlexp>>)

  // 20-23:
  (VECTOR, pair<vector<lexp>, tyc>)
  (RECORD, vector<lexp>)
  (SRECORD, vector<lexp>)
  (SELECT, pair<vector<int>, dlexp>)

  (PACK, tuple<lty, vector<tyc>, vector<tyc>, dlexp>)
  (WRAP, tuple<tyc, bool, dlexp>)
  (UNWRAP, tuple<tyc, bool, dlexp>)
);

std::istream& operator>>(std::istream& is, lexp& lexp);

}
