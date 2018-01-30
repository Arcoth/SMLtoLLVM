#include "ParserUtilities.hpp"
#include "Primop.hpp"

#include "Misc/StringSwitchHash.hpp"

#include <istream>

namespace SMLNJInterface::Primop {

const std::unordered_map<std::string_view, arithop> arithop_names {
  {"add", ADD},
  {"sub", SUB},
  {"mul", MUL},
  {"div", DIV},
  {"mod", MOD},
  {"neg", NEG},
  {"fdiv", FDIV},
  {"abs", ABS},
  {"fsqrt", FSQRT},
  {"fsin", FSIN},
  {"fcos", FCOS},
  {"ftan", FTAN},
  {"lshift", LSHIFT},
  {"rshift", RSHIFT},
  // TODO
};

const std::unordered_map<std::string_view, cmpop> cmpop_strings {
  {"=", EQL}, {"<>", NEQ},
  {"<", LT}, {">", GT}, {"<=", LTE}, {">=", GTE},
  {"<U", LTU}, {">U", GTU}, {"<=U", LEU}, {">=U", GEU},
  {"fsgn", FSGN}
};

std::istream& operator>>(std::istream& is, numkind& nk) {
  char c;
  unsigned bits;
  if (!(is >> c >> bits))
    return is;
  if (c == 'i') nk.emplace<INT>(bits);
  else if (c == 'u') nk.emplace<UINT>(bits);
  else if (c == 'f') nk.emplace<FLOAT>(bits);
  else Parser::on_error(is, "Invalid numkind prefix: ", c);
  return is;
}


std::istream& operator>>(std::istream& is, primop& op) {
  using namespace Parser;

  auto s = extract_cond(is, [] (auto&, char c) {
    return std::isgraph(c) && c != '_' && c != ',';});

  if (arithop_names.count(s)
   || (ends_with(s, 'n') && arithop_names.count(s.substr(0, s.length()-1)))) {
    bool overflow = ends_with(s, 'n');
    if (overflow)
      s.pop_back();

    numkind nk;
    if (is >> char_<'_'> >> nk)
      emplace_by_value<ARITH>(op, {.oper = arithop_names.at(s),
                                   .overflow = overflow,
                                   .kind = nk});
    else
      on_error(is, "Failed parsing numkind for primop ", s);
  }
  else if (cmpop_strings.count(s)) {
    numkind nk;
    if (is >> char_<'_'> >> nk)
      emplace_by_value<CMP>(op, {.oper = cmpop_strings.at(s),
                                 .kind = nk});
    else
      on_error(is, "Failed parsing numkind for primop ", s);
  }
  else if (s == "markexn")
    op.emplace<MARKEXN>();
  else if (s == "!")
    op.emplace<DEREF>();
  else if (s == ":=")
    op.emplace<ASSIGN>();
  else if (s == "makeref")
    op.emplace<MAKEREF>();
  else
    on_error(is, "Invalid primop prefix: ", s);
  return is;
}

} // end SMLNJInterface::Primop
