#include "ParserUtilities.hpp"
#include "Primop.hpp"

#include "StringSwitchHash.hpp"

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

std::istream& operator>>(std::istream& is, numkind& nk) {
  char c;
  unsigned bits;
  if (!(is >> c >> std::noskipws >> bits))
    return is;
  if (c == 'i') nk.emplace<INT>(bits);
  else if (c == 'u') nk.emplace<UINT>(bits);
  else if (c == 'f') nk.emplace<FLOAT>(bits);
  else Parser::on_error(is, "Invalid numkind prefix: ", c);
  return is;
}


std::istream& operator>>(std::istream& is, primop& op) {
  using namespace Parser;
  auto s = parse_identifier_nounsc(is);
  if (arithop_names.count(s)
   || (ends_with(s, 'n') && arithop_names.count(s.substr(s.length()-1)))) {
    bool overflow = ends_with(s, 'n');
    if (overflow)
      s.pop_back();

    numkind nk;
    if (is >> std::noskipws >> char_<'_'> >> nk >> std::skipws)
      emplace_by_value<ARITH>(op, {.oper = arithop_names.at(s),
                                   .overflow = overflow,
                                   .kind = nk});
  }
  else if (s == "markexn")
    op.emplace<MARKEXN>();
  else
    on_error(is, "Invalid primop prefix: ", s);
  return is;
}

} // end SMLNJInterface::Primop
