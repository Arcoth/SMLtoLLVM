#include "PLambda.hpp"

#include "ParserUtilities.hpp"

#include <boost/lexical_cast.hpp>

namespace SMLNJInterface::PLambda {

std::istream& operator>>(std::istream& is, lexp& lexp) {
  using namespace Parser;
  auto s = parse_identifier(is);
  if (s.empty()) {
    auto put_number = [&lexp, &is] (auto x) {
      is >> x;
      lexp = x;
    };
    char c = is.get();
    if (c == '(') {
      auto s = parse_identifier(is);
      is >> char_<')'>;
      if (s == "W") put_number(word{});
      else if (s == "W32") put_number(word32{});
      else if (s == "I32") {
        std::int32_t i; is >> i; lexp.emplace<INT32>(i);
      }
    }
    else if (c == '"') {
      string s;
      getline(is, s, '"');
      lexp.emplace<STRING>(s);
    }
    else {
      is.putback(c);
      int i; is >> i; lexp.emplace<INT>(i);
    }
  }
  else if (s[0] == 'v') { // VAR or LET
    int i = boost::lexical_cast<int>(s.substr(1));
    if (!is.eof() && (is >> std::ws).peek() == '=') { // check whether this is a LET expression
      is.ignore();
      struct lexp r, l;
      is >> r >> l;
      lexp.emplace<LET>(i, dynamic_wrapper(r), dynamic_wrapper(l));
    }
    else
      lexp.emplace<VAR>(i);
  }
  else if (s == "PRM") {
    Primop::primop p;
    lty l;
    is >> char_<'('> >> p >> char_<','> >> l >> char_<','>;
    auto v = parse_list<tyc>(is);
    is >> char_<')'>;
    lexp.emplace<PRIM>(p, l, v);
  }
  else if (s == "FN") {
    is >> char_<'('>;
    lvar v = parse_var(is);
    lty t;
    struct lexp l;
    is >> char_<':'> >> t >> char_<','> >> l >> char_<')'>;
    lexp.emplace<FN>(v, t, l);
  }
  else if (s == "APP") {
    struct lexp a, b;
    is >> char_<'('> >> a >> char_<','> >> b >> char_<')'>;
    lexp.emplace<APP>(a, b);
  }
  else if (s == "RCD")
    lexp.emplace<RECORD>(parse_sequence<struct lexp, '(', ',', ')'>(is));
  else if (s == "SRCD")
    lexp.emplace<SRECORD>(parse_sequence<struct lexp, '(', ',', ')'>(is));
  else if (s == "RAISE") {
    lty t; struct lexp l;
    is >> char_<'('> >> t >> char_<','> >> l >> char_<')'>;
    lexp.emplace<RAISE>(l, t);
  }
  else if (s == "CON") {
    // TODO
    throw std::logic_error{""};
  }
  else
    on_error(is, "lexp parser unknown symbol ", s);

  is >> std::ws;
  if (is.peek() == '[') {
    auto v = parse_list<lvar>(is);
    lexp.emplace<SELECT>(v, dlexp{std::move(lexp)});
  }

  return is;
}

}
