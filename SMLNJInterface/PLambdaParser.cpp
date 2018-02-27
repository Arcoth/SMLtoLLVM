#include "PLambda.hpp"

#include "ParserUtilities.hpp"

namespace SMLNJInterface::PLambda {

using namespace Parser;

dataconstr parse_dataconstr(std::istream& is) {
  is >> char_<'('>;
  auto sym = Symbol::parse_symbol(is);
  Access::conrep crep;
  lty t;
  is >> char_<','> >> crep >> char_<','> >> t >> char_<')'>;
  return {sym, crep, t};
}

std::istream& operator>>(std::istream& is, con& c) {
  is >> std::ws;
  if (is.peek() == '(') {
    string s;
    getline(is, s, ')');
         if (s == "I32") {std::int32_t x; is >> x; c.emplace<INT32con>(x);}
    else if (s == "II")  {      maxint x; is >> x; c.emplace<INTINFcon>(x);}
    else if (s == "W")   {        word x; is >> x; c.emplace<WORDcon>(x);}
    else if (s == "W32") {      word32 x; is >> x; c.emplace<WORD32con>(x);}
    else on_error(is, "Invalid type in con: ", s);
  }
  else if (std::isdigit(is.peek())) {
    // TODO: handle floating point? Handle VLEN?
    int i;
    is >> i;
    c.emplace<INTcon>(i);
  }
  else if(is.peek() == '"') {
    string s;
    is >> quoted(s);
    c.emplace<STRINGcon>(s);
  }
  // Here we are certain it must be a datacon:
  else {
    auto s = Symbol::parse_symbol(is);
    is >> char_<'.'>;
    auto v = parse_var(is);
    c.emplace<DATAcon>(s, v);
  }
  return is;
}

static auto const no_select_xword_index = std::ios_base::xalloc();

void parse_lexp(std::istream& is, std::string_view s, lexp& exp) {
  using namespace Parser;
  if (s.empty()) {
    char c = is.get();
    if (c == '(') {
      auto s = parse_alnum_id(is);
      is >> char_<')'>;
           if (s == "W") parse_into<WORD>(exp, is, word{});
      else if (s == "W32") parse_into<WORD32>(exp, is, word32{});
      else if (s == "I32") parse_into<INT32>(exp, is, std::int32_t{});
      else if (s == "APP") // followed by a LET expression
        is >> exp;
    }
    else if (c == '"') {
      is.putback(c);
      string s;
      is >> std::quoted(s);
      exp.emplace<STRING>(s);
    }
    else {
      std::string digits;

      auto parse_remaining_digits = [&] {
        char c;
        while (std::isdigit(c = is.get()))
          digits += c;
        is.putback(c);
      };

      if (c == '~')
        digits += '-';
      else
        is.putback(c);

      parse_remaining_digits();
      if (is.peek() == '.') {
        digits += '.';
        is.ignore();
        parse_remaining_digits();
        exp.emplace<REAL>(std::stod(digits));
      }
      else if (is.peek() == 'e') {
        digits += 'e';
        is.ignore();
        if (is.peek() == '~') {
          digits += '-';
          is.ignore();
        }
        parse_remaining_digits();
        exp.emplace<REAL>(std::stod(digits));
      }
      else
        exp.emplace<INT>(std::stoi(digits));
    }
  }
  else if (s[0] == 'v') { // VAR or LET
    int i = std::stoi(std::string{s.substr(1)});
    if ((is >> std::ws).peek() == '=') { // check whether this is a LET expression
      is.ignore();
      lexp a, b;
      if (!(is >> a))
        on_error(is, "Failed parsing first LET expression for ", s);
      else if (!(is >> b))
        on_error(is, "Failed parsing second LET expression for ", s);
      else
        exp.emplace<LET>(i, a, b);
    }
    else
      exp.emplace<VAR>(i);
  }
  else if (s == "PRM")
    parse_into<PRIM>(exp, is, '(', Primop::primop{}, ',', lty{}, ',', vector<tyc>{}, ')');
  else if (s == "FN")
    parse_into<FN>(exp, is, '(', var_tag<lvar>{}, ':', lty{}, ',', lexp{}, ')');
  else if (s == "FIX") {
    is >> char_<'('>;
    vector<tuple<lvar, lty, lexp>> v;
    do {
      v.emplace_back();
      std::get<lvar>(v.back()) = parse_var(is);
      is >> char_<':'> >> std::get<lty>(v.back())
         >> string_{"::"} >> std::get<lexp>(v.back()) >> std::ws;
    } while(is.peek() == 'v');
    lexp l;
    is >> string_{"IN"} >> l >> char_<')'>;
    exp.emplace<FIX>(move(v), l);
  }
  else if (s == "APP")
    parse_into<APP>(exp, is, '(', lexp{}, ',', lexp{}, ')');
  else if (s == "RCD")
    exp.emplace<RECORD>(parse_sequence<lexp, '(', ',', ')'>(is));
  else if (s == "SRCD") {
    auto v = parse_sequence<lexp, '(', ',', ')'>(is);
    exp.emplace<SRECORD>(v);
  }
  else if (s == "RAISE") {
    auto tup = parse(is, '(', lty{}, ',', lexp{}, ')');
    exp.emplace<RAISE>(std::get<1>(tup), std::get<0>(tup));
  }
  else if (s == "CON") {
    is >> char_<'('>;
    auto dc = parse_dataconstr(is);
    is >> char_<','>;
    auto v = parse_list<tyc>(is);
    lexp l;
    is >> char_<','> >> l >> char_<')'>;
    exp.emplace<CON>(dc, move(v), l);
  }
  else if (s == "TFN") {
    is >> char_<'('>;
    auto v = parse_sequence<tkind, '(', ',', ')'>(is);
    lexp l;
    is >> l >> char_<')'>;
    exp.emplace<TFN>(move(v), l);
  }
  else if (s == "TAPP")
    parse_into<TAPP>(exp, is, '(', no_select, lexp{}, vector<tyc>{}, ')');
  else if (starts_with(s, "SWI")) {
    lexp l;
    parse_lexp(is, s.substr(3), l);
    is >> string_{"of"} >> char_<'('>;
    vector<pair<con, lexp>> v;
    do {
      con c;
      lexp e;
      is >> c >> string_{"=>"} >> e >> std::ws;
      v.emplace_back(c, e);
    } while(is.peek() != ')' && is.peek() != '_');
    optional<dlexp> default_exp;
    if (is.peek() == '_')
      is.ignore() >> string_{"=>"} >> default_exp.emplace();
    is >> char_<')'>;
    exp.emplace<SWITCH>(l, move(v), default_exp);
  }
  else
    on_error(is, "lexp parser unknown symbol ", s);

  if (!is.iword(no_select_xword_index)) {
    is >> std::ws;
    if (is.peek() == '[') {
      auto v = parse_list<lvar>(is);
      exp.emplace<SELECT>(move(v), dlexp{std::move(exp)});
    }
  }
  else
    is.iword(no_select_xword_index) = false;
}

std::istream& operator>>(std::istream& is, lexp& lexp) {
  parse_lexp(is, Parser::parse_alnum_id(is), lexp);
  return is;
}

std::istream& no_select(std::istream& is) {
  is.iword(no_select_xword_index) = true;
  return is;
}

}
