#include "ParserUtilities.hpp"

#include "Types.hpp"

namespace SMLNJInterface::Symbol {

symbol parse_symbol(std::istream& is) {
  is >> std::ws;
  std::string s;
  getline(is, s, '$');
  auto name = Parser::parse_symbol_id(is);
  switch(fnv(s)) {
    case "VAL"_fnv: return symbol{varInt, name};
    case "SIG"_fnv: return symbol{sigInt, name};
    case "STR"_fnv: return symbol{strInt, name};
    case "FSIG"_fnv: return symbol{fsignInt, name};
    case "FCT"_fnv: return symbol{fctInt, name};
    case "TYC"_fnv: return symbol{tycInt, name};
    case "LAB"_fnv: return symbol{labInt, name};
    case "TYV"_fnv: return symbol{tyvInt, name};
    case "FIX"_fnv: return symbol{fixInt, name};
    default:
      Parser::on_error(is, "Invalid symbol type: ", s);
      return {};
  }
}

}

namespace SMLNJInterface::Access {

using namespace Parser;

access parse_access(std::string_view name, std::istream& is);

std::istream& operator>>(std::istream& is, access& a) {
  a = parse_access(parse_alnum_id(is), is);
  return is;
}

access parse_access(std::string_view name, std::istream& is) {
  switch(fnv(name)) {
    case "LVAR"_fnv:
      return access(std::in_place_index<LVAR>, parenthesized<lvar>(is));

    case "PATH"_fnv: {
      int i;
      access a;
      is >> char_<'('> >> i >> char_<','> >> a >> char_<')'>;
      return access(std::in_place_index<PATH>, a, i);
    }
    case "EXTERN"_fnv:
      throw;
    case "NO_ACCESS"_fnv:
      return access(std::in_place_index<NO_ACCESS>);
    default:
      on_error(is, "Invalid access: ", name);
      return {};
  }
}

std::istream& operator>>(std::istream& is, conrep& crep) {
  auto id = parse_alnum_id(is);
  switch(fnv(id)) {
    case "UT"_fnv: crep.emplace<UNTAGGED>(); break;
    case "TG"_fnv: crep.emplace<TAGGED>(parenthesized<int>(is)); break;
    case "TN"_fnv: crep.emplace<TRANSPARENT>(); break;
    case "CN"_fnv: crep.emplace<CONSTANT>(parenthesized<int>(is)); break;
    case "RF"_fnv: crep.emplace<REF>(); break;
    case "LC"_fnv: crep.emplace<LISTCONS>(); break;
    case "LN"_fnv: crep.emplace<LISTNIL>(); break;
    case "SS"_fnv: crep.emplace<SUSP>(); break;
    default:
      if (starts_with(id, "EXN"))
        crep.emplace<EXN>(parse_access(id.substr(3), is));
      else
        on_error(is, "Invalid conrep: ", id);
  }
  return is;
}

}
