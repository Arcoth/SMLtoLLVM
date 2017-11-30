#include "Lty.hpp"
#include "ParserUtilities.hpp"


namespace SMLNJInterface::PrimTyc {

const std::unordered_map<std::string_view, PrimTyc::primtyc> primtyc_names{
    {"I31", PT_INT31},
    {"I32", PT_INT32},
    {"F64", PT_REAL},
    {"STR", PT_STRING},
    {"EXN", PT_EXN},
    {"ARR", PT_ARRAY},
    {"VEC", PT_VECTOR},
    {"REF", PT_REF},
    {"CONT", PT_CONT},
    {"CCONT", PT_CCONT},
    {"FUN", PT_ARROW},
    {"OBJ", PT_OBJ},
    {"CFN", PT_CFUN},
    {"BARR", PT_BARRAY},
    {"RARR", PT_RARRAY},
    {"SLCK", PT_SLOCK},
    {"INF", PT_INTINF},
    {"ETG", PT_ETAG},
    {"VOID", PT_VOID}
};

}


namespace SMLNJInterface::Lty
{

  std::istream& operator>>(std::istream& is, tkind& tk) {
    using namespace Parser;
    auto s = parse_identifier(is);
    if (s == "M")
      tk.emplace<TK_MONO>();
    else if (s == "B")
      tk.emplace<TK_BOX>();
    else if (s == "") {
      auto vec = parse_sequence<tkind, '(', ',', '='>(is);
      tkind t;
      is >> char_<'>'> >> t;
      tk.emplace<TK_FUN>(move(vec), move(t));
    }
    else if (s == "KSEQ")
      tk.emplace<TK_SEQ>(parse_nonempty_sequence<tkind, ','>(is));
    else
      on_error(is, "tkind parser unknown symbol ", s);
    return is;
  }

  std::istream& operator>>(std::istream& is, tyc& tyc) {
    using namespace Parser;
    auto s = parse_identifier(is);
    if (s == "TV") {
      DebIndex::index i;
      int j;
      is >> char_<'('> >> i >> char_<','> >> j >> char_<')'>;
      tyc.emplace<TC_VAR>(i, j);
    }
    else if (s == "NTV") {
      is >> char_<'('>;
      tyc.emplace<TC_NVAR>(parse_var(is));
      is >> char_<')'>;
    }
    else if (s == "PRIM") {
      is >> char_<'('>;
      auto s = parse_identifier(is);
      tyc.emplace<TC_PRIM>(PrimTyc::primtyc_names.at(s));
      is >> char_<')'>;
    }
    else if (s == "TCFN") {
      is >> char_<'('>;
      auto v = parse_list<tkind>(is);
      struct tyc t;
      is >> char_<','> >> t >> char_<')'>;
      tyc.emplace<TC_FN>(v, t);
    }
    else if (s == "TCAP") {
      struct tyc t;
      is >> char_<'('> >> t >> char_<','>;
      auto v = parse_list<struct tyc>(is);
      is >> char_<')'>;
      tyc.emplace<TC_APP>(t, v);
    }
    else if (s == "AR") {
      is >> std::ws;
      fflag flag;
      if (is.peek() == '[') {
        char l, r;
        is.ignore() >> l >> r >> char_<']'>;
        if ((l != 'r' && l != 'c')
         || (r != 'r' && r != 'c'))
          return on_error(is, "Invalid fflag's on arrow tyc: ", l, r);

        flag.emplace<FF_VAR>(l == 'r', r == 'r');
      }
      else
        flag.emplace<FF_FIXED>();

      is >> char_<'('>;
      auto v = parse_list<struct tyc>(is);
      is >> char_<','>;
      auto w = parse_list<struct tyc>(is);
      is >> char_<')'>;
      tyc.emplace<TC_ARROW>(flag, v, w);
    }
    else if (s.empty() && is.peek() == '{') {
      auto v = parse_sequence<struct tyc, '{', ',', '}'>(is);
      tyc.emplace<TC_TUPLE>(v);
    }
    else
      on_error(is, "tyc parser unknown symbol ", s);
    return is;
  }


  std::istream& operator>>(std::istream& is, lty& lty) {
    using namespace Parser;
    auto s = parse_identifier(is);
    if (s == "TYC")
      is >> char_<'('> >> lty.emplace<tyc>() >> char_<')'>;
    else if (s == "STR") {
      is >> char_<'('>;
      auto v = parse_list<struct lty>(is);
      is >> char_<')'>;
      lty.emplace<LT_STR>(v);
    }
    else if (s == "POL") {
      is >> char_<'('>;
      auto v1 = parse_list<tkind>(is);
      is >> char_<','>;
      auto v2 = parse_list<struct lty>(is);
      is >> char_<')'>;
      lty.emplace<LT_POLY>(v1, v2);
    }
    else
      on_error(is, "lty parser unknown symbol ", s);
    return is;
  }
}
