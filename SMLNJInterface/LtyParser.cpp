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
  using namespace Parser;

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

  std::istream& operator>>(std::istream& is, tyc& t) {
    using namespace Parser;
    auto s = parse_identifier(is);
    if (s == "TV")
      parse_into<TC_VAR>(t, is, '(', DebIndex::index{}, ',', int{}, ')');
    else if (s == "NTV")
      parse_into<TC_NVAR>(t, is, '(', var_tag<unsigned>{}, ')');
    else if (s == "PRIM") {
      is >> char_<'('>;
      auto s = parse_identifier(is);
      t.emplace<TC_PRIM>(PrimTyc::primtyc_names.at(s));
      is >> char_<')'>;
    }
    else if (s == "TCFN")
      parse_into<TC_FN>(t, is, '(', vector<tkind>{}, ',', tyc{}, ')');
    else if (s == "TCAP")
      parse_into<TC_APP>(t, is, '(', tyc{}, ',', vector<tyc>{}, ')');
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

      t.emplace<TC_ARROW>(std::tuple_cat(std::tuple{flag},
                                        parse(is, '(', vector<tyc>{}, ',', vector<tyc>{}, ')')));
    }
    else if (s == "FIX") {
      is >> char_<'('>;
      unsigned s, i;
      tyc t;
      is >> string_{"size ="}   >> s
         >> string_{"index ="}  >> i
         >> string_{"gen ="}    >> t
         >> string_{"prms ="};
      auto v = parse_list<tyc>(is);
      is >> char_<')'>;
      emplace_by_value<TC_FIX>(t, {.family = {.size = s, .gen = t, .params = v}, .index = i});
    }
    else if (s == "SUM")
      parse_into<TC_SUM>(t, is, '(', vector<tyc>{}, ')');
    else if (s.empty() && is.peek() == '{') {
      auto v = parse_sequence<struct tyc, '{', ',', '}'>(is);
      t.emplace<TC_TUPLE>(v);
    }
    return is;
  }

  std::istream& operator>>(std::istream& is, lty& l) {
    using namespace Parser;
    auto s = parse_identifier(is);
    if (s == "TYC")
      parse_into<LT_TYC>(l, is, '(', tyc{}, ')');
    else if (s == "STR")
      parse_into<LT_STR>(l, is, '(', vector<lty>{}, ')');
    else if (s == "POL")
      parse_into<LT_POLY>(l, is, '(', vector<tkind>{}, ',', vector<lty>{}, ')');
    else
      on_error(is, "lty parser unknown name ", s);
    return is;
  }
}
