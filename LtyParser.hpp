#pragma once

#include "Lty.hpp"

#include <boost/spirit/include/qi.hpp>

namespace SMLNJInterface::Parser {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

struct primtyc_grammar : qi::symbols<char, decltype(PrimTyc::PT_INT31)> {

  primtyc_grammar() {
    using namespace PrimTyc;
    this->add("I31", PT_INT31);
    this->add("I32", PT_INT32);
    this->add("F64", PT_REAL);
    this->add("STR", PT_STRING);
    this->add("EXN", PT_EXN);
    this->add("ARR", PT_ARRAY);
    this->add("VEC", PT_VECTOR);
    this->add("REF", PT_REF);
    this->add("CONT", PT_CONT);
    this->add("CCONT", PT_CCONT);
    this->add("FUN", PT_ARROW);
    this->add("OBJ", PT_OBJ);
    this->add("CFN", PT_CFUN);
    this->add("BARR", PT_BARRAY);
    this->add("RARR", PT_RARRAY);
    this->add("SLCK", PT_SLOCK);
    this->add("INF", PT_INTINF);
    this->add("ETG", PT_ETAG);
    this->add("VOID", PT_VOID);
  }

};

template <typename Iterator, typename Rule>
inline const Rule list = qi::lit('[') >> *(Rule{} % ',') >> qi::lit(']');

template <typename Iterator>
inline const qi::grammar<Iterator, LambdaVar::lvar, ascii::space_type> lvar_grammar = 'v' >> qi::uint_;

template <typename Iterator>
struct tkind_parser : qi::grammar<Iterator, Lty::tkind, ascii::space_type> {
  tkind_parser() : tkind_parser::base_type(start)
  {
    using namespace qi;
    start = lit("M") | lit("B")
          | lit('(') >> list<Iterator, tkind_parser> >> lit ("=>") >> tkind_parser{} >> lit(')')
          | lit("KSEQ") >> list<Iterator, tkind_parser>;
  }
  qi::rule<Iterator, Lty::tkind, ascii::space_type> start;
};


template <typename Iterator>
struct tyc_parser : qi::grammar<Iterator, Lty::tyc, ascii::space_type> {
  tyc_parser() : tyc_parser::base_type(start)
  {
    using namespace qi;
    start = "TV"   >> lit('(') >> uint_ >> lit(',') >> uint_ >> lit(')')
          | "NTV"  >> lit('(') >> lvar_grammar<Iterator>  >> lit(')')
          | "PRIM" >> lit('(') >> primtyc_grammar{} >> lit(')')
          | "TCFN" >> lit('(') >> list<Iterator, tkind_parser> >> ',' >> tyc_parser{} >> lit(')')
          | "TCAP" >> lit('(') >> tyc_parser{} >> ',' >> list<Iterator, tyc_parser> >> lit(')')
          | "SEQ"  >> lit('(') >> list<Iterator, tyc_parser> >> lit(')')
          | "PROJ" >> lit('(') >> tyc_parser{} >> ',' >> int_ >> ')'
          ;
  }
  qi::rule<Iterator, Lty::tyc, ascii::space_type> start;
};


template <typename Iterator>
struct lty_parser : qi::grammar<Iterator, Lty::lty, ascii::space_type> {
  lty_parser() : lty_parser::base_type(start)
  {
    using namespace qi;
    start =  "TYC" >> lit('(') >> tyc_parser<Iterator>{} >> lit(')')
           | "STR" >> lit('(') >> list<Iterator, lty_parser> >> lit(')')
        // | "FCT" >> lit('(') >>  >> lit(')')
           | "POL" >> lit('(') >> list<Iterator, tkind_parser> >> lit(',') >> list<Iterator, tyc_parser> >> lit(')')
        // | "CONT" >> lit('(') >>  >> lit(')')
        // | "IND" >> lit('(') >>  >> lit(')')
        // | "LT_ENV" >> lit('(') >>  >> lit(')')
        ;
  }

  qi::rule<Iterator, Lty::lty, ascii::space_type> start;
};

}
