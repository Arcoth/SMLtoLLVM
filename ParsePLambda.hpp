#pragma once

#include "PLambda.hpp"

#include "LtyParser.hpp"

namespace SMLNJInterface::Parser {

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

template <typename Iterator>
struct plambda_parser : qi::grammar<Iterator, PLambda::lexp, ascii::space_type> {
  plambda_parser() : plambda_parser::base_type(start) {
    using namespace qi;
    start = lvar_grammar<Iterator>
          | int_
          | "RCD" >> lit('(') >> (plambda_parser{} % ',') >> lit(')')
          | "SRCD">> lit('(') >> (plambda_parser{} % ',') >> lit(')')
      //  | "PRM" >> lit('(') >>  >> ',' >> list<Iterator, tyc_parser> >> lit(')')
          | "FN"  >> lit('(') >> lvar_grammar<Iterator> >> ':' >> lty_parser<Iterator>{} >> ',' >> plambda_parser{} >> ')'
          | lvar_grammar<Iterator> >> '=' >> plambda_parser{} >> plambda_parser{}
          ;
  }
  qi::rule<Iterator, PLambda::lexp, ascii::space_type> start;
};

}
