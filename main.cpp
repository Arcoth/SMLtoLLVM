#include "PLambda.hpp"
#include "ParsePLambda.hpp"

#include <iostream>
#include <regex>

#include <boost/process.hpp>

namespace bp = boost::process;

int main()
{
    bp::ipstream pipe_stream;
    bp::opstream stream;
    bp::child sml("sml @SMLload=smlnj/base/system/sml Test.sml", bp::std_in < stream, bp::std_out > pipe_stream);

    std::string line, absyn;
    for (bool ignore = true; pipe_stream && std::getline(pipe_stream, line);)
      if (!ignore)
        absyn = absyn + line + '\n';
      else if (std::regex_match(line, std::regex{R"(\[opening .+\])"}))
        ignore = false;

    std::cout << absyn << std::endl;

    using namespace boost::spirit;
    std::cout << std::boolalpha
              << qi::phrase_parse(begin(absyn), end(absyn), SMLNJInterface::Parser::plambda_parser<std::string::iterator>{}, ascii::space);

    sml.wait();
}
