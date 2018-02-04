#include "SMLNJInterface/ParserUtilities.hpp"
#include "Compiler/compile.hpp"

#include "REPL.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>

#include <boost/process.hpp>

#include <iostream>

auto parseVerbosePlambda(std::istream& is) {
  for (std::string line; std::getline(is, line) && line != "PLambda:";)
    ;

  SMLCompiler::PLambda::lexp e;
  is >> e;
  return e;
}

int main(int argc, char** argv) try
{
  if (argc < 2) {
    std::cout << "Pass a file to compile.";
    return EXIT_SUCCESS;
  }

  using namespace SMLNJInterface;
  using namespace SMLCompiler;
  namespace bp = boost::process;

  bp::ipstream output;
  bp::opstream input;
  bp::child sml("sml @SMLload=smlnj/base/system/sml " + std::string{argv[1]},
                bp::std_out > output, bp::std_in < input);
  std::istream stream(new Parser::log_input_buf(output.rdbuf(), std::cout));

  // stream.exceptions(std::ios::failbit);
  auto plambda = parseVerbosePlambda(stream);
  if (!stream) {
    std::cerr << "Failed to parse the plambda expression!";
    return 0;
  }

  using namespace llvm;

  LLVMContext context;
  auto module = std::make_unique<Module>("SML default module", context);

  SMLTranslationUnit unit(plambda);
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "::"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getTrue(context)};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "nil"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getFalse(context)};

  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "true"}]
    = {.type = symbol_rep::CONSTANT, .value = ConstantInt::getTrue(context)};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "false"}]
    = {.type = symbol_rep::CONSTANT, .value = ConstantInt::getFalse(context)};

  for (auto& [var, name] : unit.exportedDecls) {
    static auto skip_to = [&] (std::istream& s, char c) {
      s.ignore(std::numeric_limits<std::streamsize>::max(), c);
    };
    for (;;) {
      auto kind = Parser::parse_alnum_id(stream);
      if (kind == "fun" || kind == "val")
        break;
      if (kind == "datatype") {
        unsigned debruijn = 0;
        skip_to(stream, '=');
        std::string s; getline(stream, s);
        stream.putback('\n');
        for (std::istringstream ss(s);;) {
          auto name = Parser::parse_symbol_id(ss);
          if (!ss)
            break;
          unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, name}]
            = {.type = symbol_rep::TAGGED, ConstantInt::get(genericIntType(*module), debruijn++)};
          std::cout << "New constructor: " << name << '\n';
          skip_to(ss, '|');
        }
      }
      skip_to(stream, '\n');
    }
    name = Parser::parse_symbol_id(stream);
    skip_to(stream, '\n');
  }

  std::cout << "Exporting " << unit.exportedDecls << '\n';

  // Invoke the compiler.
  addGCSymbols(*module);
  SMLCompiler::compile_top(unit, *module);
  SMLCompiler::performPasses(*module);

  // Print out all of the generated code.
  module->print(outs(), nullptr);

  if (verifyModule(*module, &errs())) {
    errs() << "The code is ill-formed!";
    return EXIT_SUCCESS;
  }

  return execute(module.release());
} catch (SMLCompiler::CompileFailException const& e) {
  std::cerr << "Compilation failed: " << e.what();
} catch (SMLCompiler::UnsupportedException const& e) {
  std::cerr << "Unsupported: " << e.what();
} catch (std::exception const& e) {
  std::cerr << "Exception: " << e.what();
  return EXIT_FAILURE;
} catch (...) {
  std::cerr << "An unknown exception interrupted the program.";
  return EXIT_FAILURE;
}
