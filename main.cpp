#include "SMLNJInterface/ParserUtilities.hpp"
#include "Compiler/compile.hpp"

#include "BinaryEmission.h"
#include "JITExecution.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>

#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>

#include <boost/process.hpp>

#include <iostream>

using namespace SMLNJInterface;
using namespace SMLCompiler;

ImportsVector parseImports(std::istream& is) {
  for (std::string line; std::getline(is, line) && line != " the current import tree is :";)
    ;

  ImportsVector tree;
  for (std::string pid_label; (is >> pid_label) && pid_label == "Pid";) {
    boost::container::string pid;
    is >> pid;

    // While the next character is a digit...
    for (std::string line; std::isdigit((is >> std::ws).peek()) && getline(is, line);) {
      std::istringstream line_stream(line);
      tree.emplace_back(pid, boost::container::vector<int>{});
      for (int x; line_stream >> x;)
        tree.back().second.emplace_back(x);
    }
  }
  return tree;
}

auto parseVerbosePlambda(std::istream& is) {
  for (std::string line; std::getline(is, line) && line != "PLambda:";)
    ;

  PLambda::lexp e;
  is >> e;
  return e;
}

void addSymbols(SMLTranslationUnit& unit) {
  auto& module = *unit.module;
  auto& context = module.getContext();
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "::"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getTrue(context)};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "nil"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getFalse(context)};

  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "true"}]
    = {.type = symbol_rep::CONSTANT, .value = cast<ConstantInt>(ConstantInt::get(tagType(module), 1))};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "false"}]
    = {.type = symbol_rep::CONSTANT, .value = cast<ConstantInt>(ConstantInt::get(tagType(module), 0))};
}



int main(int argc, char** argv) try
{
  if (argc < 2) {
    std::cout << "Pass a file to compile.";
    return EXIT_SUCCESS;
  }

  namespace bp = boost::process;

  bp::ipstream output;
  bp::opstream input;
  bp::child sml("smlnj/base/system/testml sml " + std::string{argv[1]},
                bp::std_out > output, bp::std_in < input);
  std::istream stream(new Parser::log_input_buf(output.rdbuf(), std::cout));

  auto importMap = parseImports(stream);

  // stream.exceptions(std::ios::failbit);
  auto plambda = parseVerbosePlambda(stream);
  if (!stream) {
    std::cerr << "Failed to parse the plambda expression!";
    return 0;
  }

  using namespace llvm;

  LLVMContext context;
  SMLTranslationUnit unit(plambda, std::move(importMap),
                          new Module("SML module", context));
  auto module = unit.module;
  addSymbols(unit);

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
            = {.type = symbol_rep::TAGGED, ConstantInt::get(tagType(*module), debruijn++)  };
          std::cout << "New constructor: " << name << '\n';
          skip_to(ss, '|');
        }
      }
      skip_to(stream, '\n');
    }
    name = Parser::parse_symbol_id(stream);
    skip_to(stream, '\n');
  }

  // Invoke the compiler.
  addGCSymbols(*module);
  SMLCompiler::compile_top(unit);

  // Print out all of the generated code.
  outs() << "\n\n\n\nPRINTING LLVM MODULE CONTENTS:\n\n";
  module->print(outs(), nullptr);

  SMLCompiler::performOptimisationPasses(*module);

  if (verifyModule(*module, &errs())) {
    errs() << "\nThe code is ill-formed!\n\n";
    return EXIT_SUCCESS;
  }

  SMLCompiler::performStatepointsPass(*module);

  std::cout << "Exporting " << unit.exportedDecls << '\n';

  if (verifyModule(*module, &errs())) {
    errs() << "The code is ill-formed!";
    return EXIT_SUCCESS;
  }

  std::set<std::string> functionNames;
  for (auto& [_, n] : unit.exportedDecls)
    functionNames.insert(n);

  auto decl_iter = functionNames.find("main");
  if (decl_iter == functionNames.end()) {
    std::cerr << "\nNo main function in SML program!";
    return EXIT_SUCCESS;
  }

  auto main_fun_index = std::distance(functionNames.begin(),
                                      decl_iter);

  return execute(main_fun_index, unit);
}
  catch (SMLCompiler::CompileFailException const& e) {
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
