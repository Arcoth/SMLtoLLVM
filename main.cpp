#include "SMLNJInterface/ParserUtilities.hpp"
#include "Compiler/compile.hpp"

#include "BinaryEmission.h"
#include "JITExecution.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>

#include <boost/process.hpp>

#include <iostream>

using namespace SMLNJInterface;
using namespace SMLCompiler;

ImportsVector parseImports(std::istream& is) {
  for (std::string line; std::getline(is, line) && line != " the current import tree is :";)
    ;
  if (!is.good())
    throw std::runtime_error{"No import tree in stream!"};

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
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "::"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getTrue(context)};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "nil"}]
    = {.type = symbol_rep::UNTAGGED, .value = ConstantInt::getFalse(context)};

  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "true"}]
    = {.type = symbol_rep::CONSTANT, .value = cast<ConstantInt>(ConstantInt::get(tagType(module), 1))};
  unit.symbolRepresentation[Symbol::symbol{Symbol::varInt, "false"}]
    = {.type = symbol_rep::CONSTANT, .value = cast<ConstantInt>(ConstantInt::get(tagType(module), 0))};
}

void parseEntityData(SMLTranslationUnit& unit, std::istream& stream) {
  for (auto& [var, name] : unit.exportedDecls) {
    static auto skip_to = [&] (std::istream& s, char c) {
      s.ignore(std::numeric_limits<std::streamsize>::max(), c);
    };
    bool name_already_parsed = false;
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
            = {.type = symbol_rep::TAGGED, ConstantInt::get(tagType(*unit.module), debruijn++)  };
          std::cout << "New constructor: " << name << '\n';
          skip_to(ss, '|');
        }
      }
      else if (kind == "structure"
            || kind == "signature"
            || kind == "functor") {
        name = Parser::parse_symbol_id(stream);
        name_already_parsed = true;
        for (std::string str; stream >> str && str != "end";)
          ;
        break;
      }
      skip_to(stream, '\n');
    }
    if (!name_already_parsed)
      name = Parser::parse_symbol_id(stream);
    skip_to(stream, '\n');
  }
}

SMLTranslationUnit parseSMLFile(std::string const& fileName) {
  namespace bp = boost::process;

  bp::ipstream output;
  bp::opstream input;
  bp::child sml("smlnj/base/system/testml sml " + fileName,
                bp::std_out > output, bp::std_in < input);
  std::istream stream(new Parser::log_input_buf(output.rdbuf(), std::cout));

  auto importMap = parseImports(stream);
  if (!stream)
    throw std::runtime_error{"parseSMLFile: couldn't parse imports"};

  auto plambda = parseVerbosePlambda(stream);
  if (stream.eof())
    throw std::runtime_error{"parseSMLFile: No information on exports?"};
  if (!stream)
    throw std::runtime_error{"parseSMLFile: couldn't parse PLambda"};

  using namespace llvm;

  SMLTranslationUnit unit(plambda, std::move(importMap),
                          new Module("SML module", context));

  parseEntityData(unit, stream);

  return unit;
}

template <typename Map>
std::size_t getMainIndex(Map const& exportedDecls) {
  std::set<std::string> functionNames;
  for (auto& [_, n] : exportedDecls)
    functionNames.insert(n);

  auto decl_iter = functionNames.find("main");
  if (decl_iter == functionNames.end())
    throw CompileFailException{"No main function in SML program!"};

  return std::distance(functionNames.begin(), decl_iter);
}

int main(int argc, char** argv) try
{
  if (argc < 2) {
    std::cout << "Usage: SMLToLLVM <File>\n";
    return EXIT_SUCCESS;
  }

  SMLTranslationUnit unit = parseSMLFile(argv[1]);
  addSymbols(unit); // Add the core symbols (false, true, etc.)

  auto module = unit.module;

  auto write = [&] (char const* extension) {
    std::string str = argv[1];
    str += "-";
    str += extension;
    str += ".llvm";
    writeToFile(*module, str.c_str());
  };


  // Invoke the compiler.
  addGCSymbols(*module);
  SMLCompiler::compile_top(unit);

  write("pre-opt");

  SMLCompiler::performOptimisationPasses(*module);

  write("post-opt");

  SMLCompiler::performStatepointsPass(*module);

  write("post-statepoints");


  if (verifyModule(*module, &errs())) {
    errs() << "\nThe code is ill-formed!\n\n";
    return EXIT_SUCCESS;
  }

  std::cout << "Exporting " << unit.exportedDecls << '\n';

  if (verifyModule(*module, &errs())) {
    errs() << "The code is ill-formed!";
    return EXIT_SUCCESS;
  }

  // Execute the main function
  return execute(getMainIndex(unit.exportedDecls), unit);
}
  catch (SMLCompiler::CompileFailException const& e) {
  std::cerr << "\n\nCompilation failed: " << e.what();
} catch (SMLCompiler::UnsupportedException const& e) {
  std::cerr << "\n\nUnsupported feature necessitated: " << e.what();
} catch (std::exception const& e) {
  std::cerr << "\n\nException: " << e.what();
  return EXIT_FAILURE;
} catch (...) {
  std::cerr << "\n\nAn unknown exception interrupted the program.";
  return EXIT_FAILURE;
}
