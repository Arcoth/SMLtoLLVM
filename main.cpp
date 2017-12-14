#include "SMLNJInterface/ParserUtilities.hpp"
#include "Compiler/compile.hpp"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <boost/process.hpp>

#include <iostream>

namespace bp = boost::process;

int main() try
{
  using namespace SMLNJInterface;

  bp::ipstream pipe_stream;
  bp::child sml("sml @SMLload=smlnj/base/system/sml Test.sml", bp::std_out > pipe_stream);

  std::string line, absyn;
  for (bool ignore = true; pipe_stream && std::getline(pipe_stream, line);)
    if (!ignore)
      absyn = absyn + line + '\n';
    else {
      if (line == "PLambda:")
        ignore = false;
      std::cout << "> " << line << '\n';
    }
  sml.wait();

  std::cout << absyn << std::endl;

  std::istringstream stream(absyn);
  PLambda::lexp plambda;
  if (!(stream >> plambda)) {
    std::cerr << "Failed to parse the plambda expression!";
    return 0;
  }

  using namespace SMLCompiler;
  using namespace llvm;

  SMLTranslationUnit unit(plambda);

  LLVMContext context;
  Module module("SML default module", context);

  for (auto& [var, name] : unit.exportedDecls) {
    auto s = Parser::parse_identifier(stream);
    if (!stream) {
      std::cerr << "Couldn't extract names of entities!";
      return 0;
    }
    if (s == "fun" || s == "val")
      name = Parser::parse_identifier(stream);
    else if (s != "datatype") {
      std::cerr << "Unrecognised entity kind: " << s;
      return 0;
    }
    stream.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  }

  for (auto& [var, exp] : unit.globalDecls)
    std::cout << var << ", freevars = " << freeVars(exp) << "\n";
  std::cout << "Exporting " << unit.exportedDecls << '\n';

  // Invoke the compiler.
  SMLCompiler::compile_top(unit, module);

  // Print out all of the generated code.
  module.print(errs(), nullptr);
} catch (SMLCompiler::CompileFailException const& e) {
  std::cerr << "Compilation failed: " << e.what();
} catch (SMLCompiler::UnsupportedException const& e) {
  std::cerr << "Unsupported: " << e.what();
} catch (std::exception const& e) {
  std::cerr << "Exception: " << e.what();
} catch (...) {
  std::cerr << "An unknown exception interrupted the program.";
}
