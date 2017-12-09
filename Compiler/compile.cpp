#include "compile.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>

#include <boost/lexical_cast.hpp>

namespace SMLCompiler {

using std::get;
using std::get_if;

using namespace PLambda;

std::set<lvar> freeVars( vector<lexp> const& exps ) {
  std::set<lvar> s;
  for (auto& e : exps)
    s = set_union(s, freeVars(e));
  return s;
}

std::set<lvar> freeVars( lexp const& exp ) {
  switch(exp.index()) {
    case INT: case INT32: case WORD:
    case WORD32: case REAL: case PRIM:
    case STRING: case GENOP:
      return {};

    case VAR: return {get<VAR>(exp)};

    case FN: {
      auto& fn = get<FN>(exp);
      return set_subtract(freeVars(get<dlexp>(fn)), {get<lvar>(fn)});
    }
    case FIX: {
      auto& [decls, in] = get<FIX>(exp);
      std::set<lvar> freevars, params;
      for (auto& match : decls) {
        freevars = set_union(freevars, freeVars(lexp{std::in_place_index<FN>, match}));
        params.insert(get<lvar>(match));
      }
      return set_union(freevars, set_subtract(freeVars(in), params));
    }
    case APP: {
      auto& [fun, arg] = get<APP>(exp);
      return set_union(freeVars(fun), freeVars(arg));
    }
    case LET: {
      auto& [var, assign, in] = get<LET>(exp);
      return set_union(freeVars(assign), set_subtract(freeVars(in), {var}));
    }
    case SWITCH: {
      auto& [arg, matches, default_] = get<SWITCH>(exp);
      auto s = freeVars(arg);
      for (auto& [constructor, target] : matches) {
        auto exp_vars = freeVars(target);
        if (constructor.index() == DATAcon)
          exp_vars = set_subtract(exp_vars, {get<lvar>(get<DATAcon>(constructor))});
        s = set_union(s, exp_vars);
      }
      return s;
    }

    // Simple compositional cases:

    case RAISE:  return freeVars(get<dlexp>( get<RAISE>(exp)));
    case ETAG:   return freeVars(get<dlexp>(  get<ETAG>(exp)));
    case SELECT: return freeVars(get<dlexp>(get<SELECT>(exp)));
    case PACK:   return freeVars(get<dlexp>(  get<PACK>(exp)));
    case WRAP:   return freeVars(get<dlexp>(  get<WRAP>(exp)));
    case UNWRAP: return freeVars(get<dlexp>(get<UNWRAP>(exp)));
    case CON:    return freeVars(get<dlexp>(   get<CON>(exp)));

    case RECORD:  return freeVars( get<RECORD>(exp));
    case SRECORD: return freeVars(get<SRECORD>(exp));
    case VECTOR: return freeVars(get<0>(get<VECTOR>(exp)));


    default:
      throw UnsupportedException{"Unsupported lambda type " + boost::lexical_cast<string>(exp.index())};
  }
}

SMLTranslationUnit::SMLTranslationUnit(lexp const& exp) {
  auto* top_fn = get_if<FN>(&exp);
  if (!top_fn)
    throw CompileFailException{"getGlobalDeclarations: Argument not a function!"};

  lexp body = get<2>(get<LET>(get<dlexp>(*top_fn).get()));

  // The body should not depend on the structure argument.
  if (!freeVars(body).empty())
    throw CompileFailException{"getGlobalDeclarations: Argument not a pure function over structures!"};

  while (body.index() == LET) {
    auto& [var, assign, value] = get<LET>(body);
    globalDecls.emplace_back(var, assign);
    body = std::move(value).get();
  }
  if (body.index() != SRECORD)
    throw CompileFailException{"getGlobalDeclarations: Function doesn't yield structure!"};
  auto& params = get<SRECORD>(body);
  std::transform(begin(params), end(params), std::back_inserter(exportedDecls),
                 [] (auto& e) {
    if (e.index() != VAR)
      throw CompileFailException{"getGlobalDeclarations: Function doesn't yield structure of variables!"};
    return get<VAR>(e);
  });
}

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(lexp const& expression,
                 llvm::Module& module) {
  llvm::IRBuilder<> builder(module.getContext());

}

}

