#include "compile.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>

#include <boost/container/static_vector.hpp>

#include <iostream>
#include <iterator>

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
    case HANDLE: return freeVars(get<0>(    get<HANDLE>(exp)));
    case ETAG:   return freeVars(get<dlexp>(  get<ETAG>(exp)));
    case SELECT: return freeVars(get<dlexp>(get<SELECT>(exp)));
    case PACK:   return freeVars(get<dlexp>(  get<PACK>(exp)));
    case WRAP:   return freeVars(get<dlexp>(  get<WRAP>(exp)));
    case UNWRAP: return freeVars(get<dlexp>(get<UNWRAP>(exp)));
    case CON:    return freeVars(get<dlexp>(   get<CON>(exp)));
    case TFN:    return freeVars(get<dlexp>(   get<TFN>(exp)));
    case TAPP:   return freeVars(get<dlexp>(  get<TAPP>(exp)));


    case RECORD:  return freeVars( get<RECORD>(exp));
    case SRECORD: return freeVars(get<SRECORD>(exp));
    case VECTOR: return freeVars(get<0>(get<VECTOR>(exp)));


    default:
      throw UnsupportedException{"Unsupported lambda type " + std::to_string(exp.index())};
  }
}

SMLTranslationUnit::SMLTranslationUnit(lexp const& exp) {
  if (exp.index() != FN)
    throw CompileFailException{"getGlobalDeclarations: Argument not a function!"};
  auto& top_fn = get<FN>(exp);

  lexp const* body = &get<2>(get<LET>(get<dlexp>(top_fn).get())).get();

  // The body should not depend on the structure argument.
  if (!freeVars(*body).empty())
    throw CompileFailException{"getGlobalDeclarations: Argument not a pure function over structures!"};

  while (body->index() == LET) {
    auto& [var, assign, value] = get<LET>(*body);
    globalDecls.emplace_back(var, assign);
    body = &value.get();
  }

  if (body->index() != SRECORD)
    throw CompileFailException{"getGlobalDeclarations: Function doesn't yield structure!"};
  auto& params = get<SRECORD>(*body);
  for (auto& e : params) {
    if (e.index() != VAR)
      throw CompileFailException{"getGlobalDeclarations: Function doesn't yield structure of variables!"};
    exportedDecls.emplace(get<VAR>(e), std::string{});
  }
}

// For now, we will employ malloc.
Value* createAllocation(Module& module, IRBuilder<>& builder, std::size_t x) {
  static const auto size_type = Type::getInt64Ty(module.getContext());
  static const auto malloc_ptr
    = cast<Function>(module.getOrInsertFunction("malloc",
                                 FunctionType::get(Type::getInt8Ty(module.getContext())->getPointerTo(),
                                                   {size_type}, false)));
  return builder.CreateCall(malloc_ptr, ConstantInt::get(size_type, x), "storage");
}

Value* compile(Module& module, IRBuilder<>& builder, lexp const& expression, std::map<PLambda::lvar, Value*> const& variables);

// Here, exp is a RCD of one or two arguments to the operator.
Value* compile_primop(Module& module, IRBuilder<>& builder,
                      Primop::primop const& op, lexp const& exp, std::map<PLambda::lvar, Value*> const& variables) {
  using namespace Primop;
  if (exp.index() != RECORD)
    throw CompileFailException{"Argument to primop is not a record"};
  boost::container::static_vector<Value*, 5> arg_values;
  for (auto& e : get<RECORD>(exp))
    arg_values.push_back(compile(module, builder, e, variables));
  auto assert_arity = [] (auto& container, std::size_t expected) {
    if (std::size(container) != expected)
      throw CompileFailException{"Wrong number of arguments to primop - " + std::to_string(std::size(container))
                               + " instead of " + std::to_string(expected)};
  };
  switch (op.index()) {
    case ARITH: {
      auto& [oper, overflow, kind] = get<ARITH>(op);
      switch (oper) {
        #define PRIMOP_BIN_I(fun) \
          assert_arity(arg_values, 2); \
          return builder.Create##fun(arg_values[0], arg_values[1]);
        case MUL: PRIMOP_BIN_I(Mul)
        case ADD: PRIMOP_BIN_I(Add)
        case SUB: PRIMOP_BIN_I(Sub)
        default:
          throw UnsupportedException{"Arithmetic operator not implemented: " + std::to_string(oper)};
      }
    }
    default:
      throw UnsupportedException{"primop " + std::to_string(op.index())};
  }
}

Value* compile(Module& module, IRBuilder<>& builder, lexp const& expression, std::map<PLambda::lvar, Value*> const& variables) {
  auto& ctx = module.getContext();
  auto make_index = [i32_t = IntegerType::get(ctx, 32)] (std::size_t s) {
    return ConstantInt::get(i32_t, s);
  };
  switch (expression.index()) {
    case VAR: {
      auto var = get<VAR>(expression);
      auto iter = variables.find(var);
      if (iter == variables.end())
        throw CompileFailException{"Variable not in scope: " + std::to_string(var)};
      return iter->second;
    }
    case FN: {
      auto& [fn_var, fn_lty, fn_body] = get<FN>(expression);
      auto free_vars = freeVars(expression);
      auto fun_type = genericFunctionType(ctx);
      // Create the hoisted function.
      auto F = Function::Create(fun_type, Function::ExternalLinkage, "lambda", &module);

      auto env_array_type = ArrayType::get(genericPointerType(ctx), free_vars.size());
      auto wrapper_type = StructType::create({genericFunctionType(ctx)->getPointerTo(),
                                              env_array_type}, F->getName().str() + "Wrapper");

      // Allocate and fill the closure with captured variables.
      auto memory = createAllocation(module, builder, DataLayout{&module}.getTypeAllocSize(wrapper_type));
      memory->mutateType(wrapper_type->getPointerTo());

      builder.CreateStore(F, builder.CreateGEP(wrapper_type, memory, {make_index(0), make_index(0)}, "funptrslot")); // store function pointer...
      auto env_ptr = builder.CreateGEP(wrapper_type, memory, {make_index(0), make_index(1), make_index(0)}, "env");
      {
        unsigned index = 0;
        for (auto var : free_vars)
          builder.CreateStore(variables.at(var), builder.CreateGEP(env_array_type->getElementType(), env_ptr, make_index(index++), "varslot"));
      }

      BasicBlock *BB = BasicBlock::Create(ctx, "entry", F);
      IRBuilder<> fun_builder(BB);

      std::map<PLambda::lvar, Value*> inner_variables;
      // Adapt free bindings from the enclosing expression
      {
        unsigned index = 0;
        auto env = std::next(F->arg_begin()); // second argument is the environment.
        outs() << "type: "; env->print(outs()); outs () << '\n';
        for (auto var : free_vars)
          inner_variables[var] = builder.CreateExtractValue(env, {0, index++}, "extract");
      }
      inner_variables[fn_var] = F->arg_begin(); // first argument is the argument

      fun_builder.CreateRet(compile(module, fun_builder, fn_body, inner_variables));

      return memory;
    }
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);
      auto assign_value = compile(module, builder, assign_exp, variables);
      auto inner_vars = variables;
      inner_vars.emplace(let_var, assign_value);
      return compile(module, builder, body_exp, inner_vars);
    }
    case APP: {
      auto& [fn_exp, arg_exp] = get<APP>(expression);
      auto fn_val  = compile(module, builder, fn_exp,  variables),
           arg_val = compile(module, builder, arg_exp, variables);
      // Primitive operations require special treatment:
      if (fn_exp.get().index() == PRIM) {
        auto& prim_oper = get<Primop::primop>(get<PRIM>(fn_exp.get()));
        return compile_primop(module, builder, prim_oper, arg_exp, variables);
      }
      // The remaining cases are arbitrary function calls.
      else {
        static auto wrapper_type = StructType::create({genericFunctionType(ctx)->getPointerTo(),
                                                       ArrayType::get(genericPointerType(ctx), 0)}, "GenericWrapper");
        fn_val->mutateType(wrapper_type->getPointerTo());
        return builder.CreateCall(genericFunctionType(ctx), builder.CreateGEP(wrapper_type, fn_val, {make_index(0), make_index(0)}, "funptr"),
                                  {arg_val,
                                   builder.CreateGEP(wrapper_type, fn_val, {make_index(0), make_index(1)}, "envptr")});
      }
    }
    case TFN:
      return compile(module, builder, get<dlexp>(get<TFN>(expression)), variables);
    case TAPP:
      return compile(module, builder, get<dlexp>(get<TAPP>(expression)), variables);

    // Implement exceptions as terminations for now.
    case RAISE:
      builder.CreateCall(module.getOrInsertFunction("abort",
                           FunctionType::get(Type::getVoidTy(module.getContext()), false)),
                         {});
      return nullptr;
    case HANDLE:
      return compile(module, builder, get<0>(get<HANDLE>(expression)), variables);

    default:
      throw UnsupportedException{"compile: unsupported plambda type: " + std::to_string(expression.index())};
  }
}

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit const& unit,
                 Module& module) {
  auto& ctx = module.getContext();

  auto exportFn = Function::Create(FunctionType::get(ArrayType::get(genericPointerType(ctx),
                                                             unit.exportedDecls.size()),
                                                    Type::getVoidTy(ctx),
                                                    false),
                            Function::ExternalLinkage, "export", &module);

  BasicBlock *BB = BasicBlock::Create(ctx, "entry", exportFn);
  IRBuilder<> builder(BB);
  std::vector<Value*> values;
  for (auto& [val, exp] : unit.globalDecls) {
    auto p = compile(module, builder, exp, {});
    if (unit.exportedDecls.count(val))
      values.push_back(p);
  }
  builder.CreateAggregateRet(values.data(), values.size());

}

}

