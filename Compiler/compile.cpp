#include "compile.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>

#include <boost/container/static_vector.hpp>
#include <boost/range/adaptor/transformed.hpp>

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
    case CON: {
      using namespace Access;

      auto& [constr, tycs, arg] = get<CON>(exp);
      auto fvs = freeVars(arg);
      // Compute any references to lambda-bound variables. Ignore SUSPs for now,.
      if (auto exn = get_if<EXN>(&get<Access::conrep>(constr))) {
        while (auto next = get_if<PATH>(exn))
          exn = &next->first;
        if (auto v = get_if<LVAR>(exn))
          fvs.insert(*v);
      }
      return fvs;
    }

    //! Exceptions are not supported yet; ignore raises and handles.
    case RAISE:  return {};


    // Simple compositional cases:

    case HANDLE: return freeVars(get<0>(    get<HANDLE>(exp)));
    case ETAG:   return freeVars(get<dlexp>(  get<ETAG>(exp)));
    case SELECT: return freeVars(get<dlexp>(get<SELECT>(exp)));
    case PACK:   return freeVars(get<dlexp>(  get<PACK>(exp)));
    case WRAP:   return freeVars(get<dlexp>(  get<WRAP>(exp)));
    case UNWRAP: return freeVars(get<dlexp>(get<UNWRAP>(exp)));
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

  lexp const* body = &get<dlexp>(get<FN>(exp)).get();

  for (bool is_intro = true; body->index() == LET;) {
    auto& [var, assign, value] = get<LET>(*body);
    if (is_intro && assign.get().index() != SELECT) {
      // For now, assume that we don't have dependencies on imports
      auto freevars = freeVars(*body);
      if (!freevars.empty())
        throw CompileFailException{"getGlobalDeclarations: Unresolved imports: " + std::to_string(*freevars.begin()) + ", ..."};
      is_intro = false;
    }
    if (!is_intro)
      globalDecls.emplace_back(var, assign);
    body = &value.get();
  }


  if (body->index() != SRECORD)
    throw CompileFailException{"getGlobalDeclarations: Function doesn't yield a structural record!"};
  auto& params = get<SRECORD>(*body);
  for (auto& e : params) {
    if (e.index() != VAR)
      throw CompileFailException{"getGlobalDeclarations: Function doesn't yield structural record of variables!"};
    exportedDecls.emplace(get<VAR>(e), std::string{});
  }
}

// For now, we will employ malloc.
Value* createAllocation(Module& module, IRBuilder<>& builder, Type* type, std::size_t n = 1) {
  static const auto size_type = Type::getInt64Ty(module.getContext());
  static const auto malloc_ptr
    = cast<Function>(module.getOrInsertFunction("malloc",
                                 FunctionType::get(Type::getInt8Ty(module.getContext())->getPointerTo(),
                                                   {size_type}, false)));
  auto ptr = builder.CreateCall(malloc_ptr, ConstantInt::get(size_type, n * DataLayout{&module}.getTypeAllocSize(type)), "storage");
  return builder.CreatePointerCast(ptr, type->getPointerTo(), "allocatedobj");
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
      if (kind.index() == Primop::FLOAT)
        throw UnsupportedException{"Floating point arithmetic"};
      else {
        // Integers are stored unboxed:
        Value* result;
        for (auto& val : arg_values)
          val = builder.CreatePtrToInt(val, Type::getInt64Ty(module.getContext()));
        switch (oper) {
          #define PRIMOP_BIN_I(fun) \
            assert_arity(arg_values, 2); \
            result = builder.Create##fun(arg_values[0], arg_values[1]); \
            break;
          case MUL: PRIMOP_BIN_I(Mul)
          case ADD: PRIMOP_BIN_I(Add)
          case SUB: PRIMOP_BIN_I(Sub)
          default:
            throw UnsupportedException{"Arithmetic operator not implemented: " + std::to_string(oper)};
        }
        return builder.CreateIntToPtr(result, genericPointerType(module.getContext()));
      }
    }
    default:
      throw UnsupportedException{"primop " + std::to_string(op.index())};
  }
}

Value* compile(Module& module, IRBuilder<>& builder, lexp const& expression, std::map<PLambda::lvar, Value*> const& variables) {
  auto& ctx = module.getContext();
  auto recurse = [&] (lexp const& e) {
    return compile(module, builder, e, variables);
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

      auto aggregate_v = builder.CreateInsertValue(UndefValue::get(wrapper_type), F, {0}, "funptrslot"); // store function pointer...
      {
        unsigned index = 0;
        for (auto var : free_vars)
          aggregate_v = builder.CreateInsertValue(aggregate_v, variables.at(var), {1, index++}, "closure");
      }

      // Allocate and fill the closure with captured variables.
      auto memory = createAllocation(module, builder, wrapper_type);
      builder.CreateStore(aggregate_v, memory);

      BasicBlock *BB = BasicBlock::Create(ctx, "entry", F);
      IRBuilder<> fun_builder(BB);

      std::map<PLambda::lvar, Value*> inner_variables;
      // Adapt free bindings from the enclosing expression
      {
        unsigned index = 0;
        auto env = std::next(F->arg_begin()); // second argument is the environment.
        for (auto var : free_vars)
          inner_variables[var] = fun_builder.CreateLoad(fun_builder.CreateConstGEP1_32(env, index++, "capturedptr"), "captured");
      }
      inner_variables[fn_var] = F->arg_begin(); // first argument is the argument

      if (auto retv = compile(module, fun_builder, fn_body, inner_variables))
        fun_builder.CreateRet(retv);
      else
        fun_builder.CreateRet(ConstantPointerNull::get(genericPointerType(ctx)));

      return builder.CreatePointerCast(memory, genericPointerType(ctx));
    }
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);
      auto assign_value = recurse(assign_exp);
      auto inner_vars = variables;
      inner_vars.emplace(let_var, assign_value);
      return compile(module, builder, body_exp, inner_vars);
    }
    case APP: {
      auto& [fn_exp, arg_exp] = get<APP>(expression);
      // Primitive operations require special treatment:
      if (fn_exp.get().index() == PRIM) {
        auto& prim_oper = get<Primop::primop>(get<PRIM>(fn_exp.get()));
        return compile_primop(module, builder, prim_oper, arg_exp, variables);
      }
      // The remaining cases are arbitrary function calls.
      else {
        static auto wrapper_type = StructType::create({genericFunctionType(ctx)->getPointerTo(),
                                                       ArrayType::get(genericPointerType(ctx), 0)}, "GenericWrapper");
        auto closure_ptr = builder.CreatePointerCast(recurse(fn_exp), wrapper_type->getPointerTo(), "closureptr");
        auto arg_val = recurse(arg_exp);
        auto fn_ptr = builder.CreateLoad(builder.CreateConstGEP2_32(wrapper_type, closure_ptr, 0, 0, "funptrptr"), "funptr");
        return builder.CreateCall(fn_ptr, {arg_val, builder.CreatePointerCast(builder.CreateConstGEP2_32(wrapper_type, closure_ptr, 0, 1, "envptr"),
                                                                              genericPointerType(ctx)->getPointerTo())});
      }
    }

    case RECORD: {
      auto values = get<RECORD>(expression) | boost::adaptors::transformed(recurse);
      auto storage = createAllocation(module, builder, genericPointerType(ctx), values.size());
      for (std::size_t i = 0; i < values.size(); ++i)
        builder.CreateStore(values[i], builder.CreateConstGEP1_32(storage, i, "rcdvalptr"));
      return builder.CreatePointerCast(storage, genericPointerType(ctx));
    }

    case SELECT: {
      auto& [indices, record] = get<SELECT>(expression);
      if (indices.size() != 1)
        throw UnsupportedException{"Nested indexing"};
      auto record_v = builder.CreatePointerCast(compile(module, builder, record, variables),
                                                genericPointerType(ctx)->getPointerTo());
      return builder.CreateLoad(builder.CreateConstGEP1_32(record_v, indices[0], "selectedptr"), "selected");
    }

    case TFN:
      return recurse(get<dlexp>(get<TFN>(expression)));
    case TAPP:
      return recurse(get<dlexp>(get<TAPP>(expression)));

    // Implement exceptions as terminations for now.
    case RAISE:
      builder.CreateCall(module.getOrInsertFunction("abort",
                           FunctionType::get(Type::getVoidTy(module.getContext()), false)),
                         {});
      return nullptr;
    case HANDLE:
      return recurse(get<0>(get<HANDLE>(expression)));

    default:
      throw UnsupportedException{"compile: unsupported plambda type: " + std::to_string(expression.index())};
  }
}

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit const& unit, Module& module) {
  auto& ctx = module.getContext();

  auto exportFn = Function::Create(FunctionType::get(ArrayType::get(genericPointerType(ctx),
                                                             unit.exportedDecls.size()),
                                                    /* isVarArg = */false),
                            Function::ExternalLinkage, "export", &module);

  BasicBlock *BB = BasicBlock::Create(ctx, "entry", exportFn);
  IRBuilder<> builder(BB);
  std::vector<Value*> values;
  for (auto& [val, exp] : unit.globalDecls) {
    auto p = compile(module, builder, exp, {});
    if (unit.exportedDecls.count(val))
      values.push_back(builder.CreatePointerCast(p, genericPointerType(ctx)));
  }
  builder.CreateAggregateRet(values.data(), values.size());
}

}

