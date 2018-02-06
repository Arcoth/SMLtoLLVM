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
      for (auto& [var, _, body] : decls) {
        freevars = set_union(freevars, freeVars(body));
        params.insert(var);
      }
      return set_subtract(freevars, params);
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

lexp const* outermostClosedLet(lexp const& exp) {
  lexp const* ret = &exp;
  while (ret->index() == LET) {
    auto& [var, assign, value] = get<LET>(*ret);
    if (freeVars(*ret).empty())
      return ret;
    ret = &value;
  }
  return freeVars(*ret).empty()? ret : nullptr;
}

SMLTranslationUnit::SMLTranslationUnit(lexp const& exp) {
  if (exp.index() != FN)
    throw CompileFailException{"SMLTranslationUnit(lexp const&): expression not a function"};

  auto body = outermostClosedLet(get<dlexp>(get<FN>(exp)).get());
  if (!body)
    throw CompileFailException{"PLambda depends on external imports!"};
  exportedLetExpr = *body;
  while (auto p = get_if<LET>(body))
    body = &get<2>(*p);
  if (body->index() != SRECORD)
    throw CompileFailException{"SMLTranslationUnit(lexp const&): Function doesn't yield a structural record!"};
  auto& params = get<SRECORD>(*body);
  for (auto& e : params) {
    if (e.index() != VAR)
      throw CompileFailException{"SMLTranslationUnit(lexp const&): Function doesn't yield structural record of variables!"};
    exportedDecls.emplace(get<VAR>(e), std::string{});
  }
}

Value* createAllocation(Module& module, IRBuilder<>& builder, Type* type, std::size_t n = 1) {
  static const auto size_type = genericIntType(module);
  auto alloc_fun = module.getFunction(allocationFunctionName);
  if (!alloc_fun)
    throw CompileFailException{"Allocation function not found!"};

  auto ptr = builder.CreateCall(alloc_fun, ConstantInt::get(size_type, n * DataLayout{&module}.getTypeAllocSize(type)), "storage");
  return builder.CreatePointerCast(ptr, type->getPointerTo(heapAddressSpace), "allocatedobj");
}

Value* boxIntNoCast(IRBuilder<>& builder, Value* x) {
  return builder.CreateAdd(builder.CreateShl(x, 1, "shifted_int"), ConstantInt::get(x->getType(), 1), "boxed_int");
}

Value* boxInt(IRBuilder<>& builder, Value* x) {
  auto& module = *builder.GetInsertBlock()->getModule();
  return builder.CreateIntToPtr(boxIntNoCast(builder, x), genericPointerType(module.getContext()));
}

// expects a value of generic pointer tpye,
Value* unboxInt(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return x;
  return builder.CreateAShr(builder.CreatePtrToInt(x, genericIntType(*builder.GetInsertBlock()->getModule()), "ptr_to_int"),
                            1, "unboxed_int");
}

Value* compile(IRBuilder<>& builder,
               lexp const& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit&);

// Here, exp is a RCD of one or two arguments to the operator.
Value* compile_primop(IRBuilder<>& builder,
                      Primop::primop const& op,
                      lexp const& exp,
                      std::map<PLambda::lvar, Value*> const& variables,
                      SMLTranslationUnit& unit) {
  using namespace Primop;
  auto& module = *builder.GetInsertBlock()->getModule();

  boost::container::static_vector<std::function<Value*()>, 5> arg_values;
  std::function<Value*()> arg_value;
  if (auto rcd = get_if<RECORD>(&exp))
    for (auto& e : *rcd)
      arg_values.push_back([&] {return compile(builder, e, variables, unit);});
  else {
    auto v = compile(builder, exp, variables, unit);
    auto v_ptr = builder.CreatePointerCast(v, genericPointerType(module.getContext())->getPointerTo(heapAddressSpace));
    arg_value = [v] {return v;};
    for (int i = 0; i < 3; ++i)
      arg_values.push_back([i, v_ptr, &builder] {return builder.CreateLoad(builder.CreateConstGEP1_32(v_ptr, i+1));});
  }

  auto unbox_args = [&] {
    for (auto& val : arg_values)
      val = [&builder, val] {return unboxInt(builder, val());};
  };

  auto assert_arity = [] (auto& container, std::size_t expected) {
    if (std::size(container) != expected)
      throw CompileFailException{"Wrong number of arguments to primop - " + std::to_string(std::size(container))
                               + " instead of " + std::to_string(expected)};
  };

  Value* result;
  switch (op.index()) {
    case ARITH: {
      auto& [oper, overflow, kind] = get<ARITH>(op);
      if (kind.index() == Primop::FLOAT)
        throw UnsupportedException{"Floating point arithmetic"};

      // Integers are stored unboxed:
      unbox_args();
      switch (oper) {
        #define PRIMOP_BIN_I(fun) \
          assert_arity(arg_values, 2); \
          result = builder.Create##fun(arg_values[0](), arg_values[1]()); \
          break;
        case MUL: PRIMOP_BIN_I(Mul)
        case ADD: PRIMOP_BIN_I(Add)
        case SUB: PRIMOP_BIN_I(Sub)
        default:
          throw UnsupportedException{"Arithmetic operator: " + std::to_string(oper)};
      }
      break;
    }
    case CMP: {
      auto& cmp = get<CMP>(op);
      if (cmp.kind.index() == Primop::FLOAT)
        throw UnsupportedException{"Floating point arithmetic"};
      auto op = [&] (CmpInst::Predicate pred) {
        result = builder.CreateIntCast(builder.CreateICmp(pred, arg_values[0](), arg_values[1](), "intcomp"),
                                       genericIntType(module), false, "bool_to_int");
      };
      // Integers are stored unboxed:
      unbox_args();
      switch (cmp.oper) {
        case EQL: {op(CmpInst::ICMP_EQ ); break;}
        case NEQ: {op(CmpInst::ICMP_NE ); break;}

        case LT:  {op(CmpInst::ICMP_SLT); break;}
        case LTE: {op(CmpInst::ICMP_SLE); break;}
        case GT:  {op(CmpInst::ICMP_SGT); break;}
        case GTE: {op(CmpInst::ICMP_SGE); break;}

        case LTU: {op(CmpInst::ICMP_ULT); break;}
        case LEU: {op(CmpInst::ICMP_ULE); break;}
        case GTU: {op(CmpInst::ICMP_UGT); break;}
        case GEU: {op(CmpInst::ICMP_UGE); break;}
        default:
          throw UnsupportedException{"Comparison operator: " + std::to_string(cmp.oper)};
      }
      break;
    }
    case DEREF: {
      return builder.CreateLoad(builder.CreatePointerCast(arg_value(),
                                                          genericPointerType(module.getContext())->getPointerTo(heapAddressSpace)));
    }
    case MAKEREF: {
      result = createAllocation(module, builder, genericPointerType(module.getContext()));
      builder.CreateStore(arg_value(), result);
      return builder.CreatePointerCast(result, genericPointerType(module.getContext()));
    }
    case ASSIGN: {
      builder.CreateStore(arg_values[1](), builder.CreatePointerCast(arg_values[0](),
                                                                     genericPointerType(module.getContext())->getPointerTo(heapAddressSpace)));
      return ConstantPointerNull::get(genericPointerType(module.getContext()));
    }
    default:
      throw UnsupportedException{"primop " + std::to_string(op.index())};
  }
  return result;
}

// the first 64 bit is the tag!
template <typename Rng>
Value* record(IRBuilder<>& builder, Rng const& values ) {
  auto& module = *builder.GetInsertBlock()->getModule();
  auto storage = createAllocation(module, builder, genericPointerType(module.getContext()), std::size(values)+1);
  auto tag_ptr = builder.CreatePointerCast(storage, tagType(module)->getPointerTo(heapAddressSpace), "tag_len_ptr");
  builder.CreateStore(ConstantInt::get(tagType(module), ((std::size(values)+1) << 2) | 1), tag_ptr);
  for (std::size_t i = 0; i < std::size(values); ++i)
    builder.CreateStore(values[i], builder.CreateConstGEP1_32(storage, i+1, "rcdvalptr"));
  return storage;
}

template <std::size_t N>
Value* record(IRBuilder<>& builder, Value* const (&values)[N] ){
  return record<decltype(values)>(builder, values);
}

Value* getTagPtrFromRecordPtr(IRBuilder<>& builder, Value* rcd) {
  auto& module = *builder.GetInsertBlock()->getModule();
  return builder.CreateConstGEP1_32(builder.CreatePointerCast(rcd, tagType(module)->getPointerTo(heapAddressSpace), "tag_ptr_base"), 1,
                                    "tag_ptr");
}

// Extracts the value to switch by. constr is a sample constructor to determine the switching type.
/*
  The tag of the constructor is in the lower part of the entire tag, which occupies a machine word (e.g. 64 bits).
*/
Value* extractTag(IRBuilder<>& builder, SMLTranslationUnit const& unit, Value* exp_v, con const& constr) {
  auto& module = *builder.GetInsertBlock()->getModule();
  switch (constr.index()) {
    case DATAcon: {
      auto rep = unit.symbolRepresentation.at(get<Symbol::symbol>(get<DATAcon>(constr)));
      switch (rep.type) {
        case symbol_rep::UNTAGGED:
          return builder.CreateIsNotNull(exp_v);
        case symbol_rep::TAGGED:
          return builder.CreateLoad(getTagPtrFromRecordPtr(builder, exp_v));
        case symbol_rep::CONSTANT:
          return builder.CreateIntCast(unboxInt(builder, exp_v), tagType(module), false);
      }
    }
    case INTcon: case INT32con:
      return unboxInt(builder, exp_v);
    default:
      throw UnsupportedException{"constructor " + std::to_string(constr.index())};
  }
}

ConstantInt* getTag(Module& module, SMLTranslationUnit const& unit, con const& constr) {
  std::size_t value;
  switch (constr.index()) {
    case DATAcon: {
      auto symbol = get<Symbol::symbol>(get<DATAcon>(constr));
      return unit.symbolRepresentation.at(symbol).value;
    }
    case INTcon:
      value = get<INTcon>(constr);
      break;
    case INT32con:
      value = get<INT32con>(constr);
      break;
    default:
      throw UnsupportedException{"constructor " + std::to_string(constr.index())};
  }
  return ConstantInt::get(genericIntType(module), value);
}

void insertAbort(IRBuilder<>& builder) {
  auto& module = *builder.GetInsertBlock()->getModule();
  builder.CreateCall(module.getOrInsertFunction("abort",
                           FunctionType::get(Type::getVoidTy(module.getContext()), false)),
                     {});
  builder.CreateUnreachable();
}

Value* compile(IRBuilder<>& builder,
               lexp const& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit& unit) {
  auto& module = *builder.GetInsertBlock()->getModule();
  auto& ctx = module.getContext();
  auto recurse = [&] (lexp const& e) {
    return compile(builder, e, variables, unit);
  };
  switch (expression.index()) {
    case VAR: {
      auto var = get<VAR>(expression);
      auto iter = variables.find(var);
      if (iter == variables.end())
        throw CompileFailException{"Variable not in scope: " + std::to_string(var)};
      return iter->second;
    }
    case INT: {
      auto i = get<INT>(expression);
      return ConstantInt::getSigned(genericIntType(module), i);
    }
    case FN: {
      auto& [fn_var, fn_lty, fn_body] = get<FN>(expression);
      auto free_vars = freeVars(expression);
      auto fun_type = genericFunctionType(ctx);

      // Create the hoisted function.
      char* name = new char[16] {};
      std::sprintf(name, "lambda.v%d", fn_var);
      auto F = Function::Create(fun_type, Function::ExternalLinkage, name, &module);

      unit.closureLength.emplace_back(F, free_vars.size()+1);

      outs() << "The body of fn " << fn_var << " is " << fn_body.get().index() << '\n';

      unit.paramFuncs[fn_var] = F->getName();

      auto env_array_type = ArrayType::get(genericPointerType(ctx), free_vars.size());
      auto wrapper_type = StructType::create({genericFunctionType(ctx)->getPointerTo(),
                                              env_array_type}, F->getName().str() + "Wrapper");

      auto aggregate_v = builder.CreateInsertValue(UndefValue::get(wrapper_type), F, {0}, "funptrslot"); // store function pointer...
      {
        unsigned index = 0;
        for (auto var : free_vars) {
          auto var_it = variables.find(var);
          if (var_it == variables.end())
            throw CompileFailException{"FN: Captured variable " + std::to_string(var) + " not defined in enclosing scope"};
          aggregate_v = builder.CreateInsertValue(aggregate_v, var_it->second, {1, index++}, "closure");
        }
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

      if (auto retv = compile(fun_builder, fn_body, inner_variables, unit)) {
        if (retv->getType()->isIntegerTy())
          retv = fun_builder.CreateIntToPtr(retv, genericPointerType(ctx));
        fun_builder.CreateRet(retv);
      }

      return builder.CreatePointerCast(memory, genericPointerType(ctx), "closptrcast");
    }
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);
      if (body_exp.get().index() == VAR        // If the body is a variable,
       && get<VAR>(body_exp.get()) == let_var) // and if that variable is the LET variable,
          return recurse(assign_exp);    // just return the assignment.
      auto assign_value = recurse(assign_exp);
      auto inner_vars = variables;
      inner_vars.emplace(let_var, assign_value);
      return compile(builder, body_exp, inner_vars, unit);
    }
    case APP: {
      auto& [fn_exp, arg_exp] = get<APP>(expression);
      // Primitive operations require special treatment:
      if (fn_exp.get().index() == PRIM) {
        auto& [prim_oper,_1, _2] = get<PRIM>(fn_exp.get());
        return compile_primop(builder, prim_oper, arg_exp, variables, unit);
      }
      // The remaining cases are arbitrary function calls.
      else {
        Value* arg_val = recurse(arg_exp);

        Type* fun_type = nullptr;
        if ( arg_val->getType()->isIntegerTy()) {
          fun_type = intFunctionType(module);
          arg_val = boxIntNoCast(builder, arg_val);
        }
        else
          fun_type = genericFunctionType(ctx);

        auto wrapper_type = StructType::create({fun_type->getPointerTo(),
                                                ArrayType::get(genericPointerType(ctx), 0)}, "GenericWrapper");

        auto closure_ptr = builder.CreatePointerCast(recurse(fn_exp), wrapper_type->getPointerTo(heapAddressSpace), "closureptr");

        auto fn_ptr = builder.CreateLoad(builder.CreateConstGEP2_32(wrapper_type, closure_ptr, 0, 0, "funptrptr"), "funptr");
        return builder.CreateCall(fn_ptr, {arg_val, builder.CreatePointerCast(builder.CreateConstGEP2_32(wrapper_type, closure_ptr, 0, 1, "envptr"),
                                                                              genericPointerType(ctx)->getPointerTo(heapAddressSpace))});
      }
    }
    case FIX: {
      /*
        First, create all the closures, then adjust the environments' pointers
        once all allocations have been performed.
      */
      auto& [decls, exp] = get<FIX>(expression);
      vector<Value*> closure_prs;
      for (auto& [var, _, fn] : decls) {
        if (fn.index() != FN)
          throw UnsupportedException{"FIX expressions must declare functions"};
        auto inner_vars = variables;
        for (auto& [var, _1, _2] : decls)
          //inner_vars[var] = ConstantPointerNull::get(genericPointerType(ctx));
          // Use some specific garbage for this scenario (address 12):
          inner_vars[var] = builder.CreateIntToPtr(ConstantInt::get(genericIntType(module), 12), genericPointerType(ctx));
        closure_prs.push_back(compile(builder, fn, inner_vars, unit));
      }
      // Adjust the pointers in all environments to refer to the allocated closures.
      std::size_t decl_index = 0;
      for (auto& [var, _, fn] : decls) {
        std::size_t clos_index = 0;
        for (auto clos : closure_prs) {
          auto freevars = freeVars(get<lexp>(decls[clos_index++]));
          auto iter = freevars.find(var);
          if (iter != freevars.end()) { // If this closure captures the closure of var...
            auto pos = std::distance(freevars.begin(), iter);
            builder.CreateStore(closure_prs[decl_index],
                                builder.CreateConstGEP1_32(
                                  builder.CreatePointerCast(clos, genericPointerType(ctx)->getPointerTo(heapAddressSpace)),
                                  pos+1, "closureupdateelemptr"));
          }
        }
        ++decl_index;
      }

      // Prepare environment and compilation of the body of the FIX expression:
      auto inner_vars = variables;
      for (std::size_t i = 0; i < decls.size(); ++i)
        inner_vars[get<lvar>(decls[i])] = closure_prs[i];
      return compile(builder, exp, inner_vars, unit);
    }

    case SWITCH: {
      auto& [switched_exp, cases, default_case] = get<SWITCH>(expression);
      auto func = builder.GetInsertBlock()->getParent();
      outs() << "Compiling switch for function " << func->getName() << '\n';
      auto exit_block = BasicBlock::Create(ctx, "continue", func);
      BasicBlock* default_ = BasicBlock::Create(ctx, "default", func, exit_block);
      IRBuilder<> default_builder(default_);
      auto result = builder.CreateAlloca(genericPointerType(ctx));
      if (default_case) {
        default_builder.CreateStore(compile(default_builder, default_case.value(), variables, unit), result);
        default_builder.CreateBr(exit_block);
      }
      else
        insertAbort(default_builder);

      auto switched_v = recurse(switched_exp);
      auto switch_inst = builder.CreateSwitch(extractTag(builder, unit, switched_v, cases.at(0).first),
                                              default_, cases.size());
      for (auto& [constructor, exp] : cases) {
        auto case_bb = BasicBlock::Create(ctx, "case", func, exit_block);
        switch_inst->addCase(getTag(module, unit, constructor), case_bb);

        IRBuilder<> case_builder(case_bb);
        auto vars = variables;
        if (auto datacon = get_if<DATAcon>(&constructor)) {
          auto [sym, var] = *datacon;
          switch (unit.symbolRepresentation.at(sym).type) {
            case symbol_rep::TAGGED:
              vars[var] = case_builder.CreateConstGEP1_32(
                            case_builder.CreatePointerCast(switched_v, genericPointerType(ctx)->getPointerTo(heapAddressSpace)),
                            1);
              break;
            case symbol_rep::UNTAGGED:
              vars[var] = switched_v;
              break;
            default:;
          }
        }

        // If the result is an integer, cast the store.
        Value* case_result = compile(case_builder, exp, vars, unit);
        if (case_result->getType()->isIntegerTy())
          case_builder.CreateStore(boxIntNoCast(case_builder, case_result),
                                   case_builder.CreatePointerCast(result, genericIntType(module)->getPointerTo()));
        else
          case_builder.CreateStore(case_result, result);

        case_builder.CreateBr(exit_block);
      }
      builder.SetInsertPoint(exit_block);
      return builder.CreateLoad(result);
    }

    case CON: {
      auto& [constr, tycs, argument] = get<CON>(expression);
      auto symrep = unit.symbolRepresentation.at(get<Symbol::symbol>(constr));
      switch (symrep.type) {
        case symbol_rep::CONSTANT:
          return boxInt(builder, symrep.value);
        case symbol_rep::TAGGED: {
          auto rcd = recurse(argument);
          builder.CreateStore(symrep.value, getTagPtrFromRecordPtr(builder, rcd));
          return builder.CreatePointerCast(rcd, genericPointerType(ctx));
        }
        case symbol_rep::UNTAGGED:
          if (symrep.value->isNullValue())
            return ConstantPointerNull::get(genericPointerType(ctx));
          return recurse(argument);
      }
    }

    case RECORD: {
      auto ptr = record(builder, get<RECORD>(expression) | boost::adaptors::transformed(recurse));
      return builder.CreatePointerCast(ptr, genericPointerType(ctx), "rcdptr");
    }
    case SRECORD: {
      auto ptr = record(builder, get<SRECORD>(expression) | boost::adaptors::transformed(recurse));
      return builder.CreatePointerCast(ptr, genericPointerType(ctx)->getPointerTo(heapAddressSpace), "srcdptr");
    }

    case SELECT: {
      auto& [indices, record] = get<SELECT>(expression);
      if (indices.size() != 1)
        throw UnsupportedException{"Nested indexing"};
      auto record_v = builder.CreatePointerCast(recurse(record),
                                                genericPointerType(ctx)->getPointerTo(heapAddressSpace));
      return builder.CreateLoad(builder.CreateConstGEP1_32(record_v, indices[0] + 1, "selectedptr"), "selected");
    }

    case TFN:
      return recurse(get<dlexp>(get<TFN>(expression)));
    case TAPP:
      return recurse(get<dlexp>(get<TAPP>(expression)));

    // Implement exceptions as terminations for now.
    case RAISE:
      insertAbort(builder);
      return nullptr;
    case HANDLE:
      return recurse(get<0>(get<HANDLE>(expression)));

    default:
      throw UnsupportedException{"compile: unsupported plambda type: " + std::to_string(expression.index())};
  }
}

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit& unit, Module& module) {
  auto& ctx = module.getContext();

  auto exportFn = Function::Create(FunctionType::get(genericPointerType(ctx)->getPointerTo(heapAddressSpace),
                                                    /* isVarArg = */false),
                            Function::ExternalLinkage, "export", &module);

  BasicBlock *BB = BasicBlock::Create(ctx, "entry", exportFn);
  IRBuilder<> builder(BB);
  builder.CreateRet(compile(builder, unit.exportedLetExpr, {}, unit));
}

}
