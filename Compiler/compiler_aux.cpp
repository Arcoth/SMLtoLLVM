#include "compile.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <optional>
#include <iterator>

namespace SMLCompiler {

using std::get;
using std::get_if;

using namespace PLambda;

Value* getTagPtrFromRecordPtr(IRBuilder<>& builder, Value* rcd);

Value* boxIntNoCast(IRBuilder<>& builder, Value* x) {
  return builder.CreateOr(builder.CreateShl(x, GC::valueFlagLength, "shifted_int"),
                          GC::intTag, "boxed_int");
}

Value* boxRealInInt(IRBuilder<>& builder, Value* x) {
  auto& module = *builder.GetInsertBlock()->getModule();

  x = builder.CreateBitCast(x, genericIntType(module), "real_to_int");
  return builder.CreateOr(x, GC::floatTag, "tagged_float");
}

Value* boxReal(IRBuilder<>& builder, Value* x) {
  if (!x->getType()->isDoubleTy())
    return x;
  return builder.CreateIntToPtr(boxRealInInt(builder, x), genericPointerType, "float_in_int_to_ptr");
}

Value* boxInt(IRBuilder<>& builder, Value* x) {
  if (!x->getType()->isIntegerTy())
    return x;
  return builder.CreateIntToPtr(boxIntNoCast(builder, x), genericPointerType, "int_in_ptr");
}

Value* box(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return boxInt(builder, x);
  if (x->getType()->isDoubleTy())
    return boxReal(builder, x);
  return x;
}

Value* unboxRealFromInt(IRBuilder<>& builder, Value* x) {
  auto& mod = *builder.GetInsertBlock()->getModule();
  return builder.CreateBitCast(builder.CreateAnd(x, ~GC::floatTag, "untagged_float"),
                               Type::getDoubleTy(mod.getContext()), "unboxed_real");
}

Value* unboxReal(IRBuilder<>& builder, Value* x) {
  auto& mod = *builder.GetInsertBlock()->getModule();
  if (x->getType()->isDoubleTy())
    return x;
  x = builder.CreatePtrToInt(x, genericIntType(mod), "float_in_int");
  return unboxRealFromInt(builder, x);
}

Value* compile(IRBuilder<>& builder,
               lexp& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit&,
               AstContext astContext);

Type* boxedType(Module& m, Type* t) {
  return t->isDoubleTy()? genericIntType(m) : t;
}
Value* unboxedValue(IRBuilder<>& builder, Value* x) {
  return builder.CreateAShr(x, GC::valueFlagLength, "unboxed_int");
}
Value* boxedValue(IRBuilder<>& builder, Value* value) {
  if (value->getType()->isIntegerTy())
    return boxIntNoCast(builder, value);
  if (value->getType()->isDoubleTy())
    return boxRealInInt(builder, value);
  return value;
}

Value* unboxInt(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return x;
  return unboxedValue(builder, builder.CreatePtrToInt(x, genericIntType(*builder.GetInsertBlock()->getModule()), "ptr_to_int"));
}


// To cover regular and list-CPS cases
void yieldValue(IRBuilder<>& builder, Value* v, AstContext astContext) {
  if (astContext.isListCPSFunction) {
    builder.CreateStore(v, builder.CreateConstGEP1_32(builder.GetInsertBlock()->getParent()->arg_begin() + 2, 2, "list_cps_slot"));
    builder.CreateRetVoid();
  }
  else
    builder.CreateRet(box(builder, v));
}

auto freeVarOccurrences(vector<lexp>& exps, lexp* enclosing) {
  std::multimap<lvar, Occurrence> s;
  for (auto& e : exps)
    s = set_union(s, freeVarOccurrences(e, enclosing));
  return s;
}


std::multimap<lvar, Occurrence> freeVarOccurrences( lexp& exp, lexp* enclosing) {
  switch(exp.index()) {
    case INT: case INT32: case WORD:
    case WORD32: case REAL: case PRIM:
    case STRING: case GENOP:
      return {};

    case VAR: return {{get<VAR>(exp), {.enclosing_exp = enclosing, .holding_exp = &exp}}};

    case FN: {
      auto& fn = get<FN>(exp);
      return set_subtract(freeVarOccurrences(get<dlexp>(fn), &exp), {get<lvar>(fn)});
    }
    case FIX: {
      auto& [decls, in] = get<FIX>(exp);
      std::multimap<lvar, Occurrence> freevars;
      std::set<lvar> params;
      for (auto& [var, _, body] : decls) {
        freevars = set_union(freevars, freeVarOccurrences(body, &exp));
        params.insert(var);
      }
      return set_subtract(freevars, params);
    }
    case APP: {
      auto& [fun, arg] = get<APP>(exp);
      return set_union(freeVarOccurrences(fun, &exp), freeVarOccurrences(arg, &exp));
    }
    case LET: {
      auto& [var, assign, in] = get<LET>(exp);
      return set_union(freeVarOccurrences(assign, &exp), set_subtract(freeVarOccurrences(in, &exp), {var}));
    }
    case SWITCH: {
      auto& [arg, matches, default_] = get<SWITCH>(exp);
      auto s = freeVarOccurrences(arg, &exp);
      for (auto& [constructor, target] : matches) {
        auto exp_vars = freeVarOccurrences(target, &exp);
        if (constructor.index() == DATAcon)
          exp_vars = set_subtract(exp_vars, {get<lvar>(get<DATAcon>(constructor))});
        s = set_union(s, exp_vars);
      }
      if (default_)
        s = set_union(s, freeVarOccurrences(default_.value(), &exp));

      return s;
    }
    case CON: {
      using namespace Access;

      auto& [constr, tycs, arg] = get<CON>(exp);
      auto fvs = freeVarOccurrences(arg, &exp);
      // Compute any references to lambda-bound variables. Ignore SUSPs for now,.
      if (auto exn = get_if<EXN>(&get<Access::conrep>(constr))) {
        while (auto next = get_if<PATH>(exn))
          exn = &next->first;
        if (auto v = get_if<LVAR>(exn))
          fvs.emplace(*v, Occurrence{.enclosing_exp = &exp, .holding_exp = nullptr});
      }
      return fvs;
    }

    //! Exceptions are not supported yet; ignore raises and handles.
    case RAISE:  return {};


    // Simple compositional cases:

    case HANDLE: return freeVarOccurrences(get<0>(    get<HANDLE>(exp)), &exp);
    case ETAG:   return freeVarOccurrences(get<dlexp>(  get<ETAG>(exp)), &exp);
    case SELECT: return freeVarOccurrences(get<dlexp>(get<SELECT>(exp)), &exp);
    case PACK:   return freeVarOccurrences(get<dlexp>(  get<PACK>(exp)), &exp);
    case WRAP:   return freeVarOccurrences(get<dlexp>(  get<WRAP>(exp)), &exp);
    case UNWRAP: return freeVarOccurrences(get<dlexp>(get<UNWRAP>(exp)), &exp);
    case TFN:    return freeVarOccurrences(get<dlexp>(   get<TFN>(exp)), &exp);
    case TAPP:   return freeVarOccurrences(get<dlexp>(  get<TAPP>(exp)), &exp);


    case RECORD:  return freeVarOccurrences( get<RECORD>(exp), &exp);
    case SRECORD: return freeVarOccurrences(get<SRECORD>(exp), &exp);
    case VECTOR: return freeVarOccurrences(get<0>(get<VECTOR>(exp)), &exp);


    default:
      throw UnsupportedException{"Unsupported lambda type " + std::to_string(exp.index())};
  }
}

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

    case VAR:
    return {get<VAR>(exp)};

    case FN: {
      auto& [var, _, body] = get<FN>(exp);
      return set_subtract(freeVars(body), {var});
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
      if (default_)
        s = set_union(s, freeVars(default_.value()));
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

SMLTranslationUnit::SMLTranslationUnit(lexp const& exp, ImportsVector imports, llvm::Module* mod)
  : importTree(std::move(imports)), module(std::move(mod)), exportedLexp(exp) {
  auto fn = get_if<FN>(&exportedLexp);
  if (!fn)
    throw CompileFailException{"SMLTranslationUnit(lexp const&): expression not a function"};
  auto body = &get<2>(*fn);

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

Value* createAllocation(Module& module, IRBuilder<>& builder, Type* type,
                        GC::Heap heap = GC::Heap::Young, std::size_t n = 1) {
  static const auto size_type = genericIntType(module);
  auto alloc_fun = module.getFunction(heap == GC::Heap::Mutable? mutableAllocFun :
                                      heap == GC::Heap::Young? smallAllocFun :
                                      heap == GC::Heap::Old? largeAllocFun : throw std::runtime_error{""});
  if (!alloc_fun)
    throw CompileFailException{"Allocation function not found!"};

  auto ptr = builder.CreateCall(alloc_fun, ConstantInt::get(size_type, n * allocationSizeOf(&module, type)), "storage");
  return builder.CreatePointerCast(ptr, type->getPointerTo(heapAddressSpace), "storage_ptr");
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
          return builder.CreateLoad(getTagPtrFromRecordPtr(builder, exp_v), "record_tag");
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

using namespace Lty;
bool isInteger(lty l) {
  if (l.index() == LT_TYC) {
    auto& a = std::get<LT_TYC>(l);
    if (a.index() == TC_PRIM) {
      auto& b = std::get<TC_PRIM>(a);
      return b == PrimTyc::PT_INT31
          || b == PrimTyc::PT_INT32
          || b == PrimTyc::PT_INTINF;
    }
  }
  return false;
}
bool isReal(lty l) {
  if (l.index() == LT_TYC) {
    auto& a = std::get<LT_TYC>(l);
    if (a.index() == TC_PRIM) {
      auto& b = std::get<TC_PRIM>(a);
      return b == PrimTyc::PT_REAL;
    }
  }
  return false;
}

Value* compileFunction(IRBuilder<>* builder,
                       lexp& expression,
                       std::map<PLambda::lvar, Value*> const& variables,
                       SMLTranslationUnit& unit,
                       AstContext astContext) {
  auto& module = *unit.module;
  auto& context = module.getContext();

  auto& [fn_var, fn_lty, fn_body] = get<FN>(expression);
  auto free_vars = freeVars(expression);

  const bool isRealFunction = isReal(fn_lty),
              isIntFunction = isInteger(fn_lty);


  FunctionType* fun_type;
  if (astContext.moduleExportExpression)
    fun_type = FunctionType::get(genericPointerType, {genericPointerType}, false);
  else
    fun_type = isIntFunction? intFunctionType(module, astContext.isListCPSFunction) :
              isRealFunction? realFunctionType(module, astContext.isListCPSFunction) :
                              genericFunctionType(astContext.isListCPSFunction);

  // Create the hoisted function.
  char const* name = astContext.moduleExportExpression? "export" : nameForFunction(fn_var, astContext.isListCPSFunction);
  auto F = Function::Create(fun_type, Function::ExternalLinkage, name, &module);

  unit.closureLength.emplace_back(F, free_vars.size()+1);

  unit.paramFuncs[fn_var] = F->getName();

  BasicBlock *BB = BasicBlock::Create(context, "entry", F);
  IRBuilder<> fun_builder(BB);

  std::map<PLambda::lvar, Value*> inner_variables;
  // Adapt free bindings from the enclosing expression

  {
    unsigned index = 1; //! Start at 1; we pass the closure in without offset.
    auto env = F->arg_begin()+1; // second argument is the environment.
    for (auto var : free_vars)
      inner_variables[var] = fun_builder.CreateLoad(fun_builder.CreateConstGEP1_32(env, index++, "captured_ptr"), "captured");
  }

  // Don't unbox in CPS functions, they're not meant to be generic
  if (isRealFunction && !astContext.isListCPSFunction)
    inner_variables[fn_var] = unboxRealFromInt(fun_builder, F->arg_begin());
  else if (isIntFunction && !astContext.isListCPSFunction)
    inner_variables[fn_var] = unboxedValue(fun_builder, F->arg_begin());
  else
    inner_variables[fn_var] = F->arg_begin();

  std::vector<Type*> types{fun_type->getPointerTo()};
  for (auto var : free_vars) {
    auto var_it = variables.find(var);
    if (var_it == variables.end())
      throw CompileFailException{"FN: Captured variable " + std::to_string(var) + " not defined in enclosing scope"};
    types.push_back(boxedType(module, var_it->second->getType()));
  }

  if (astContext.isBodyOfFixpoint)
    astContext.isBodyOfFixpoint = false;
  else
    astContext.fixPointVariable = -1;
  astContext.isFinalExpression = true;

  AstContext newAstCtx = astContext;
  newAstCtx.enclosingFunctionExpr = &expression;
  newAstCtx.moduleExportExpression = false;
  if (auto retv = compile(fun_builder, fn_body, inner_variables, unit, newAstCtx))
    yieldValue(fun_builder, retv, astContext);

  if (astContext.isListCPSFunction)
    return F;
  if (!astContext.moduleExportExpression) {
    assert(builder && "Builder must be non-zero!");

    auto wrapper_type = StructType::create(types);
    auto memory = createAllocation(module, *builder, wrapper_type);

    auto aggregate_v = builder->CreateInsertValue(UndefValue::get(wrapper_type), F, {0}, "fun_ptr_slot"); // store function pointer...
    {
      unsigned index = 1;
      for (auto var : free_vars) {
        auto& var_v = variables.at(var);
        aggregate_v = builder->CreateInsertValue(aggregate_v, boxedValue(*builder, var_v), {index++}, "closure");
      }
    }

    // Allocate and fill the closure with captured variables.
    builder->CreateStore(aggregate_v, memory);
    return builder->CreatePointerCast(memory, genericPointerType, "clos_ptr_cast");
  }
  return nullptr;
}


}
