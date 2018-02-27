#include "compile.hpp"
#include "GCPlugin/GCBasicConstants.hpp"

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

Value* createAllocation(Module& module, IRBuilder<>& builder, Type* type, bool _mutable = false, std::size_t n = 1) {
  static const auto size_type = genericIntType(module);
  auto alloc_fun = module.getFunction(_mutable? mutableAllocFun : immutableAllocFun);
  if (!alloc_fun)
    throw CompileFailException{"Allocation function not found!"};

  auto ptr = builder.CreateCall(alloc_fun, ConstantInt::get(size_type, n * DataLayout{&module}.getTypeAllocSize(type)), "storage");
  return builder.CreatePointerCast(ptr, type->getPointerTo(heapAddressSpace), "allocatedobj");
}

Value* boxIntNoCast(IRBuilder<>& builder, Value* x) {
  return builder.CreateOr(builder.CreateShl(x, GC::valueTagLen, "shifted_int"),
                          GC::intTag, "boxed_int");
}

Value* boxInt(IRBuilder<>& builder, Value* x) {
  if (!x->getType()->isIntegerTy())
    return x;
  auto& module = *builder.GetInsertBlock()->getModule();
  return builder.CreateIntToPtr(boxIntNoCast(builder, x), genericPointerType(module.getContext()), "int_in_ptr");
}

Value* boxReal(IRBuilder<>& builder, Value* x) {
  if (!x->getType()->isDoubleTy())
    return x;
  auto& module = *builder.GetInsertBlock()->getModule();

  auto asInt = builder.CreateBitCast(x, genericIntType(module), "real_to_int");
  asInt = builder.CreateOr(
      asInt,
      GC::floatTag, "tagged_float");
  return builder.CreateIntToPtr(asInt, genericPointerType(module.getContext()), "float_in_int_to_ptr");
}

Value* box(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return boxInt(builder, x);
  if (x->getType()->isDoubleTy())
    return boxReal(builder, x);
  return x;
}

Value* getBoxedValue(IRBuilder<>& builder, Value* value) {
  if (value->getType()->isIntegerTy())
    return boxIntNoCast(builder, value);
  return value;
}

// Cast the second argument to a pointer to int if the first one is an int.
// Return appropriately casted pointer to stored location.
Value* storeValue(IRBuilder<>& builder, Value* value, Value* ptr) {
  if (value->getType()->isIntegerTy()) {
    ptr = builder.CreatePointerCast(ptr, value->getType()->getPointerTo(heapAddressSpace), "int_store");
    builder.CreateStore(boxIntNoCast(builder, value), ptr);
  }
  else
    builder.CreateStore(value, ptr);
  return ptr;
}


// expects a value of generic pointer tpye,
Value* unboxInt(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return x;
  return builder.CreateAShr(builder.CreatePtrToInt(x, genericIntType(*builder.GetInsertBlock()->getModule()), "ptr_to_int"),
                            GC::valueTagLen, "unboxed_int");
}

Value* unboxReal(IRBuilder<>& builder, Value* x) {
  auto& mod = *builder.GetInsertBlock()->getModule();
  if (x->getType()->isDoubleTy())
    return x;
  x = builder.CreatePtrToInt(x, genericIntType(mod), "float_in_int");
  return builder.CreateBitCast(builder.CreateAnd(x, ~GC::floatTag, "untagged_float"), Type::getDoubleTy(mod.getContext()), "unboxed_real");
}

struct AstContext {
  bool isBodyOfFixpoint;
  PLambda::lvar fixPointVariable;
  bool isFinalExpression;
};

Value* compile(IRBuilder<>& builder,
               lexp& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit&,
               AstContext astContext);

// Here, exp is a RCD of one or two arguments to the operator.
Value* compile_primop(IRBuilder<>& builder,
                      Primop::primop const& op,
                      lexp& exp,
                      std::map<PLambda::lvar, Value*> const& variables,
                      SMLTranslationUnit& unit,
                      AstContext astContext) {
  using namespace Primop;
  auto& module = *builder.GetInsertBlock()->getModule();

  std::vector<std::function<Value*()>> arg_values;
  std::function<Value*()> arg_value;
  astContext.isFinalExpression = false;
  if (auto rcd = get_if<RECORD>(&exp))
    for (auto& e : *rcd)
      arg_values.push_back([&] {return compile(builder, e, variables, unit, astContext);});
  else {
    auto v = compile(builder, exp, variables, unit, astContext);
    arg_value = [v] {return v;};
    if (!v->getType()->isIntegerTy()) {
      auto v_ptr = builder.CreatePointerCast(v, genericPointerType(module.getContext())->getPointerTo(heapAddressSpace), "primop_rcd_ptr");
      for (int i = 0; i < 3; ++i)
        arg_values.push_back([i, v_ptr, &builder] {return builder.CreateLoad(builder.CreateConstGEP1_32(v_ptr, i+1), "primop_rcd_elem");});
    }
  }

  auto unbox_args = [&] (bool isFloat) {
    for (auto& val : arg_values)
      val = [&builder, val, isFloat] {return isFloat? unboxReal(builder, val()) : unboxInt(builder, val());};
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
      const bool isFloat = kind.index() == Primop::FLOAT;

      // Integers are stored unboxed:
      unbox_args(isFloat);
      switch (oper) {
        #define PRIMOP_BIN_I(fun) \
          assert_arity(arg_values, 2); \
          result = isFloat? builder.CreateF##fun(arg_values[0](), arg_values[1]()) \
                          : builder.Create##fun(arg_values[0](), arg_values[1]()); \
          break;
        case MUL: PRIMOP_BIN_I(Mul)
        case ADD: PRIMOP_BIN_I(Add)
        case SUB: PRIMOP_BIN_I(Sub)
        default:
          throw UnsupportedException{"Arithmetic operator: " + std::to_string(oper)};
        #undef PRIMOP_BIN_I
      }
      break;
    }
    case CMP: {
      auto& cmp = get<CMP>(op);
      const bool isFloat = cmp.kind.index() == Primop::FLOAT;
      auto op = [&] (auto pred) {
        result = builder.CreateIntCast(isFloat? builder.CreateFCmp(pred, arg_values[0](), arg_values[1](), "float_comp")
                                              : builder.CreateICmp(pred, arg_values[0](), arg_values[1](), "int_comp"),
                                       genericIntType(module), false, "bool_to_int");
      };
      // Scalars are stored unboxed:
      unbox_args(isFloat);
      #define COMP(fltname, intname) op(isFloat? CmpInst::FCMP_##fltname : CmpInst::ICMP_##intname); break;
      switch (cmp.oper) {
        case EQL: COMP(OEQ, EQ)
        case NEQ: COMP(ONE, NE)

        case LT:  COMP(OLT, SLT)
        case LTE: COMP(OLE, SLE)
        case GT:  COMP(OGT, SGT)
        case GTE: COMP(OGE, SGE)

        case LTU: COMP(OLT, ULT)
        case LEU: COMP(OLE, ULE)
        case GTU: COMP(OGT, UGT)
        case GEU: COMP(OGE, UGE)
        default:
          throw UnsupportedException{"Comparison operator: " + std::to_string(cmp.oper)};
      }
      break;
    }
    case DEREF: {
      return builder.CreateLoad(builder.CreatePointerCast(arg_value(),
                                                          genericPointerType(module.getContext())->getPointerTo(heapAddressSpace), "deref_ptr"), "deref_val");
    }
    case MAKEREF: {
      auto val = arg_value();
      result = createAllocation(module, builder, val->getType(), true);
      storeValue(builder, val, result);
      return builder.CreatePointerCast(result, genericPointerType(module.getContext()), "ref_ptr");
    }
    case ASSIGN: {
      auto lhs = arg_values[0](),
           rhs = arg_values[1]();
      lhs = builder.CreatePointerCast(lhs, rhs->getType()->getPointerTo(heapAddressSpace), "assign_casted_ptr");
      storeValue(builder, rhs, lhs);
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
  // The type used to hold the results while they're being computed.
  std::vector<Type*> record_elem_types{genericIntType(module)};
  for (auto x : values)
    record_elem_types.push_back(x->getType());
  auto record_type = StructType::create(record_elem_types);

  assert(DataLayout{&module}.getTypeAllocSize(record_type)== std::size(record_elem_types) * 8);

  //! DON'T ALLOCATE memory first; first compute values, then the heap array to store them in.
  auto tag_v = ConstantInt::get(genericIntType(module), (std::size(record_elem_types) << GC::recordTagLen) | GC::lengthTag);
  auto aggregate_v = builder.CreateInsertValue(UndefValue::get(record_type), tag_v, {0}, "record_tag"); // store function pointer...
  for (unsigned i = 0; i < std::size(values); ++i)
    aggregate_v = builder.CreateInsertValue(aggregate_v, getBoxedValue(builder, values[i]), {i+1}, "rcd_val_ptr");

  auto storage = createAllocation(module, builder, record_type);
  builder.CreateStore(aggregate_v, storage);

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

void insertAbort(IRBuilder<>& builder) {
  auto& module = *builder.GetInsertBlock()->getModule();
  builder.CreateCall(module.getOrInsertFunction("abort",
                           FunctionType::get(Type::getVoidTy(module.getContext()), false)),
                     {});
  builder.CreateUnreachable();
}

Value* compile(IRBuilder<>& builder,
               lexp& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit& unit,
               AstContext astContext) {
  auto& module = *builder.GetInsertBlock()->getModule();
  auto& ctx = module.getContext();
  auto recurse = [&, astContext] (lexp& e, std::optional<bool> last = std::nullopt) mutable {
    if (last.has_value())
      astContext.isFinalExpression = last.value();
    return compile(builder, e, variables, unit, astContext);
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
    case REAL: {
      auto i = get<REAL>(expression);
      return ConstantFP::get(Type::getDoubleTy(ctx), i);
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

      unit.paramFuncs[fn_var] = F->getName();

      auto env_array_type = ArrayType::get(genericPointerType(ctx), free_vars.size());
      auto wrapper_type = StructType::create({genericFunctionType(ctx)->getPointerTo(),
                                              env_array_type}, F->getName().str() + "Wrapper");

      auto aggregate_v = builder.CreateInsertValue(UndefValue::get(wrapper_type), F, {0}, "fun_ptr_slot"); // store function pointer...
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
      auto memory = createAllocation(module, builder, wrapper_type, false);
      builder.CreateStore(aggregate_v, memory);

      BasicBlock *BB = BasicBlock::Create(ctx, "entry", F);
      IRBuilder<> fun_builder(BB);

      std::map<PLambda::lvar, Value*> inner_variables;
      // Adapt free bindings from the enclosing expression
      {
        unsigned index = 0;
        auto env = std::next(F->arg_begin()); // second argument is the environment.
        for (auto var : free_vars)
          inner_variables[var] = fun_builder.CreateLoad(fun_builder.CreateConstGEP1_32(env, index++, "captured_ptr"), "captured");
      }
      inner_variables[fn_var] = F->arg_begin(); // first argument is the argument

      if (astContext.isBodyOfFixpoint)
        astContext.isBodyOfFixpoint = false;
      else
        astContext.fixPointVariable = -1;
      astContext.isFinalExpression = true;
      if (auto retv = compile(fun_builder, fn_body, inner_variables, unit, astContext))
        fun_builder.CreateRet(box(fun_builder, retv));

      return builder.CreatePointerCast(memory, genericPointerType(ctx), "clos_ptr_cast");
    }
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);
      if (body_exp.get().index() == VAR        // If the body is a variable,
       && get<VAR>(body_exp.get()) == let_var) // and if that variable is the LET variable,
          return recurse(assign_exp);    // just return the assignment.

      /*
        Perform inlining. If a function is only called once, substitute the function body as a let-expression.
      */
      if (auto function_exp = get_if<FN>(&assign_exp)) {
        auto freeOccursInLet = freeVarOccurrences(body_exp, &expression);
        auto [occur_first, occur_last] = freeOccursInLet.equal_range(let_var);
        if (std::distance(occur_first, occur_last) == 1) {
          auto [var, occurrence] = *occur_first;
          auto app_exp = get_if<APP>(occurrence.enclosing_exp);
          if (app_exp && &app_exp->first.get() == occurrence.holding_exp) {
            std::cout << "Found a single application of v" << var << '\n';
            // Replace the application with a LET fnparam = argument IN fnbody END
            auto argument = get<1>(*app_exp).get();
            occurrence.enclosing_exp->emplace<LET>(get<0>(*function_exp),
                                                   argument,
                                                   get<2>(*function_exp));
            return compile(builder, body_exp, variables, unit, astContext);
          }
        }
      }

      auto assign_value = recurse(assign_exp, false);
      auto inner_vars = variables;
      inner_vars.emplace(let_var, assign_value);
      return compile(builder, body_exp, inner_vars, unit, astContext);
    }
    case APP: {
      auto& [fn_exp, arg_exp] = get<APP>(expression);
      // Primitive operations require special treatment:
      if (fn_exp.get().index() == PRIM) {
        auto& [prim_oper,_1, _2] = get<PRIM>(fn_exp.get());
        return compile_primop(builder, prim_oper, arg_exp, variables, unit, astContext);
      }

      // The remaining cases are arbitrary function calls.
      Value* arg_val = recurse(arg_exp, false);

      FunctionType* fun_type = nullptr;
      if ( arg_val->getType()->isIntegerTy()) {
        fun_type = intFunctionType(module);
        arg_val = boxIntNoCast(builder, arg_val);
      }
      else
        fun_type = genericFunctionType(ctx);

      // Recursive call?
      if (fn_exp.get().index() == VAR
       && get<VAR>(fn_exp.get()) == astContext.fixPointVariable
       && astContext.isFinalExpression) {
        std::cout << "Replacing app of " << astContext.fixPointVariable << "\n";
        auto recursive_function = builder.GetInsertBlock()->getParent();

        auto result = builder.CreateCall(recursive_function,
                                         {arg_val, recursive_function->arg_begin()+1}, "recursive_call");
        result->mutateFunctionType(fun_type);
        builder.CreateRet(result);
        return nullptr;
      }

      auto fn_v = recurse(fn_exp, false);
      auto closure_ptr = builder.CreatePointerCast(fn_v, genericPointerType(ctx)->getPointerTo(), "closure_ptr");
      auto fn_ptr = builder.CreateLoad(builder.CreatePointerCast(closure_ptr, fun_type->getPointerTo()->getPointerTo(heapAddressSpace), "fun_ptr_ptr"), "fun_ptr");
      auto env_v = builder.CreateConstGEP1_32(closure_ptr, 1, "env_ptr");

      return builder.CreateCall(fn_ptr, {arg_val, env_v});
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
          inner_vars[var] = builder.CreateIntToPtr(ConstantInt::get(genericIntType(module), 0xDEADBEEF), genericPointerType(ctx));
        closure_prs.push_back(compile(builder, fn, inner_vars, unit,
                                      {.isBodyOfFixpoint = true,
                                       .fixPointVariable = var,
                                       .isFinalExpression = false}));
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
                                  pos+1, "closure_update_elem_ptr"));
          }
        }
        ++decl_index;
      }

      // Prepare environment and compilation of the body of the FIX expression:
      auto inner_vars = variables;
      for (std::size_t i = 0; i < decls.size(); ++i)
        inner_vars[get<lvar>(decls[i])] = closure_prs[i];
      return compile(builder, exp, inner_vars, unit, astContext);
    }

    case SWITCH: {
      auto& [switched_exp, cases, default_case] = get<SWITCH>(expression);
      auto func = builder.GetInsertBlock()->getParent();
      auto exit_block = BasicBlock::Create(ctx, "continue", func);
      BasicBlock* default_ = BasicBlock::Create(ctx, "default", func, exit_block);
      auto default_builder = std::make_unique<IRBuilder<>>(default_);

      std::vector<std::pair<std::unique_ptr<IRBuilder<>>, Value*>> results;

      //auto result = builder.CreateAlloca(genericPointerType(ctx), nullptr, "switch_result_ptr");
      //default_builder.CreateStore(ConstantPointerNull::get(genericPointerType(ctx)), result);
      if (default_case) {
        auto default_v = compile(*default_builder, default_case.value(), variables, unit, astContext);
        if (default_v) {
          results.emplace_back(std::move(default_builder), default_v);
        }
        else if (default_->getTerminator() == nullptr) // no terminator
          default_builder->CreateUnreachable();
      }
      else
        insertAbort(*default_builder);

      auto switched_v = recurse(switched_exp, false); // the switched expression is not final, but the cases might be
      auto switch_inst = builder.CreateSwitch(extractTag(builder, unit, switched_v, cases.at(0).first),
                                              default_, cases.size());
      for (auto& [constructor, exp] : cases) {
        auto case_bb = BasicBlock::Create(ctx, "case", func, exit_block);
        switch_inst->addCase(getTag(module, unit, constructor), case_bb);

        auto case_builder = std::make_unique<IRBuilder<>>(case_bb);
        auto vars = variables;
        if (auto datacon = get_if<DATAcon>(&constructor)) {
          auto [sym, var] = *datacon;
          switch (unit.symbolRepresentation.at(sym).type) {
            case symbol_rep::TAGGED:
              vars[var] = case_builder->CreatePointerCast(switched_v, genericPointerType(ctx)->getPointerTo(heapAddressSpace));
              break;
            case symbol_rep::UNTAGGED:
              vars[var] = switched_v;
              break;
            default:;
          }
        }

        // Result is zero when there is an early return within the produced code.
        if (Value* case_result = compile(*case_builder, exp, vars, unit, astContext)) {
          results.emplace_back(std::move(case_builder), case_result);
//          // If the result is an integer, cast the store.
//          if (case_result->getType()->isIntegerTy())
//            case_builder.CreateStore(boxIntNoCast(case_builder, case_result),
//                                     case_builder.CreatePointerCast(result, genericIntType(module)->getPointerTo()));
//          else
//            case_builder.CreateStore(case_result, result);

        }
      }

      builder.SetInsertPoint(exit_block);

      PHINode* result_node;

      if (std::any_of(begin(results), end(results), [] (auto& pair) {
        return pair.second->getType()->isIntegerTy();
      })) {
        result_node = builder.CreatePHI(genericIntType(module), results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(unboxInt(*result_builder, result), result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }
      else if (std::any_of(begin(results), end(results), [] (auto& pair) {
        return pair.second->getType()->isDoubleTy();
      })) {
        result_node = builder.CreatePHI(Type::getDoubleTy(ctx), results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(unboxReal(*result_builder, result), result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }
      else {
        result_node = builder.CreatePHI(genericPointerType(ctx), results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(result, result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }

      return result_node;
    }

    case CON: {
      auto& [constr, tycs, argument] = get<CON>(expression);
      auto symrep = unit.symbolRepresentation.at(get<Symbol::symbol>(constr));
      switch (symrep.type) {
        case symbol_rep::CONSTANT:
          return boxInt(builder, symrep.value);
        case symbol_rep::TAGGED: {
          auto rcd = recurse(argument, false);
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
      std::vector<Value*> values;
      astContext.isFinalExpression = false;
      for (auto& e : get<RECORD>(expression))
        values.push_back(recurse(e));
      if (values.empty())
        return ConstantPointerNull::get(genericPointerType(ctx));
      auto ptr = record(builder, values);
      return builder.CreatePointerCast(ptr, genericPointerType(ctx), "rcd_ptr");
    }
    case SRECORD: {
      std::vector<Value*> values;
      astContext.isFinalExpression = false;
      for (auto& e : get<SRECORD>(expression))
        values.push_back(recurse(e));
      auto ptr = record(builder, values);
      return builder.CreatePointerCast(ptr, genericPointerType(ctx)->getPointerTo(heapAddressSpace), "srcd_ptr");
    }

    case SELECT: {
      auto& [indices, record] = get<SELECT>(expression);
      if (indices.size() != 1)
        throw UnsupportedException{"Nested indexing"};
      auto record_v = builder.CreatePointerCast(recurse(record),
                                                genericPointerType(ctx)->getPointerTo(heapAddressSpace));
      return builder.CreateLoad(builder.CreateConstGEP1_32(record_v, indices[0] + 1, "selected_ptr"), "selected");
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
  auto v = compile(builder, unit.exportedLetExpr, {}, unit, {});
  builder.CreateStore(ConstantInt::get(Type::getInt32Ty(ctx), 0x777), getTagPtrFromRecordPtr(builder, v));
  builder.CreateRet(v);
}

}
