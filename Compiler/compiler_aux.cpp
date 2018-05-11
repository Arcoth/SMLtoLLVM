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

lvar createNewVarIndex(lvar var, int index);

Value* getTagPtrFromRecordPtr(IRBuilder<>& builder, Value* rcd);

Value* boxIntNoCast(IRBuilder<>& builder, Value* x) {
  return builder.CreateOr(builder.CreateShl(x, GC::valueFlagLength, "shifted_int"),
                          GC::intTag, "boxed_int");
}

Value* boxRealInInt(IRBuilder<>& builder, Value* x) {
  x = builder.CreateBitCast(x, genericIntType, "real_to_int");
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

  return builder.CreatePointerCast(x, genericPointerType, "box_ptr");
}

Value* unboxRealFromInt(IRBuilder<>& builder, Value* x) {
  auto& mod = *builder.GetInsertBlock()->getModule();
  return builder.CreateBitCast(builder.CreateAnd(x, ~GC::floatTag, "untagged_float"),
                               Type::getDoubleTy(mod.getContext()), "unboxed_real");
}

Value* unboxReal(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isDoubleTy())
    return x;
  x = builder.CreatePtrToInt(x, genericIntType, "float_in_int");
  return unboxRealFromInt(builder, x);
}

Value* compile(IRBuilder<>& builder,
               lexp& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit&,
               AstContext astContext);

Type* boxedType(Type* t) {
  return t->isDoubleTy()? genericIntType : t;
}
Value* unboxedValue(IRBuilder<>& builder, Value* x) {
  if (x->getType()->isIntegerTy())
    return builder.CreateAShr(x, GC::valueFlagLength, "unboxed_int");
  throw CompileFailException{"Unexpected unboxedValue call on non-int"};
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
  return unboxedValue(builder, builder.CreatePtrToInt(x, genericIntType, "ptr_to_int"));
}


// To cover regular and list-CPS cases
void yieldValue(IRBuilder<>& builder, Value* v, AstContext astContext) {
  if (astContext.isListCPSFunction) {
    builder.CreateStore(v, builder.CreateConstGEP1_32(builder.GetInsertBlock()->getParent()->arg_end()-1, 2, "list_cps_slot"));
    builder.CreateRetVoid();
  }
  else
    builder.CreateRet(box(builder, v));
}

auto freeVarOccurrences(vector<lexp>& exps, lexp* enclosing, SMLTranslationUnit const& unit) {
  std::multimap<lvar, Occurrence> s;
  for (auto& e : exps)
    s = set_union(s, freeVarOccurrences(e, enclosing, unit));
  return s;
}

void performReduction(lexp& expression, SMLTranslationUnit const& unit);
void performReduction(vector<lexp>& exps, SMLTranslationUnit const& unit) {
  for (auto& x : exps)
    performReduction(x, unit);
}
// Perform simplifications up to recursive function definitions.
void performReduction(lexp& expression, SMLTranslationUnit const& unit) {
  switch(expression.index()) {
    case VAR: {
      auto& v = get<VAR>(expression);
      if (unit.assignedConstant.count(v)) {
        auto&& var = unit.assignedConstant.at(v);
        if (auto x = get_if<ConstantData*>(&var))
          expression.emplace<CONSTANT>(*x);
      }
    }
    break;
    case FIX: {
      auto& [decls, in] = get<FIX>(expression);
      performReduction(in, unit);
    }
    break;
    case APP: {
      auto& [fun, arg] = get<APP>(expression);
      if (auto lvar_ptr = get_if<VAR>(&fun); lvar_ptr && unit.assignedConstant.count(*lvar_ptr)) {
        auto fn = get<Function*>(unit.assignedConstant.at(*lvar_ptr));
        auto iter = std::find_if(unit.closureLength.rbegin(), unit.closureLength.rend(), [fn] (auto const& pair) {
          return pair.first == fn;
        });
        assert(iter != unit.closureLength.rend());
        // No closure
        auto var = *lvar_ptr;
        fun.get().emplace<FUNCTION>(fn,
                                    iter->second > 1? var : 0);
      }
      else
        performReduction(fun, unit);
      performReduction(arg, unit);
    }
    break;
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);
      performReduction(assign_exp, unit);
      performReduction(body_exp, unit);
      {
        auto freeOccursInLet = freeVarOccurrences(body_exp, &expression, unit);
        auto [occur_first, occur_last] = freeOccursInLet.equal_range(let_var);
        if (std::distance(occur_first, occur_last) == 0 && get_if<FN>(&assign_exp))
        {
          expression = lexp{body_exp};
          return;
        }
        if (std::distance(occur_first, occur_last) == 1)
        {
          auto [var, occurrence] = *occur_first;

          /*
            Perform eta-reduction.
            If a function is only called once, substitute the function body as a let-expression.
          */
          bool reduced = false;
          if (auto function_exp = get_if<FN>(&assign_exp))
          if (auto app_exp = get_if<APP>(occurrence.enclosing_exp);
              app_exp && &app_exp->first.get() == occurrence.holding_exp)
          {
            reduced = true;
            // Replace the application with a LET fnparam = argument IN fnbody END
            auto argument = get<1>(*app_exp).get();
            occurrence.enclosing_exp->emplace<LET>(get<0>(*function_exp),
                                                   argument,
                                                   get<2>(*function_exp));
          }


          if (!reduced
           && (occurrence.enclosing_exp == &expression        // immediately enclosed by this exp,
            || assign_exp.get().index() == PRIM               // a primitive operator declaration (no side effects),
            || ((occurrence.enclosing_exp->index() == CON
              || occurrence.enclosing_exp->index() == RECORD)
             && occurrence.enclosing_exp == &body_exp))) {       // or a argument of an immediately enclosed constructor
            *occur_first->second.holding_exp = assign_exp;
            reduced = true;
          }

          // If either of the reductions has been performed:
          if (reduced) {
            performReduction(expression = lexp{body_exp}, unit);
            return;
          }
        }
        // If selecting from imports, push the selects inwards
        if (auto select_exp = get_if<SELECT>(&assign_exp)) {
          auto& [indices, rec_exp] = *select_exp;
          if (auto var_p = get_if<VAR>(&rec_exp.get()); var_p && *var_p == unit.importVariable) {
            for (auto it = occur_first; it != occur_last; ++it)
              *it->second.holding_exp = lexp{*select_exp};
            performReduction(expression = lexp{body_exp}, unit);
            return;
          }
        }
      }

      // If a record is only selected from, we can avoid constructing it in the first place.
      if (auto record_exp = get_if<RECORD>(&assign_exp)) {
        auto freeOccursInLet = freeVarOccurrences(body_exp, &expression, unit);
        auto [occur_first, occur_last] = freeOccursInLet.equal_range(let_var);
        bool onlySelects = std::all_of(occur_first, occur_last, [] (auto& o) {
          return o.second.enclosing_exp->index() == SELECT;
        });
        if (onlySelects) {
          for (auto it = occur_first; it != occur_last; ++it) {
            auto& [indices, _] = get<SELECT>(*it->second.enclosing_exp);
            assert(indices.size() == 1);
            auto index = indices[0];
            auto new_index = createNewVarIndex(let_var, index);
            // replace the SELECT with  a reference to the new variable we define below
            it->second.enclosing_exp->emplace<VAR>(new_index);
          }
          lexp result = body_exp;
          int elem_index = 0;
          for (auto& rcd_elem : *record_exp) {
            result.emplace<LET>(createNewVarIndex(let_var, elem_index++),
                           lexp{rcd_elem},
                           lexp{result});
          }
          expression = lexp{result};
        }
      }
    }
    break;
    case SWITCH: {
      auto& [arg, matches, default_] = get<SWITCH>(expression);
      performReduction(arg, unit);
      for (auto& [_, target] : matches)
        performReduction(target, unit);
      if (default_)
        performReduction(default_.value(), unit);
    }
    break;
    case CON: {
      auto& [x, y, arg] = get<CON>(expression);
      performReduction(arg, unit);
    }
    break;
    // Simple compositional cases:

    case HANDLE: return performReduction(get<0>(    get<HANDLE>(expression)), unit);
    case ETAG:   return performReduction(get<dlexp>(  get<ETAG>(expression)), unit);
    case SELECT: return performReduction(get<dlexp>(get<SELECT>(expression)), unit);
    case PACK:   return performReduction(get<dlexp>(  get<PACK>(expression)), unit);
    case WRAP:   return performReduction(get<dlexp>(  get<WRAP>(expression)), unit);
    case UNWRAP: return performReduction(get<dlexp>(get<UNWRAP>(expression)), unit);
    // Elide this information to ease optimisation
    case TFN:
      expression = lexp{get<dlexp>(get<TFN>(expression))};
      return performReduction(expression, unit);
    case TAPP:
      expression = lexp{get<dlexp>(get<TAPP>(expression))};
      return performReduction(expression, unit);


    case RECORD:  return performReduction(get<RECORD>(expression), unit);
    case SRECORD: return performReduction(get<SRECORD>(expression), unit);
    case VECTOR: return performReduction(get<0>(get<VECTOR>(expression)), unit);


    default:
      ;
  }
}

std::multimap<lvar, Occurrence> freeVarOccurrences(lexp& exp, lexp* enclosing, SMLTranslationUnit const& unit) {
  switch(exp.index()) {
    case INT: case INT32: case WORD:
    case WORD32: case REAL: case PRIM:
    case STRING: case GENOP: case CONSTANT:
      return {};

    case FUNCTION: {
      auto v = get<lvar>(get<FUNCTION>(exp));
      if (v == 0)
        return {};
      return {{v, {.enclosing_exp = enclosing, .holding_exp = &exp}}};
    }

    case VAR:
      if (get<VAR>(exp) != unit.importVariable)
        return {{get<VAR>(exp), {.enclosing_exp = enclosing, .holding_exp = &exp}}};
      return {};
    case FN: {
      auto& fn = get<FN>(exp);
      return set_subtract(freeVarOccurrences(get<dlexp>(fn), &exp, unit), {get<lvar>(fn)});
    }
    case FIX: {
      auto& [decls, in] = get<FIX>(exp);
      std::multimap<lvar, Occurrence> freevars;
      std::set<lvar> params;
      for (auto& [var, _, body] : decls) {
        freevars = set_union(freevars, freeVarOccurrences(body, &exp, unit));
        params.insert(var);
      }
      return set_subtract(freevars, params);
    }
    case APP: {
      auto& [fun, arg] = get<APP>(exp);
      return set_union(freeVarOccurrences(fun, &exp, unit),
                       freeVarOccurrences(arg, &exp, unit));
    }
    case LET: {
      auto& [var, assign, in] = get<LET>(exp);
      return set_union(freeVarOccurrences(assign, &exp, unit),
                       set_subtract(freeVarOccurrences(in, &exp, unit), {var}));
    }
    case SWITCH: {
      auto& [arg, matches, default_] = get<SWITCH>(exp);
      auto s = freeVarOccurrences(arg, &exp, unit);
      for (auto& [constructor, target] : matches) {
        auto exp_vars = freeVarOccurrences(target, &exp, unit);
        if (constructor.index() == DATAcon)
          exp_vars = set_subtract(exp_vars, {get<lvar>(get<DATAcon>(constructor))});
        s = set_union(s, exp_vars);
      }
      if (default_)
        s = set_union(s, freeVarOccurrences(default_.value(), &exp, unit));

      return s;
    }
    case CON: {
      using namespace Access;

      auto& [constr, tycs, arg] = get<CON>(exp);
      auto fvs = freeVarOccurrences(arg, &exp, unit);
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

    case HANDLE: return freeVarOccurrences(get<0>(    get<HANDLE>(exp)), &exp, unit);
    case ETAG:   return freeVarOccurrences(get<dlexp>(  get<ETAG>(exp)), &exp, unit);
    case SELECT: return freeVarOccurrences(get<dlexp>(get<SELECT>(exp)), &exp, unit);
    case PACK:   return freeVarOccurrences(get<dlexp>(  get<PACK>(exp)), &exp, unit);
    case WRAP:   return freeVarOccurrences(get<dlexp>(  get<WRAP>(exp)), &exp, unit);
    case UNWRAP: return freeVarOccurrences(get<dlexp>(get<UNWRAP>(exp)), &exp, unit);
    case TFN:    return freeVarOccurrences(get<dlexp>(   get<TFN>(exp)), &exp, unit);
    case TAPP:   return freeVarOccurrences(get<dlexp>(  get<TAPP>(exp)), &exp, unit);


    case RECORD:  return freeVarOccurrences( get<RECORD>(exp), &exp, unit);
    case SRECORD: return freeVarOccurrences(get<SRECORD>(exp), &exp, unit);
    case VECTOR: return freeVarOccurrences(get<0>(get<VECTOR>(exp)), &exp, unit);


    default:
      throw UnsupportedException{"Unsupported lambda type " + std::to_string(exp.index())};
  }
}

std::set<lvar> freeVars( lexp const& exp, SMLTranslationUnit const& unit ) {
  auto occurrences = freeVarOccurrences(const_cast<lexp&>(exp), nullptr, unit);
  std::set<lvar> vars;
  for (auto it = occurrences.begin(), end = occurrences.end(); it != end; it = occurrences.upper_bound(it->first))
    vars.insert(it->first);
  return vars;
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
  static const auto size_type = genericIntType;
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

ConstantInt* getTag(SMLTranslationUnit const& unit, con const& constr) {
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
  return ConstantInt::get(genericIntType, value);
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

bool isPrim(tyc const& t) {
  return t.index() == TC_PRIM;
}
// 0 if not, length > 1 if true
std::size_t isAggr(tyc const& t) {
  return t.index() == TC_TUPLE? get<TC_TUPLE>(t).size() : 0;
}
std::size_t isPrimAggr(tyc const& t) {
  if (auto tup = std::get_if<TC_TUPLE>(&t))
    return std::all_of(tup->begin(), tup->end(), isPrim)? tup->size() : 0;
  return 0;
}

Type* LLVMTypeFromTYC(tyc const& t) {
  if (auto primtyc = get_if<TC_PRIM>(&t)) {
    if (*primtyc == PrimTyc::PT_INT31
     || *primtyc == PrimTyc::PT_INT32)
      return genericIntType;
    else if (*primtyc == PrimTyc::PT_REAL)
      return realType;
  }
  return genericPointerType;
}

std::vector<Type*> LLVMTypesFromTYC(tyc const& t) {

  if (auto tup = std::get_if<TC_TUPLE>(&t)) {
    std::vector<Type*> v;
    for (auto&& x : *tup)
      v.push_back(LLVMTypeFromTYC(x));
    return v;
  }
  throw std::invalid_argument{"LLVMTypesFromTYC: expected tyc-tuple!"};
}

void unboxForUnboxedCall(IRBuilder<>& builder, std::vector<Value*>& values, Function* targetFn) {
  std::size_t index = 0;
  for (auto& v : values) {
    if (targetFn->arg_begin()[index].getType()->isIntegerTy())
      v = unboxInt(builder, v);
    else if (targetFn->arg_begin()[index].getType()->isDoubleTy())
      v = unboxReal(builder, v);
    ++index;
  }
}


std::vector<Value*> decomposeRecord(IRBuilder<>& builder, Value* rec, std::size_t len) {
  auto base = builder.CreatePointerCast(rec, genericPtrToPtr, "record_ptr");
  std::vector<Value*> arguments;
  // Construct the argument set when delegating to the record function
  for (std::size_t i = 0; i < len; ++i)
    arguments.push_back(builder.CreateLoad(builder.CreateConstGEP1_32(base, i+1, "elem_ptr"), "elem"));
  return arguments;
}

Value* compileFunction(IRBuilder<>* builder,
                       lexp& original_expression,
                       std::map<PLambda::lvar, Value*> const& variables,
                       SMLTranslationUnit& unit,
                       AstContext astContext) {
  auto& module = *unit.module;
  auto expression = original_expression;

  auto& [fn_var, fn_lty, fn_body] = get<FN>(expression);

  char const* name = astContext.moduleExportExpression? "export" : nameForFunction(fn_var, astContext.isListCPSFunction, astContext.isRecordFunction);

  FunctionType* fun_type;
  if (astContext.moduleExportExpression)
    fun_type = FunctionType::get(genericPointerType, {genericPointerType}, false);
  else
    fun_type = genericFunctionType(astContext.isListCPSFunction);

  // Create the hoisted function.
  auto ImplementationFunction = astContext.isListCPSFunction? nullptr
                              : Function::Create(fun_type, Function::ExternalLinkage, name, &module),
       InterfaceFunction = ImplementationFunction;

  unit.paramFuncs[fn_var] = name;

  std::optional<IRBuilder<>> fun_builder_opt;
  if (ImplementationFunction)
    fun_builder_opt.emplace(BasicBlock::Create(context, "entry", ImplementationFunction));


  std::map<PLambda::lvar, Value*> inner_variables;

  if (astContext.moduleExportExpression) {
    GlobalVariable* imports_gvar = new GlobalVariable(/*Module=*/*unit.module,
            /*Type=*/genericPtrToPtr,
            /*isConstant=*/false,
            /*Linkage=*/GlobalValue::PrivateLinkage,
            /*Initializer=*/ConstantPointerNull::get(genericPtrToPtr),
            /*Name=*/"imports");
    fun_builder_opt.value().CreateStore(
      fun_builder_opt.value().CreatePointerCast(ImplementationFunction->arg_begin(), genericPtrToPtr),
      imports_gvar); // store the imports in the record
    inner_variables[fn_var] = imports_gvar;
    unit.importVariable = fn_var;
  }
  else
    inner_variables[unit.importVariable] = variables.at(unit.importVariable);

  performReduction(fn_body, unit);
  performReduction(get<2>(get<FN>(original_expression)), unit);

  const auto free_vars = freeVars(expression, unit); // Determine free vars after reduction

  //! Cover record functions
  if (auto tyc = get_if<LT_TYC>(&fn_lty))
  if (auto len = isAggr(*tyc)) {
    auto freeOccursInLet = freeVarOccurrences(fn_body, &expression, unit);
    auto [occur_first, occur_last] = freeOccursInLet.equal_range(fn_var);
    bool onlySelects = std::all_of(occur_first, occur_last, [] (auto& o) {
      return o.second.enclosing_exp->index() == SELECT;
    });
    if (onlySelects)
    {
      astContext.isRecordFunction = len;
      // The record function with N args
      std::vector<Type*> types = LLVMTypesFromTYC(*tyc);
      types.push_back(genericPtrToPtr); // env
      if (astContext.isListCPSFunction)
        types.push_back(genericPtrToPtr); // CPS

      ImplementationFunction = Function::Create(FunctionType::get(astContext.isListCPSFunction? Type::getVoidTy(context) : genericPointerType, types, false), Function::InternalLinkage,
                                       nameForFunction(fn_var, astContext.isListCPSFunction, true), &module);

      if (InterfaceFunction) {
        auto arguments = decomposeRecord(fun_builder_opt.value(), InterfaceFunction->arg_begin(), len);
        unboxForUnboxedCall(fun_builder_opt.value(), arguments, ImplementationFunction);
        arguments.push_back(InterfaceFunction->arg_begin()+1);
        if (astContext.isListCPSFunction)
          arguments.push_back(InterfaceFunction->arg_begin()+2);

        yieldValue(fun_builder_opt.value(), fun_builder_opt.value().CreateCall(ImplementationFunction, arguments, "propagate"), astContext);
      }
      else
        InterfaceFunction = ImplementationFunction;

      // The remaining code is using the wrapper function instead
      fun_builder_opt.emplace(BasicBlock::Create(context, "entry_rec", ImplementationFunction));

      // Replace all uses of the record argument
      for (auto it = occur_first; it != occur_last; ++it) {
        auto& [indices, _] = get<SELECT>(*it->second.enclosing_exp);
        assert(indices.size() == 1);
        auto index = indices[0];
        auto new_index = createNewVarIndex(fn_var, index);
        inner_variables[new_index] = ImplementationFunction->arg_begin() + index;
        // replace the SELECT with  a reference to the new variable we define below
        it->second.enclosing_exp->emplace<VAR>(new_index);
      }
    }
  }

  unit.closureLength.emplace_back(InterfaceFunction, free_vars.size()+1); // Add the closure length after potentially replacing ImplementationFunction
  unit.closureLength.emplace_back(ImplementationFunction, free_vars.size()+1); // .. for both functions.

  IRBuilder<>& fun_builder = fun_builder_opt.value();

  if (!astContext.isRecordFunction && !astContext.moduleExportExpression)
   inner_variables[fn_var] = ImplementationFunction->arg_begin();


  // Adapt free bindings from the enclosing expression
  {
    unsigned index = 1; //! Start at 1; we pass the closure in without offset.
    auto env = ImplementationFunction->arg_end()-1-astContext.isListCPSFunction; // environment argument
    for (auto var : free_vars)
      inner_variables[var] = fun_builder.CreateLoad(fun_builder.CreateConstGEP1_32(env, index++, "captured_ptr"), "captured");
  }

  std::vector<Type*> types{fun_type->getPointerTo()};
  for (auto var : free_vars) {
    auto var_it = variables.find(var);
    if (var_it == variables.end())
      throw CompileFailException{"FN: Captured variable " + std::to_string(var) + " not defined in enclosing scope"};
    types.push_back(boxedType(var_it->second->getType()));
  }

  if (astContext.isFunctionOfFixPoint)
    astContext.isFunctionOfFixPoint = false;
  else
    astContext.fixPoint = std::nullopt;
  astContext.isFinalExpression = true;

  AstContext newAstCtx = astContext;
  newAstCtx.enclosingFunctionExpr = &original_expression;
  newAstCtx.moduleExportExpression = false;
  newAstCtx.isSolelyApplied = false;
  if (auto retv = compile(fun_builder, fn_body, inner_variables, unit, newAstCtx))
    yieldValue(fun_builder, retv, astContext);

  if (astContext.isListCPSFunction
   || (astContext.isSolelyApplied && free_vars.empty())) // If the function's closure is never needed, need not create it!
    return InterfaceFunction;
  if (!astContext.moduleExportExpression) {
    assert(builder && "Builder must be non-zero!");

    auto wrapper_type = StructType::create(types);
    auto memory = createAllocation(module, *builder, wrapper_type);

    auto aggregate_v = builder->CreateInsertValue(UndefValue::get(wrapper_type), InterfaceFunction, {0}, "fun_ptr_slot"); // store function pointer...
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
