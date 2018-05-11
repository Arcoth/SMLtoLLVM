#include "compile.hpp"
#include "GCPlugin/GCBasicConstants.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/raw_ostream.h>

#include <iostream>
#include <optional>
#include <iterator>

// Enable the CPS optimisation for recursive cons's.
#define ENABLE_CONS_CPS_OPT

namespace SMLCompiler {

using std::get;
using std::get_if;

using namespace PLambda;


std::multimap<lvar, Occurrence> freeVarOccurrences(vector<lexp>& exps, lexp* enclosing, SMLTranslationUnit const&);
std::multimap<lvar, Occurrence> freeVarOccurrences( lexp& exp, lexp* enclosing, SMLTranslationUnit const&);
std::set<lvar> freeVars( lexp const& exp, SMLTranslationUnit const& );
lexp const* outermostClosedLet(lexp const& exp);
Value* createAllocation(Module& module, IRBuilder<>& builder, Type* type,
                        GC::Heap heap = GC::Heap::Young, std::size_t n = 1);
Value* extractTag(IRBuilder<>& builder, SMLTranslationUnit const& unit, Value* exp_v, con const& constr);
ConstantInt* getTag(SMLTranslationUnit const& unit, con const& constr);


Value* boxIntNoCast(IRBuilder<>& builder, Value* x);
Value* boxInt(IRBuilder<>& builder, Value* x);
Value* boxRealInInt(IRBuilder<>& builder, Value* x);
Value* boxReal(IRBuilder<>& builder, Value* x);
Value* box(IRBuilder<>& builder, Value* x);
Value* boxedValue(IRBuilder<>& builder, Value* value);

std::vector<Value*> decomposeRecord(IRBuilder<>& builder, Value* rec, std::size_t len) ;

lvar createNewVarIndex(lvar var, int index) {
  return (var << (sizeof(lvar)*8/2)) + index;
}

// Cast the second argument to a pointer to int if the first one is an int.
// Return appropriately casted pointer to stored location.
Value* storeValue(IRBuilder<>& builder, Value* value, Value* ptr) {
  if (value->getType()->isIntegerTy()) {
    ptr = builder.CreatePointerCast(ptr, value->getType()->getPointerTo(heapAddressSpace), "int_store");
    builder.CreateStore(boxIntNoCast(builder, value), ptr);
  }
  else if (value->getType()->isDoubleTy()) {
    ptr = builder.CreatePointerCast(ptr, genericIntType->getPointerTo(heapAddressSpace), "float_in_int_store");
    builder.CreateStore(boxRealInInt(builder, value), ptr);
  }
  else
    builder.CreateStore(value, ptr);
  return ptr;
}

Type* boxedType(Type* t);

Value* unboxedValue(IRBuilder<>& builder, Value* x);

// expects a value of generic pointer tpye,
Value* unboxInt(IRBuilder<>& builder, Value* x);

Value* unboxRealFromInt(IRBuilder<>& builder, Value* x);

Value* unboxReal(IRBuilder<>& builder, Value* x);

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
    if (!v->getType()->isIntegerTy()
     && !v->getType()->isDoubleTy()) {
      auto v_ptr = builder.CreatePointerCast(v, genericPtrToPtr, "primop_rcd_ptr");
      for (int i = 0; i < 2; ++i)
        arg_values.push_back([i, v_ptr, &builder] {return builder.CreateLoad(builder.CreateConstGEP1_32(v_ptr, i+1), "primop_rcd_elem");});
    }
  }

  auto unbox_args = [&] (bool isFloat) {
    arg_value = [&builder, arg_value, isFloat] {return isFloat? unboxReal(builder, arg_value()) : unboxInt(builder, arg_value());;};
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
        #define PRIMOP_BIN_F(fun) \
          assert_arity(arg_values, 2); assert(isFloat); \
          result = builder.Create##fun(arg_values[0](), arg_values[1](), "float_" #fun); \
          break;
        #define PRIMOP_BIN_I(fun) \
          assert_arity(arg_values, 2); assert(!isFloat); \
          result = builder.Create##fun(arg_values[0](), arg_values[1](), "int_" #fun); \
          break;
        #define PRIMOP_BIN(fun) \
          assert_arity(arg_values, 2); \
          result = isFloat? builder.CreateF##fun(arg_values[0](), arg_values[1](), "float_" #fun) \
                          : builder.Create##fun(arg_values[0](), arg_values[1](), "int_" #fun); \
          break;
        case MUL: PRIMOP_BIN(Mul)
        case ADD: PRIMOP_BIN(Add)
        case SUB: PRIMOP_BIN(Sub)
        case DIV: PRIMOP_BIN_I(SDiv)
        case NEG:
          result = isFloat? builder.CreateFNeg(arg_value(), "float_Neg")
                          : builder.CreateNeg(arg_value(), "int_Neg");
          break;

        case FDIV: PRIMOP_BIN_F(FDiv)

        case FSIN:
          return builder.CreateCall(module.getOrInsertFunction("sin", FunctionType::get(realType, {realType}, false)), arg_value());
        case FCOS:
          return builder.CreateCall(module.getOrInsertFunction("cos", FunctionType::get(realType, {realType}, false)), arg_value());
        case FTAN:
          return builder.CreateCall(module.getOrInsertFunction("tan", FunctionType::get(realType, {realType}, false)), arg_value());
        case FSQRT:
          return builder.CreateCall(module.getOrInsertFunction("sqrt", FunctionType::get(realType, {realType}, false)), arg_value());

        default:
          throw UnsupportedException{"Arithmetic operator: " + std::to_string(oper)};
        #undef PRIMOP_BIN
      }
      break;
    }
    case CMP: {
      auto& cmp = get<CMP>(op);
      const bool isFloat = cmp.kind.index() == Primop::FLOAT;
      // Scalars are stored unboxed:
      unbox_args(isFloat);
      #define COMP(fltname, intname) \
        result = builder.CreateIntCast(isFloat? builder.CreateFCmp(CmpInst::FCMP_##fltname, arg_values[0](), arg_values[1](), "float_comp") \
                                              : builder.CreateICmp(CmpInst::ICMP_##intname, arg_values[0](), arg_values[1](), "int_comp"),  \
                                       genericIntType, false, "bool_to_int"); break;
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

        // Test whether the integer represented by the float is less than zero
        case FSGN: {
          auto t = Type::getInt64Ty(context);
          return builder.CreateICmp(CmpInst::ICMP_SLT, builder.CreateBitCast(arg_value(), t, "real_to_int"), ConstantInt::get(t, 0), "less_than_zero");
        }
        default:
          throw UnsupportedException{"Comparison operator: " + std::to_string(cmp.oper)};
      }

      #undef COMP
      break;
    }
    case DEREF: {
      return builder.CreateLoad(builder.CreatePointerCast(arg_value(),
                                                          genericPtrToPtr, "deref_ptr"), "deref_val");
    }
    case MAKEREF: {
      auto val = arg_value();
      result = createAllocation(module, builder, val->getType(), GC::Heap::Mutable);
      storeValue(builder, val, result);
      return builder.CreatePointerCast(result, genericPointerType, "ref_ptr");
    }
    case ASSIGN: {
      auto lhs = arg_values[0](),
           rhs = arg_values[1]();
      lhs = builder.CreatePointerCast(lhs, rhs->getType()->getPointerTo(heapAddressSpace), "assign_casted_ptr");
      storeValue(builder, rhs, lhs);
      return ConstantPointerNull::get(genericPointerType);
    }
    case Primop::REAL: {
      unbox_args(false);
      return builder.CreateSIToFP(arg_value(), Type::getDoubleTy(module.getContext()), "int_to_double");
    }
    default:
      throw UnsupportedException{"primop " + std::to_string(op.index())};
  }
  return result;
}

// the first 64 bit is the tag!
template <typename Rng>
Value* record(IRBuilder<>& builder, Rng const& values, GC::Heap heap = GC::Heap::Young ) {
  auto& module = *builder.GetInsertBlock()->getModule();

  // The type used to hold the results while they're being computed.
  std::vector<Type*> record_elem_types{genericIntType};
  for (auto x : values)
    record_elem_types.push_back(boxedType(x->getType()));
  auto record_type = StructType::create(record_elem_types);

  assert(DataLayout{&module}.getTypeAllocSize(record_type)== std::size(record_elem_types) * 8);

  //! DON'T ALLOCATE memory first; first compute values, then the heap array to store them in.
  auto storage = createAllocation(module, builder, record_type, heap);

  auto tag_v = ConstantInt::get(genericIntType, (std::size(record_elem_types) << GC::recordFlagLength) | GC::lengthTag
                                                      | ((uint64_t)0xBEEF << 32));
  auto aggregate_v = builder.CreateInsertValue(UndefValue::get(record_type), tag_v, {0}, "record_tag");
  for (unsigned i = 0; i < std::size(values); ++i)
    aggregate_v = builder.CreateInsertValue(aggregate_v, boxedValue(builder, values[i]), {i+1}, "record_value");

  builder.CreateStore(aggregate_v, storage);

  return builder.CreatePointerCast(storage, genericPointerType, "record_ptr");
}

template <std::size_t N>
Value* record(IRBuilder<>& builder, Value* const (&values)[N], GC::Heap heap = GC::Heap::Young ){
  return record<decltype(values)>(builder, values, heap);
}

Value* getTagPtrFromRecordPtr(IRBuilder<>& builder, Value* rcd) {
  auto& module = *builder.GetInsertBlock()->getModule();
  return builder.CreateConstGEP1_32(builder.CreatePointerCast(rcd, tagType(module)->getPointerTo(heapAddressSpace), "tag_ptr_base"), 1,
                                    "tag_ptr");
}

void insertAbort(IRBuilder<>& builder) {
  auto& module = *builder.GetInsertBlock()->getModule();
  builder.CreateCall(module.getOrInsertFunction("abort",
                           FunctionType::get(Type::getVoidTy(module.getContext()), false)),
                     {});
  builder.CreateUnreachable();
}

bool isFixpointVar(lexp& fn_exp, AstContext const& astContext) {
  return fn_exp.index() == VAR
      && astContext.fixPoint && get<VAR>(fn_exp) == astContext.fixPoint.value().variable;
}

// To cover regular and list-CPS cases
void yieldValue(IRBuilder<>& builder, Value* v, AstContext astContext);

Value* compileFunction(IRBuilder<>* builder,
                       lexp& expression,
                       std::map<PLambda::lvar, Value*> const& variables,
                       SMLTranslationUnit& unit,
                       AstContext astContext);


using namespace Lty;

bool isPrim(tyc const& t);
std::size_t isPrimAggr(tyc const& t);

void unboxForUnboxedCall(IRBuilder<>& builder, std::vector<Value*>& values, Function* targetFn);

Value* compile(IRBuilder<>& builder,
               lexp& expression,
               std::map<PLambda::lvar, Value*> const& variables,
               SMLTranslationUnit& unit,
               AstContext astContext) {
  Module& module = *unit.module;

  auto recurse = [&] (lexp& e, std::optional<bool> last = std::nullopt) mutable {
    auto context = astContext;
    if (last.has_value())
      context.isFinalExpression = last.value();
    return compile(builder, e, variables, unit, context);
  };

  auto establish_recursive_arguments = [&] (Function* f, lexp& arg_exp) {
    std::vector<Value*> recursive_arguments;
    if (auto len = astContext.isRecordFunction)
      if (arg_exp.index() == RECORD)
        for (auto& e : get<RECORD>(arg_exp))
          recursive_arguments.push_back(recurse(e, false));
      else
        recursive_arguments = decomposeRecord(builder, recurse(arg_exp, false), len);
    else
      recursive_arguments.push_back(box(builder, recurse(arg_exp, false)));
    if (astContext.isListCPSFunction) // If the last argument is the CPS one, the penultimate is propagated, too
      recursive_arguments.push_back(f->arg_end()-2);
    recursive_arguments.push_back(f->arg_end()-1);
    return recursive_arguments;
  };

  switch (expression.index()) {
    case CONSTANT: {
      return get<CONSTANT>(expression);
    }
    case VAR: {
      auto var = get<VAR>(expression);
      if (auto iter = unit.assignedConstant.find(var);
          iter != unit.assignedConstant.end())
        if (auto c = get_if<ConstantData*>(&iter->second))
          return *c;
      auto iter = variables.find(var);
      if (iter == variables.end())
        throw CompileFailException{"Variable not in scope: " + std::to_string(var)};
      return iter->second;
    }
    case INT: {
      auto i = get<INT>(expression);
      return ConstantInt::getSigned(genericIntType, i);
    }
    case WORD: {
      auto i = get<WORD>(expression);
      return ConstantInt::get(genericIntType, i);
    }
    case REAL: {
      auto i = get<REAL>(expression);
      return ConstantFP::get(realType, i);
    }
    case STRING: {
      auto& s = get<STRING>(expression);
      auto str_const = ConstantDataArray::getString(context, s, false); // Don't need the null
      auto type = StructType::get(context, {tagType(module), tagType(module), str_const->getType()});
      GlobalVariable* gvar = new GlobalVariable(module,
              type,
              true,
              GlobalValue::InternalLinkage,
              ConstantStruct::get(type, {ConstantInt::get(tagType(module), (allocationSizeOf(&module, type)/8 << GC::recordFlagLength) | GC::lengthTag),
                                         ConstantInt::get(tagType(module), s.length()),
                                         str_const}),
              "string");

      return gvar;
    }
    case FN: {
      astContext.isListCPSFunction = false; // We can do this here, as the CPS code below calls compileFunction directly
      astContext.isRecordFunction = 0; // We can do this here, as the CPS code below calls compileFunction directly
      return compileFunction(&builder, expression, variables, unit, astContext);
    }
    case LET: {
      auto& [let_var, assign_exp, body_exp] = get<LET>(expression);

      {
        auto fnOccurInLet = freeVarOccurrences(body_exp, &expression, unit);
        auto [occur_first, occur_last] = fnOccurInLet.equal_range(let_var);
        if (get_if<FN>(&assign_exp) && std::all_of(occur_first, occur_last, [] (auto& o) {
          auto app = get_if<APP>(o.second.enclosing_exp);
          if (app)
            return app && &get<0>(*app) == o.second.holding_exp;
          return false;
        }))
          astContext.isSolelyApplied = true;
      }

      //! Compile this first, need functions to exist in module
      auto assign_value = recurse(assign_exp, false);
      astContext.isSolelyApplied = false;

      {
        // Walk down the let chain
        lexp* bottom = &assign_exp;
        while (auto let = get_if<LET>(bottom))
          bottom = &get<2>(*let).get();
        if (auto srcd = get_if<SRECORD>(bottom)) {
          auto& srcd_assignment = unit.assignedSRecords[let_var];
          std::size_t i = 0;
          for (auto& e : *srcd) {
            if (auto var = get_if<VAR>(&e);
                var && unit.assignedConstant.count(*var)) {
              srcd_assignment[i] = unit.assignedConstant[*var];
            }
            ++i;
          }
        }
      }

      if (auto select = get_if<SELECT>(&assign_exp))
        if (auto var = get_if<VAR>(&get<dlexp>(*select).get()))
          if (unit.assignedSRecords.count(*var)
          &&  unit.assignedSRecords[*var].count(get<0>(*select)[0])) {
            unit.assignedConstant[let_var] = unit.assignedSRecords[*var][get<0>(*select)[0]];
          }

      if (auto i = get_if<INT>(&assign_exp))
        unit.assignedConstant[let_var] = ConstantInt::get(genericIntType, *i);
      if (auto f = get_if<REAL>(&assign_exp))
        unit.assignedConstant[let_var] = cast<ConstantData>(ConstantFP::get(realType, *f));
      if (auto fn = get_if<FN>(&assign_exp)) {
        auto p = module.getFunction(nameForFunction(get<lvar>(*fn)));
        assert(p);
        unit.assignedConstant[let_var] = p;
      }
      if (auto fix = get_if<FIX>(&assign_exp)) {
        auto& [defs, in] = *fix;
        if (auto l = get_if<VAR>(&in)) {
          auto it = std::find_if(defs.begin(), defs.end(), [l] (auto const& tup) {
            return get<lvar>(tup) == *l;
          });
          assert(it != defs.end());
          auto p = module.getFunction(nameForFunction(get<lvar>(get<FN>(get<lexp>(*it)))));
          assert(p);
          unit.assignedConstant[let_var] = p;
        }
      }

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

      if (isFixpointVar(fn_exp.get(), astContext)
       && (astContext.isFinalExpression || !astContext.isListCPSFunction)) {
//        std::cout << "Replacing recursive app of " << astContext.fixPointVariable << ", final: " << std::boolalpha << astContext.isFinalExpression << "\n";
        auto recursive_function = builder.GetInsertBlock()->getParent();

        auto recursive_arguments = establish_recursive_arguments(recursive_function, arg_exp);
        if (astContext.isRecordFunction)
          unboxForUnboxedCall(builder, recursive_arguments, recursive_function);

        auto result = builder.CreateCall(recursive_function, recursive_arguments, "recursive_call");

        if (astContext.isFinalExpression) {
          yieldValue(builder, result, astContext);
          return nullptr;
        }
        return result;
      }

      if (auto fn_p = get_if<FUNCTION>(&fn_exp)) {
        auto& [fn, var] = *fn_p;

        std::vector<Value*> arguments;
        if (auto f = module.getFunction("rec_" + std::string{fn->getName()}); f && arg_exp.get().index() == RECORD) {
          fn = f;
          for (auto& e : get<RECORD>(arg_exp.get()))
            arguments.push_back(recurse(e, false));
          unboxForUnboxedCall(builder, arguments, fn);
        }
        else
          arguments.push_back(box(builder, recurse(arg_exp, false)));

        if (var == 0)
          arguments.push_back(ConstantPointerNull::get(closurePointerType));
        else
          arguments.push_back(builder.CreatePointerCast(variables.at(var), closurePointerType, "closure_ptr"));

        return builder.CreateCall(fn, arguments, "direct_call");
      }

      auto closure_ptr = builder.CreatePointerCast(recurse(fn_exp, false), closurePointerType, "closure_ptr");

      Value* arg_val = box(builder, recurse(arg_exp, false));

      auto fn = builder.CreateLoad(builder.CreatePointerCast(closure_ptr, genericFunctionType()->getPointerTo()->getPointerTo(heapAddressSpace), "fun_ptr_ptr"), "fun_ptr");

      return builder.CreateCall(fn, {arg_val, closure_ptr});
    }
    case FIX: {
      /*
        First, create all the closures, then adjust the environments' pointers
        once all allocations have been performed.
      */
      auto& [decls, exp] = get<FIX>(expression);
      vector<Value*> closure_prs;
      for (auto& [var, lty, fn] : decls) {
        if (fn.index() != FN)
          throw UnsupportedException{"FIX expressions must declare functions"};
        auto inner_vars = variables;
        for (auto& [var, _1, _2] : decls)
          inner_vars[var] = builder.CreateIntToPtr(ConstantInt::get(genericIntType, 0xDEADBEEF), genericPointerType);

        auto tyc_arr = get_if<TC_ARROW>(get_if<LT_TYC>(&lty));
        if (!tyc_arr)
          throw CompileFailException{"No arrow type in FIX expression?"};
        auto& [_, args, rets] = *tyc_arr;
        assert(rets.size() == 1 && "More than one return type in FIX function?");
        closure_prs.push_back(compile(builder, fn, inner_vars, unit,
                                      {.fixPoint = AstContext::FixPointInfo{.variable = var,
                                                                            .paramType = args,
                                                                            .retType = rets[0]},
                                       .isFunctionOfFixPoint = true,
                                       .isFinalExpression = false}));
      }
      // Adjust the pointers in all environments to refer to the allocated closures.
      std::size_t decl_index = 0;
      for (auto& [var, _, fn] : decls) {
        std::size_t clos_index = 0;
        for (auto clos : closure_prs) {
          auto freevars = freeVars(get<lexp>(decls[clos_index++]), unit);
          auto iter = freevars.find(var);
          if (iter != freevars.end()) { // If this closure captures the closure of var...
            auto pos = std::distance(freevars.begin(), iter);
            builder.CreateStore(closure_prs[decl_index],
                                builder.CreateConstGEP1_32(
                                  builder.CreatePointerCast(clos, closurePointerType),
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
      auto exit_block = BasicBlock::Create(context, "continue", func);
      BasicBlock* default_ = BasicBlock::Create(context, "default", func, exit_block);
      auto default_builder = std::make_unique<IRBuilder<>>(default_);

      std::vector<std::pair<std::unique_ptr<IRBuilder<>>, Value*>> results;

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
        auto case_bb = BasicBlock::Create(context, "case", func, exit_block);
        switch_inst->addCase(getTag(unit, constructor), case_bb);

        auto case_builder = std::make_unique<IRBuilder<>>(case_bb);
        auto vars = variables;
        if (auto datacon = get_if<DATAcon>(&constructor)) {
          auto [sym, var] = *datacon;
          switch (unit.symbolRepresentation.at(sym).type) {
            case symbol_rep::TAGGED:
              vars[var] = case_builder->CreatePointerCast(switched_v, genericPtrToPtr);
              break;
            case symbol_rep::UNTAGGED:
              vars[var] = switched_v;
              break;
            default:;
          }
        }

        // Result is zero when there is an early return within the produced code.
        if (Value* case_result = compile(*case_builder, exp, vars, unit, astContext))
          results.emplace_back(std::move(case_builder), case_result);
      }

      builder.SetInsertPoint(exit_block);

      PHINode* result_node;

      if (std::any_of(begin(results), end(results), [] (auto& pair) {
        return pair.second->getType()->isIntegerTy();
      })) {
        result_node = builder.CreatePHI(genericIntType, results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(unboxInt(*result_builder, result), result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }
      else if (std::any_of(begin(results), end(results), [] (auto& pair) {
        return pair.second->getType()->isDoubleTy();
      })) {
        result_node = builder.CreatePHI(Type::getDoubleTy(context), results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(unboxReal(*result_builder, result), result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }
      else {
        result_node = builder.CreatePHI(genericPointerType, results.size(), "switch_result");
        for (auto& [result_builder, result] : results) {
          result_node->addIncoming(result, result_builder->GetInsertBlock());
          result_builder->CreateBr(exit_block);
        }
      }

      return result_node;
    }

    case CON: {
      auto& [constr, tycs, argument] = get<CON>(expression);
      auto& symbol = get<Symbol::symbol>(constr);
      auto symrep = unit.symbolRepresentation.at(symbol);

      //! Optimisation for recursive list construction.
#ifdef ENABLE_CONS_CPS_OPT
      if (astContext.isFinalExpression
       && get<string>(symbol) == "::"      // For list conses.
       && (isPrim(tycs.front()) || isPrimAggr(tycs.front()))) // Need the new head to be primitive, as otherwise we violate GC invariant.
        if (auto rec = std::get_if<RECORD>(&argument))
          if (auto recursive_app = get_if<APP>(&rec->at(1));
              recursive_app && isFixpointVar(get<0>(*recursive_app), astContext))
          {
            outs() << "Making " << get<lvar>(get<FN>(*astContext.enclosingFunctionExpr)) << " CPS\n";

            Function *recursion_function,
                     *const parentFunction = builder.GetInsertBlock()->getParent();

            if (!astContext.isListCPSFunction) {
              if (auto p = module.getFunction(nameForFunction(get<lvar>(get<FN>(*astContext.enclosingFunctionExpr)), true)))
                recursion_function = p;
              else {
                auto newAstCtx = astContext;
                newAstCtx.isListCPSFunction = true;
                newAstCtx.isFunctionOfFixPoint = true;
                recursion_function = cast<Function>(compileFunction(&builder, *newAstCtx.enclosingFunctionExpr, variables, unit, newAstCtx));
              }
            }
            else
              recursion_function = parentFunction;

            auto&  [fn_exp, arg_exp] = *recursive_app;

            auto cons_val = recurse(rec->at(0), false);
            auto arguments = establish_recursive_arguments(parentFunction, arg_exp); // the argument to the call

            //! Here, we relocate the record on the old heap
            if (auto len = isPrimAggr(tycs.front())) {
              len = 8 * (len+1);
              Value* new_storage = builder.CreateCall(module.getFunction(largeAllocFun),
                                                      ConstantInt::get(Type::getInt64Ty(context), len), "reloc_storage");
              new_storage = builder.CreatePointerCast(new_storage, genericPointerType, "reloc_target_storage_casted");
              cons_val = builder.CreatePointerCast(cons_val, genericPointerType, "reloc_source_storage_casted");
              builder.CreateMemCpy(new_storage, cons_val, len, 8);
              cons_val = new_storage;
            }

            auto result_record = record(builder, {cons_val, ConstantPointerNull::get(genericPointerType)}, GC::Heap::Old);

            // store the currently half-constructed cons node in the parameter
            if (astContext.isListCPSFunction)
              builder.CreateStore(result_record, builder.CreateConstGEP1_32(parentFunction->arg_end()-1, 2, "list_cps_slot"));

            // The last argument is the freshly created record:
            if (!astContext.isListCPSFunction)
              arguments.emplace_back();
            arguments.back() = builder.CreatePointerCast(result_record, genericPtrToPtr, "cps_result_record");
            // Perform the recursive call with the result in the CPS parameter.
            builder.CreateCall(recursion_function, arguments);
            if (!astContext.isListCPSFunction)
              builder.CreateRet(result_record);
            else
              builder.CreateRetVoid();
            return nullptr;
          }
#endif

      switch (symrep.type) {
        case symbol_rep::CONSTANT:
          return boxInt(builder, symrep.value);
        case symbol_rep::TAGGED: {
          astContext.isCtorArgument = true;
          auto rcd = recurse(argument, false);
          builder.CreateStore(symrep.value, getTagPtrFromRecordPtr(builder, rcd));
          return builder.CreatePointerCast(rcd, genericPointerType);
        }
        case symbol_rep::UNTAGGED:
          if (symrep.value->isNullValue())
            return ConstantPointerNull::get(genericPointerType);
          return recurse(argument);
      }
    }

    case RECORD: {
      astContext.isFinalExpression = false; // redundant?
      auto& elem_exps = get<RECORD>(expression);
      if (elem_exps.empty() && !astContext.isCtorArgument) // If a Ctor argument, we must yield something that a tag can be written into
        return ConstantPointerNull::get(genericPointerType);
      astContext.isCtorArgument = false;
      std::vector<Value*> values;
      for (auto& e : elem_exps)
        values.push_back(recurse(e, false));
      return record(builder, values);
    }
    case SRECORD: {
      std::vector<Value*> values;
      astContext.isFinalExpression = false;
      for (auto& e : get<SRECORD>(expression))
        values.push_back(recurse(e, false));
      return record(builder, values);
    }

    case SELECT: {
      auto& [indices, record] = get<SELECT>(expression);
      if (indices.size() != 1)
        throw UnsupportedException{"Nested indexing"};
      auto rec_v = recurse(record, false);
      if (isa<GlobalVariable>(rec_v)) // import global variable
        rec_v = builder.CreateLoad(rec_v, "imports");
      else
        rec_v = builder.CreatePointerCast(rec_v, genericPtrToPtr, "record_ptr");
      return builder.CreateLoad(builder.CreateConstGEP1_32(rec_v, indices[0] + 1, "selected_ptr"), "selected");
    }

    case TFN:
      return recurse(get<dlexp>(get<TFN>(expression)));
    case TAPP:
      return recurse(get<dlexp>(get<TAPP>(expression)));

    // Implement exceptions as terminations for now.
    case RAISE: {
      lexp& l = get<dlexp>(get<RAISE>(expression));

      // Print the ĺocation of the exception being raised.
      if (auto x = get_if<APP>(&l))
      if (auto y = get_if<RECORD>(&get<1>(*x)))
      if (auto z = get_if<STRING>(&y->at(1)))
        builder.CreateCall(module.getOrInsertFunction("puts", FunctionType::get(Type::getInt32Ty(context), Type::getInt8PtrTy(context), false)),
                           {builder.CreateGlobalStringPtr( *z)});

      insertAbort(builder);
      return nullptr;
    }
    case HANDLE:
      return recurse(get<0>(get<HANDLE>(expression)));

    default:
      throw UnsupportedException{"compile: unsupported plambda type: " + std::to_string(expression.index()) + " (enclosing function is " + std::to_string(get<lvar>(get<FN>(*astContext.enclosingFunctionExpr))) + ")"};
  }
}

// The top-most compile function. It is passed the output of printing a lexp term in SML/NJ.
void compile_top(SMLTranslationUnit& unit) {
  compileFunction(nullptr, unit.exportedLexp, {}, unit, {.moduleExportExpression = true});
}

}
