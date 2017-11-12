#pragma once

#include "AbsynInterfaceBasic.hpp"

namespace SMLNJAbsynInterface {

namespace PrimCTypes {

  struct c_int : LABELLED_VARIANT(
    (I_char)
    (I_short)
    (I_int)
    (I_long)
    (I_long_long)
  );

  struct c_type : LABELLED_VARIANT(
    (C_void)
    (C_float)
    (C_double)
    (C_long_double)
    (C_unsigned, c_int)
    (C_signed, c_int)
    (C_PTR)
    (C_ARRAY, pair<dynamic_wrapper<c_type>, int>)
    (C_STRUCT, vector<dynamic_wrapper<c_type>>)
    (C_UNION, vector<dynamic_wrapper<c_type>>)
  );

  using calling_convention = string;

  struct c_proto {
    calling_convention conv;
    c_type retTy;
    vector<c_type> paramTys;
  };
}

namespace Primop {

struct numkind : LABELLED_VARIANT(
  (INT, int)
  (UINT, int)
  (FLOAT, int)
);

enum arithop {
  ADD, SUB, MUL, NEG,                    // int or float
  FDIV, ABS, FSQRT, FSIN, FCOS, FTAN,    // floating point only
  LSHIFT , RSHIFT , RSHIFTL,             // int only
  ANDB , ORB , XORB , NOTB,              // int only
  DIV , MOD , QUOT , REM                 // int only
};

enum cmpop {
  GT, GTE, LT, LTE,  // signed comparisons
  LEU, LTU, GEU, GTU, // unsigned comparisons
  EQL, NEQ,  // equality
  FSGN
};

enum ccall_type {
  CCI32, /* passed as int32 */
  CCI64, /* int64, currently unused */
  CCR64, /* passed as real64 */
  CCML   /* passed as Unsafe.Object.object */
};

  /* datatype primop:
   * Various primitive operations. Those that are designated "inline" (L:) in
   * the comments are expanded into lambda code in terms of other operators,
   * as are the "checked=true" versions of NUMSUBSCRIPT and NUMUPDATE (L?:).
   * "Environmental" primops (occurring in the InLine structure) are indicated
   * by "E:" in the comment. */
struct primop : LABELLED_VARIANT(
  (ARITH, struct {
    arithop oper;
    bool overflow;
    numkind kind;
  })
  (INLLSHIFT,  numkind)
  (INLRSHIFT,  numkind)
  (INLRSHIFTL, numkind)
  (CMP, struct {
    cmpop oper;
    numkind kind;
  })
  (TESTU     , pair<int, int>)
  (TEST      , pair<int, int>)
  (TRUNC     , pair<int, int>)
  (EXTEND    , pair<int, int>)
  (COPY      , pair<int, int>)
  (TEST_INF  , int)
  (TRUNC_INF , int)
  (EXTEND_INF, int)
  (COPY_INF  , int)
  (ROUND, struct {
    bool floor;
    numkind fromkind,
            tokind;
  })
  (REAL, struct {
    numkind fromkind,
            tokind;
  })
  (NUMSUBSCRIPT, struct {
    numkind kind;
    bool checked;
    bool immutable;
  })
  (NUMUPDATE, struct {
    numkind kind;
    bool checked;
  })
  (SUBSCRIPT)                      /* E: polymorphic array subscript */
  (SUBSCRIPTV)            /* E: poly vector subscript */
  (INLSUBSCRIPT)          /* E: L: poly array subscript */
  (INLSUBSCRIPTV)            /* E: L: poly vector subscript */
  (INLMKARRAY)            /* E: L: poly array creation */
  (PTREQL) (PTRNEQ)          /* E: pointer equality */
  (POLYEQL) (POLYNEQ)        /* E: polymorphic equality */
  (BOXED) (UNBOXED)          /* E: boxity tests */
  (LENGTH)             /* E: vector, string, array, ... length */
  (OBJLENGTH)          /* E: length of arbitrary heap object */
  (CAST)               /* E: cast */
  (GETHDLR) (SETHDLR)        /* E: get/set exn handler pointer */
  (GETVAR) (SETVAR)          /* E: get/set var register */
  (GETPSEUDO) (SETPSEUDO)       /* E: get/set pseudo registers */
  (SETMARK) (DISPOSE)        /* E: capture/dispose frames */
  (MAKEREF)               /* E: allocate a ref cell */
  (CALLCC) (CAPTURE) (THROW)    /* E: continuation operations */
  (ISOLATE)               /* E: isolating a function */
  (DEREF)              /* E: dereferencing */
  (ASSIGN)             /* E: assignment */
  (UPDATE)             /* E: array or reference update (maybe boxed) */
  (INLUPDATE)          /* E: L: array update (maybe boxed) */
  (UNBOXEDUPDATE)            /* E: update array of integers WITH tags
                   * removed by Zhong, put back by Matthias
                   * (see FLINT/trans/primopmap.sml) */
  (GETTAG)             /* E: extract the tag portion of an
                   * object's descriptor as an ML int */
  (MKSPECIAL)          /* E: make a special object */
  (SETSPECIAL)            /* E: set the state of a special object */
  (GETSPECIAL)            /* E: get the state of a special object */
  (INLMIN, numkind)        /* E: L: min */
  (INLMAX, numkind)        /* E: L: max */
  (INLABS, numkind)        /* E: L: abs */
  (INLNOT)             /* E: L: bool not operator */
  (INLCOMPOSE)            /* E: L: compose "op o"  operator */
  (INLBEFORE)          /* E: L: "before" operator */
  (INLIGNORE)          /* E: L: "ignore" function */
    /* primops to support new array representations */
  (NEW_ARRAY0)            /* E: allocate zero-length array header */
  (GET_SEQ_DATA)          /* E: get data pointer from arr/vec header */
  (SUBSCRIPT_REC)            /* E: record subscript operation */
  (SUBSCRIPT_RAW64)          /* E: raw64 subscript operation */
  (INLIDENTITY)           /* E: polymorphic identity */
  (CVT64)              /* E: convert between external and
                   * internal representation of compiler
                   * simulated 64-bit scalars, e.g. w64p */
    /* Primops to support C FFI. */
  (RAW_LOAD, numkind)         /* E: load from arbitrary memory location */
  (RAW_STORE, numkind)        /* E: store to arbitrary memory location */
    /* E: make a call to a C-function;
     * The primop carries C function prototype information and specifies
     * which of its (ML-) arguments are floating point. C prototype
     * information is for use by the backend, ML information is for
     * use by the CPS converter. */
  (RAW_CCALL, struct RAW_CCALL {
    PrimCTypes::c_proto c_proto;
    vector<ccall_type> ml_args;
    optional<ccall_type> ml_res_opt;
    bool reentrant;
  } _; typedef optional<RAW_CCALL>)
  (RAW_RECORD, struct {
    bool fblock;
  })
  (UNBOXEDASSIGN) // assignment to integer reference
  (WCAST) // ?
  (MARKEXN) // mark an exception value with a string
  (INL_ARRAY) // L: polymorphic array allocation
  (INL_VECTOR) // L: polymorphic vector allocation
  (INL_MONOARRAY, numkind) // L: monomorphic array allocation
  (INL_MONOVECTOR, numkind) // L: monomorphic vector allocation
  (MKETAG) // make a new exception tag
  (WRAP) // box a value by wrapping it
  (UNWRAP) // unbox a value by unwrapping it
);

} // end namespace Primop

} // end namespace SMLNJAbsynInterface
