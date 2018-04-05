//==============================================================================
//LLVM Release License
//==============================================================================
//University of Illinois/NCSA
//Open Source License
//
//Copyright (c) 2003-2017 University of Illinois at Urbana-Champaign.
//All rights reserved.
//
//Developed by:
//
//    LLVM Team
//
//    University of Illinois at Urbana-Champaign
//
//    http://llvm.org
//
//Permission is hereby granted, free of charge, to any person obtaining a copy of
//this software and associated documentation files (the "Software"), to deal with
//the Software without restriction, including without limitation the rights to
//use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
//of the Software, and to permit persons to whom the Software is furnished to do
//so, subject to the following conditions:
//
//    * Redistributions of source code must retain the above copyright notice,
//      this list of conditions and the following disclaimers.
//
//    * Redistributions in binary form must reproduce the above copyright notice,
//      this list of conditions and the following disclaimers in the
//      documentation and/or other materials provided with the distribution.
//
//    * Neither the names of the LLVM Team, University of Illinois at
//      Urbana-Champaign, nor the names of its contributors may be used to
//      endorse or promote products derived from this Software without specific
//      prior written permission.
//
//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
//FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
//CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE
//SOFTWARE.

//===- RewriteStatepointsForGC.h - ------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides interface to "Rewrite Statepoints for GC" pass.
//
// This passe rewrites call/invoke instructions so as to make potential
// relocations performed by the garbage collector explicit in the IR.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_SCALAR_REWRITE_STATEPOINTS_FOR_GC_H
#define LLVM_TRANSFORMS_SCALAR_REWRITE_STATEPOINTS_FOR_GC_H

#include "llvm/IR/PassManager.h"

namespace llvm {
  class DominatorTree;
  class Function;
  class Module;
  class TargetTransformInfo;
  class TargetLibraryInfo;
}

namespace SMLCompiler {

  struct RewriteStatepointsForGC_NoDerivedPtrs : public llvm::PassInfoMixin<RewriteStatepointsForGC_NoDerivedPtrs> {
    llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &AM);

    bool runOnFunction(llvm::Function &F, llvm::DominatorTree &, llvm::TargetTransformInfo &,
                       const llvm::TargetLibraryInfo &);
  };

  llvm::ModulePass *createRewriteStatepointsForGCNoDerivedPtrsLegacyPass();

} // namespace llvm

#endif // LLVM_TRANSFORMS_SCALAR_REWRITE_STATEPOINTS_FOR_GC_H
