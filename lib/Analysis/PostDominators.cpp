//===- PostDominators.cpp - Post-Dominator Calculation --------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the post-dominator construction algorithms.
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/PostDominators.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SetOperations.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GenericDomTreeConstruction.h"
using namespace llvm;

#define DEBUG_TYPE "postdomtree"

//===----------------------------------------------------------------------===//
//  PostDominatorTree Implementation
//===----------------------------------------------------------------------===//

char PostDominatorTreeWrapperPass::ID = 0;
INITIALIZE_PASS(PostDominatorTreeWrapperPass, "postdomtree",
                "Post-Dominator Tree Construction", true, true)

bool PostDominatorTreeWrapperPass::runOnFunction(Function &F) {
  DT.recalculate(F);
  return false;
}

void PostDominatorTreeWrapperPass::print(raw_ostream &OS, const Module *) const {
  DT.print(OS);
}

FunctionPass* llvm::createPostDomTree() {
  return new PostDominatorTreeWrapperPass();
}

template class llvm::AnalysisBase<PostDominatorTreeAnalysis>;

PostDominatorTree PostDominatorTreeAnalysis::run(Function &F) {
  PostDominatorTree PDT;
  PDT.recalculate(F);
  return PDT;
}

PostDominatorTreePrinterPass::PostDominatorTreePrinterPass(raw_ostream &OS)
  : OS(OS) {}

PreservedAnalyses
PostDominatorTreePrinterPass::run(Function &F, FunctionAnalysisManager *AM) {
  OS << "PostDominatorTree for function: " << F.getName() << "\n";
  AM->getResult<PostDominatorTreeAnalysis>(F).print(OS);

  return PreservedAnalyses::all();
}
