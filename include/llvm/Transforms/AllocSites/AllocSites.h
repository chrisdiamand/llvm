#ifndef LLVM_TRANSFORMS_CRUNCH_H
#define LLVM_TRANSFORMS_CRUNCH_H

#include "llvm/Pass.h"

namespace llvm {
  llvm::ModulePass *createAllocSitesSanitizerPass();
}

#endif // LLVM_TRANSFORMS_CRUNCH_H
