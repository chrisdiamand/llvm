#ifndef LLVM_TRANSFORMS_CRUNCH_H
#define LLVM_TRANSFORMS_CRUNCH_H

#include "llvm/Pass.h"

namespace llvm {
  llvm::FunctionPass *createAllocSitesSanitizerPass();
}

#endif // LLVM_TRANSFORMS_CRUNCH_H
