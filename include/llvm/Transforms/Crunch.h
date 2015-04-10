#ifndef LLVM_TRANSFORMS_CRUNCH_H
#define LLVM_TRANSFORMS_CRUNCH_H

#include "llvm/Pass.h"

namespace llvm {
  llvm::FunctionPass *createCrunchSanitizerPass();
}

#endif // LLVM_TRANSFORMS_CRUNCH_H
