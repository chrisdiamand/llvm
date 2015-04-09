#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "crunch"

namespace {
  // Crunch - The first implementation, without getAnalysisUsage.
  struct Crunch : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Crunch() : FunctionPass(ID) {}

    bool runOnInstruction(Instruction *I) {
      auto CallI = dyn_cast<CallInst>(I);
      if (CallI == nullptr)
        return false;

      Function *CalledFun = CallI->getCalledFunction();
      if (CalledFun == nullptr) // Indirect call
        return false;

      if (CalledFun->getName() != "__crunch_sizeof__")
        return false;

      CalledFun->dump();
      return true;
    }

    bool runOnBasicBlock(BasicBlock &B) {
      bool ret = false;
      for (auto it = B.begin(); it != B.end(); ++it) {
        ret = runOnInstruction(&(*it)) | ret;
      }
      return ret;
    }

    bool runOnFunction(Function &F) override {
      Function::BasicBlockListType &BBList = F.getBasicBlockList();
      bool ret = false;
      for (auto it = BBList.begin();
           it != BBList.end(); ++it) {
        ret = runOnBasicBlock(*it) | ret;
      }
      return ret;
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
    }
  };
}

char Crunch::ID = 0;
static RegisterPass<Crunch> X("crunch", "Libcrunch stuff");
