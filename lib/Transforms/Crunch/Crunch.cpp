#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "crunch"

namespace {
  typedef std::map<Value *, std::string> TypeMap;

  void DumpTypeMap(TypeMap &TypeAssigns) {
    for (auto it = TypeAssigns.begin(); it != TypeAssigns.end(); ++it) {
      errs() << it->first << ": " << it->second << "\n";
    }
  }

  // Crunch - The first implementation, without getAnalysisUsage.
  struct Crunch : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Crunch() : FunctionPass(ID) {}

    bool runOnCallInst(CallInst *I, TypeMap &TypeAssigns) {
      Function *CalledFun = I->getCalledFunction();
      if (CalledFun == nullptr) // Indirect call
        return false;

      if (CalledFun->getName() != "__crunch_sizeof__")
        return false;

      auto Arg0 = cast<ConstantDataArray>(I->getArgOperand(0));
      std::string Type = Arg0->getAsString();
      errs() << "Type: '" << Type << "'\n";
      assert(TypeAssigns.find(I) == TypeAssigns.end());
      TypeAssigns[I] = Type;


      // TODO: Replace the call instruction with its first argument.
      Value *Arg1 = I->getArgOperand(1);
      Arg1->dump();

      return true;
    }

    bool runOnStoreInst(StoreInst *I, TypeMap &TypeAssigns) {
      Value *Src = I->getValueOperand();
      Value *Dst = I->getPointerOperand();

      if (TypeAssigns.find(Src) != TypeAssigns.end()) {
        TypeAssigns[Dst] = TypeAssigns[Src];
      }

      DumpTypeMap(TypeAssigns);

      return false;
    }

    bool runOnLoadInst(LoadInst *I, TypeMap &TypeAssigns) {
      if (TypeAssigns.find(I) != TypeAssigns.end()) {
        TypeAssigns[I->getPointerOperand()] = TypeAssigns[I];
      }
      DumpTypeMap(TypeAssigns);
      return false;
    }

    bool runOnInstruction(Instruction *I, TypeMap &TypeAssigns) {
      if (auto CallI = dyn_cast<CallInst>(I)) {
        return runOnCallInst(CallI, TypeAssigns);
      } else if (auto StoreI = dyn_cast<StoreInst>(I)) {
        return runOnStoreInst(StoreI, TypeAssigns);
      } else if (auto LoadI = dyn_cast<LoadInst>(I)) {
        return runOnLoadInst(LoadI, TypeAssigns);
      }
      return false;
    }

    bool runOnBasicBlock(BasicBlock &B, TypeMap &TypeAssigns) {
      bool ret = false;
      for (auto it = B.begin(); it != B.end(); ++it) {
        ret = runOnInstruction(&(*it), TypeAssigns) | ret;
      }
      return ret;
    }

    bool runOnFunction(Function &F) override {
      TypeMap TypeAssigns;
      Function::BasicBlockListType &BBList = F.getBasicBlockList();
      bool ret = false;
      for (auto it = BBList.begin();
           it != BBList.end(); ++it) {
        ret = runOnBasicBlock(*it, TypeAssigns) | ret;
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
