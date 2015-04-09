#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

#define DEBUG_TYPE "crunch"

namespace {

  class FunctionHandler {
  private:
    llvm::Function &Func;
    llvm::LLVMContext &VMContext;
    std::map<Value *, std::string> TypeAssigns;

    void dumpTypeMap() {
      errs() << "Types:\n";
      for (auto it = TypeAssigns.begin(); it != TypeAssigns.end(); ++it) {
        errs() << "  " << it->second << ": ";
        it->first->dump();
      }
    }

    void handleCrunchSizeofCall(CallInst *I) {
      auto Arg0 = cast<ConstantDataArray>(I->getArgOperand(0));
      std::string Type = Arg0->getAsString();
      errs() << "Type: '" << Type << "'\n";
      assert(TypeAssigns.find(I) == TypeAssigns.end());
      TypeAssigns[I] = Type;

      // TODO: Replace the call instruction with its first argument.
      Value *Arg1 = I->getArgOperand(1);
      Arg1->dump();
    }

    bool isAllocationFunction(llvm::Function *F) {
      if (F->getName() == "malloc") {
        return true;
      }
      return false;
    }

    void handleAllocation(CallInst *I) {
      llvm::Value *SizeArg = I->getArgOperand(0);
      errs() << "ALloc: " << SizeArg << "\n";
      I->dump();
      SizeArg->dump();
      if (TypeAssigns.find(SizeArg) == TypeAssigns.end()) {
        VMContext.diagnose(DiagnosticInfoInlineAsm::DiagnosticInfoInlineAsm(
          *I, "Could not infer type from allocation site", DS_Warning));
        return;
      }

      Function *CalledFun = I->getCalledFunction();

      std::string Uniqtype = TypeAssigns[SizeArg];
      errs() << "Allocation! type " << Uniqtype << "\n";

      auto DebugLoc = I->getDebugLoc();
      if (!DebugLoc) {
        errs() << "Warning: Cannot find allocation site: "
               << "Debug info not available.\n";
        return;
      }
      DebugLoc->dump();
      unsigned line = DebugLoc->getLine();
      const std::string File = DebugLoc->getScope()->getFile()->getFilename();
      errs() << File << "\t" << line << "\t" << CalledFun->getName();
      errs() << "\t__uniqtype__" << Uniqtype << "\n";
    }

    bool runOnCallInst(CallInst *I) {
      Function *CalledFun = I->getCalledFunction();
      if (CalledFun == nullptr) // Indirect call
        return false;

      if (CalledFun->getName() == "__crunch_sizeof__") {
        handleCrunchSizeofCall(I);
        return true;
      } else if (isAllocationFunction(CalledFun)) {
        handleAllocation(I);
        return false;
      }

      return false;
    }

    bool runOnStoreInst(StoreInst *I) {
      Value *Src = I->getValueOperand();
      Value *Dst = I->getPointerOperand();

      if (TypeAssigns.find(Src) != TypeAssigns.end()) {
        TypeAssigns[Dst] = TypeAssigns[I] = TypeAssigns[Src];
      }

      errs() << "After store instruction:";
      I->dump();
      dumpTypeMap();

      return false;
    }

    bool runOnLoadInst(LoadInst *I) {
      Value *Src = I->getPointerOperand();
      if (TypeAssigns.find(Src) != TypeAssigns.end()) {
        TypeAssigns[I] = TypeAssigns[Src];
      }
      errs() << "After load instruction:";
      I->dump();
      dumpTypeMap();
      return false;
    }

    bool runOnInstruction(Instruction *I) {
      if (auto CallI = dyn_cast<CallInst>(I)) {
        return runOnCallInst(CallI);
      } else if (auto StoreI = dyn_cast<StoreInst>(I)) {
        return runOnStoreInst(StoreI);
      } else if (auto LoadI = dyn_cast<LoadInst>(I)) {
        return runOnLoadInst(LoadI);
      }
      return false;
    }

    bool runOnBasicBlock(BasicBlock &B) {
      bool ret = false;
      for (auto it = B.begin(); it != B.end(); ++it) {
        ret = runOnInstruction(&(*it)) | ret;
      }
      return ret;
    }

  public:
    bool run() {
      Function::BasicBlockListType &BBList = Func.getBasicBlockList();
      bool ret = false;
      for (auto it = BBList.begin();
           it != BBList.end(); ++it) {
        ret = runOnBasicBlock(*it) | ret;
      }
      return ret;
    }

    FunctionHandler(Function &F) :
      Func(F), VMContext(F.getContext()) {};
  };

  struct Crunch : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    Crunch() : FunctionPass(ID) {}

    bool runOnFunction(Function &F) override {
      FunctionHandler Handler(F);
      return Handler.run();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.setPreservesAll();
    }
  };
}

char Crunch::ID = 0;
static RegisterPass<Crunch> X("crunch", "Libcrunch stuff");
