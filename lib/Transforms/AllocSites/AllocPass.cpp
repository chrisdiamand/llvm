#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/AllocSites.h"

#include <limits.h>
#include <stdlib.h>

#include <fstream>
#include <iostream>
#include <map>

using namespace llvm;

#define DEBUG_TYPE "crunch"

namespace {

static std::string getOutputFName(const std::string &SrcPath) {
  std::string ExtRemoved = SrcPath.substr(0, SrcPath.length() - 2);
  return ExtRemoved + ".i.allocs";
}

static std::ofstream *openOutputFile(const std::string &FileName) {
  static std::map<std::string, std::ofstream *> OpenFiles;

  // FIXME: Where do these get closed?
  if (OpenFiles.find(FileName) == OpenFiles.end()) {
    OpenFiles[FileName] = new std::ofstream(FileName,
                                            std::ios::out | std::ios::trunc);
  }
  return OpenFiles[FileName];
}

static void emitAllocSite(const std::string &SourcePath, unsigned Line,
                          const std::string &FunName, const std::string &Type)
{
  std::string SitesFileName = getOutputFName(SourcePath);
  std::ofstream &Out = *openOutputFile(SitesFileName);

  char *SourceRealPath = realpath(SourcePath.c_str(), NULL);

  Out << SourceRealPath << "\t" << Line << "\t" << FunName;
  Out << "\t__uniqtype__" << Type << std::endl;

  free(SourceRealPath);
}

class FunctionHandler {
private:
  llvm::Function &Func;
  llvm::LLVMContext &VMContext;
  std::map<Value *, std::string> TypeAssigns;
  std::vector<CallInst *> InstructionsToRemove;

  void dumpTypeMap() {
    errs() << "Types:\n";
    for (auto it = TypeAssigns.begin(); it != TypeAssigns.end(); ++it) {
      errs() << "  " << it->second << ": ";
      it->first->dump();
    }
  }

  void handleCrunchSizeofCall(CallInst *I) {
    /* Replace the call instruction with its second argument. We can't
     * actually remove it here though since the iterator gets confused. */
    Value *SizeArg = I->getArgOperand(1);
    auto UniqueSize = llvm::BinaryOperator::Create(
                        Instruction::Add,
                        SizeArg,
                        llvm::ConstantInt::get(SizeArg->getType(), 0),
                        "sizeof", I);

    I->replaceAllUsesWith(UniqueSize);
    InstructionsToRemove.push_back(I);

    /* Now associate the type with the size argument. We don't need to
     * associate the CallInstr as well, since we've removed it. */
    auto TypeArg = cast<ConstantDataArray>(I->getArgOperand(0));
    std::string Type = TypeArg->getAsString();
    assert(TypeAssigns.find(SizeArg) == TypeAssigns.end());
    TypeAssigns[UniqueSize] = Type;
  }

  bool isAllocationFunction(llvm::Function *F) {
    if (F->getName() == "malloc") {
      return true;
    }
    return false;
  }

  void handleAllocation(CallInst *I) {
    llvm::Value *SizeArg = I->getArgOperand(0);

    if (TypeAssigns.find(SizeArg) == TypeAssigns.end()) {
      VMContext.diagnose(DiagnosticInfoInlineAsm::DiagnosticInfoInlineAsm(
        *I, "Could not infer type from allocation site", DS_Warning));
      return;
    }

    Function *CalledFun = I->getCalledFunction();

    std::string Uniqtype = TypeAssigns[SizeArg];

    auto DebugLoc = I->getDebugLoc();
    if (!DebugLoc) {
      errs() << "Warning: Cannot find allocation site: "
             << "Debug info not available.\n";
      return;
    }

    unsigned line = DebugLoc->getLine();
    const std::string File = DebugLoc->getScope()->getFile()->getFilename();
    emitAllocSite(File, line, CalledFun->getName(), Uniqtype);
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

    return false;
  }

  bool runOnLoadInst(LoadInst *I) {
    Value *Src = I->getPointerOperand();

    if (TypeAssigns.find(Src) != TypeAssigns.end()) {
      TypeAssigns[I] = TypeAssigns[Src];
    }

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

    for (auto it = InstructionsToRemove.begin();
         it != InstructionsToRemove.end(); ++it) {
      (*it)->eraseFromParent();
    }

    return ret;
  }

  FunctionHandler(Function &F) :
    Func(F), VMContext(F.getContext()) {};
};

struct AllocSitesPass : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  AllocSitesPass() : FunctionPass(ID) {}

  bool runOnFunction(Function &F) override {
    FunctionHandler Handler(F);
    return Handler.run();
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }
};

} // namespace

llvm::FunctionPass *llvm::createAllocSitesSanitizerPass() {
  return new AllocSitesPass();
}

char AllocSitesPass::ID = 0;
static RegisterPass<AllocSitesPass> X("allocs", "Dump allocation sites");
