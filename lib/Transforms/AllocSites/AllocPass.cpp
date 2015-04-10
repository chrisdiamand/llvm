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
#include "llvm/Transforms/AllocSites/AllocFunction.h"
#include "llvm/Transforms/AllocSites/AllocSites.h"
#include "llvm/Transforms/AllocSites/Composite.h"

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
  std::map<Value *, Crunch::AllocFunction *> AllocAssigns;
  std::vector<CallInst *> InstructionsToRemove;

  void dumpTypeMap() {
    errs() << "Types:\n";
    for (auto it = TypeAssigns.begin(); it != TypeAssigns.end(); ++it) {
      errs() << "  " << it->second << ": ";
      it->first->dump();
    }
  }

  void dumpAllocMap() {
    errs() << "Allocs:\n";
    for (auto it = AllocAssigns.begin(); it != AllocAssigns.end(); ++it) {
      errs() << "  " << it->second->getName() << ": ";
      it->first->dump();
    }
  }

  bool hasType(llvm::Value *Key) {
    return TypeAssigns.find(Key) != TypeAssigns.end();
  }

  void setType(llvm::Value *Key, std::string Val) {
    assert(!hasType(Key));
    TypeAssigns[Key] = Val;
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
    setType(UniqueSize, Type);
  }

  Crunch::AllocFunction *getAllocationFunction(llvm::Value *V) {

    if (V != nullptr && AllocAssigns.find(V) != AllocAssigns.end()) {
      return AllocAssigns[V];
    }

    if (auto F = getActualCalledFunction(V)) {
      std::string Name = F->getName();
      return Crunch::AllocFunction::get(Name);
    }

    return nullptr;
  }

  bool handleAllocation(CallInst *I) {
    auto AllocFun = getAllocationFunction(I->getCalledValue());
    if (AllocFun == NULL) {
      // Not an allocation function so did nothing; return false.
      return false;
    }

    unsigned SizeArgIndex = AllocFun->getSizeArg();
    assert(SizeArgIndex < I->getNumArgOperands());
    llvm::Value *SizeArg = I->getArgOperand(SizeArgIndex);

    if (TypeAssigns.find(SizeArg) == TypeAssigns.end()) {
      VMContext.diagnose(DiagnosticInfoInlineAsm::DiagnosticInfoInlineAsm(
        *I, "Could not infer type from allocation site", DS_Warning));
      return true;
    }
    std::string Uniqtype = TypeAssigns[SizeArg];

    auto DebugLoc = I->getDebugLoc();
    if (!DebugLoc) {
      errs() << "Warning: Cannot find allocation site: "
             << "Debug info not available.\n";
      return true;
    }

    unsigned line = DebugLoc->getLine();
    const std::string File = DebugLoc->getScope()->getFile()->getFilename();
    emitAllocSite(File, line, AllocFun->getName(), Uniqtype);

    return true;
  }

  // Sometimes the sizeof marker function is surrounded by a bitcast.
  llvm::Function *getActualCalledFunction(llvm::Value *I) {
    if (auto Fun = dyn_cast<llvm::Function>(I)) {
      return Fun;
    }

    if (auto Usr = dyn_cast<llvm::User>(I)) {
      for (auto it = Usr->op_begin(); it != Usr->op_end(); ++it) {
        if (auto Ret = getActualCalledFunction(*it)) {
          return Ret;
        }
      }
    }
    return nullptr;
  }

  bool runOnCallInst(CallInst *I) {
    Function *CalledFun = getActualCalledFunction(I->getCalledValue());

    if (handleAllocation(I)) {
      return false;

    } else if (CalledFun != nullptr &&
               CalledFun->getName() == "__crunch_sizeof__") {
      handleCrunchSizeofCall(I);
      return true;
    }

    return false;
  }

  bool runOnStoreInst(StoreInst *I) {
    Value *Src = I->getValueOperand();
    Value *Dst = I->getPointerOperand();

    if (TypeAssigns.find(Src) != TypeAssigns.end()) {
      TypeAssigns[Dst] = TypeAssigns[I] = TypeAssigns[Src];
    }

    if (auto AF = getAllocationFunction(Src)) {
      AllocAssigns[Dst] = AllocAssigns[Src] = AF;
    }

    return false;
  }

  bool runOnLoadInst(LoadInst *I) {
    Value *Src = I->getPointerOperand();

    if (TypeAssigns.find(Src) != TypeAssigns.end()) {
      TypeAssigns[I] = TypeAssigns[Src];
    }

    if (auto AF = getAllocationFunction(Src)) {
      AllocAssigns[I] = AF;
    }

    return false;
  }

  bool runOnBinaryOperator(llvm::BinaryOperator *I) {
    assert(I->getNumOperands() == 2);
    llvm::Value *V1 = I->getOperand(0);
    llvm::Value *V2 = I->getOperand(1);

    switch (I->getOpcode()) {
      case Instruction::Add:
      case Instruction::Mul:
        if (hasType(V1) && !hasType(V2)) {
          setType(I, TypeAssigns[V1]);
        } else if (!hasType(V1) && hasType(V2)) {
          setType(I, TypeAssigns[V2]);
        } else if (hasType(V1) && hasType(V2)) {
          if (I->getOpcode() == Instruction::Mul) {
            setType(I, Composite::mul(TypeAssigns[V1], TypeAssigns[V2]));
          } else {
            setType(I, Composite::add(TypeAssigns[V1], TypeAssigns[V2]));
          }
        }
        break;
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
    } else if (auto BinI = dyn_cast<BinaryOperator>(I)) {
      return runOnBinaryOperator(BinI);
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
