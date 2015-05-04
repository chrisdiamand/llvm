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

static void emitAllocSite(llvm::Instruction *I, std::string Name,
                          std::string Type)
{
  auto Loc = I->getDebugLoc();
  if (!Loc) {
    errs() << "Warning: Cannot find allocation site: "
           << "Debug info not available.\n";
    return;
  }

  unsigned Line = Loc->getLine();
  const std::string SourcePath = Loc->getScope()->getFile()->getFilename();

  std::string SitesFileName = getOutputFName(SourcePath);
  std::ofstream &Out = *openOutputFile(SitesFileName);

  char *SourceRealPath = realpath(SourcePath.c_str(), NULL);

  Out << SourceRealPath << "\t" << Line << "\t" << Name;
  Out << "\t__uniqtype__" << Type << std::endl;

  free(SourceRealPath);
}

typedef std::map<Value *, Composite::Type> TypeMap;

class ModuleHandler {
public:
  /* This is a subset of TypeAssigns. Use it to monitor if any sizeof-returning
   * functions have changed - if they have, do another pass. */
  TypeMap FunctionTypes;

  void dumpTypeMap(TypeMap &TM) {
    for (auto it = TM.begin(); it != TM.end(); ++it) {
      errs() << "  " << it->second << ": " << it->first->getName() << "\n";
    }
  }

  /* We also need to preserve the __crunch_sizeof__ call information between
   * passes, since the calls are removed in the first pass. */
  TypeMap SizeofTypes;

private:
  llvm::Module &TheModule;
  Function *CurrentFunction;
  llvm::LLVMContext &VMContext;
  TypeMap TypeAssigns;
  std::map<Value *, Crunch::AllocFunction *> AllocAssigns;
  std::vector<CallInst *> InstructionsToRemove;
  int pass; // How many passes have been run.

  void dumpTypeMap() {
    errs() << "Types:\n";
    dumpTypeMap(TypeAssigns);
  }

  void dumpAllocMap() {
    errs() << "Allocs:\n";
    for (auto it = AllocAssigns.begin(); it != AllocAssigns.end(); ++it) {
      errs() << "  " << it->second->getName() << ": ";
      it->first->dump();
    }
  }

  llvm::Value *canonicalise(llvm::Value *V) {
    assert(V != nullptr);
    return V->stripPointerCasts();
  }

  bool hasType(llvm::Value *Key) {
    return TypeAssigns.find(canonicalise(Key)) != TypeAssigns.end();
  }

  std::string getType(llvm::Value *Key) {
    Key = canonicalise(Key);
    assert(TypeAssigns.find(Key) != TypeAssigns.end());
    return TypeAssigns[Key];
  }

  void setType(llvm::Value *Key, std::string Val) {
    Key = canonicalise(Key);
    /* Generally, things should only be assigned once, since it's SSA.
     * Sizeof-returning functions persist between passes though so may be
     * overwritten. */
    assert(TypeAssigns.find(Key) == TypeAssigns.end() ||
           FunctionTypes.find(Key) != FunctionTypes.end());
    TypeAssigns[Key] = Val;
  }

  void propagateType(llvm::Value *Src, llvm::Value *Dst) {
    if (hasType(Src)) {
      setType(Dst, getType(Src));
    }
  }

  void handleCrunchSizeofCall(CallInst *I) {
    // This should only happen on the first pass - the calls should have been
    // removed after that.
    assert(pass == 1);

    /* Replace the call instruction with its second argument. We can't
     * actually remove it here though since the iterator gets confused. */
    Value *SizeArg = I->getArgOperand(1);
    auto UniqueSize = llvm::BinaryOperator::Create(
                        Instruction::Add,
                        SizeArg,
                        llvm::ConstantInt::get(SizeArg->getType(), 0),
                        "sizeof");
    UniqueSize->insertAfter(I);

    I->replaceAllUsesWith(UniqueSize);
    InstructionsToRemove.push_back(I);

    /* Now associate the type with the size argument. We don't need to
     * associate the CallInstr as well, since we've removed it. */
    auto TypeArg = cast<ConstantDataArray>(I->getArgOperand(0));
    std::string Type = TypeArg->getAsString();
    setType(UniqueSize, Type);

    // Add it to SizeofTypes as well so it will be preserved for the next pass.
    assert(SizeofTypes.find(UniqueSize) == SizeofTypes.end());
    SizeofTypes[UniqueSize] = Type;
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

    std::string UniqtypeName = "";

    if (TypeAssigns.find(SizeArg) != TypeAssigns.end()) {
      UniqtypeName = getType(SizeArg);
    } else {
      VMContext.diagnose(DiagnosticInfoInlineAsm::DiagnosticInfoInlineAsm(
        *I, "Could not infer type from allocation site", DS_Warning));
      /* Emit a `void' type if we can't find it, since casts to this won't
       * cause errors. */
      UniqtypeName = "void";
    }
    emitAllocSite(I, AllocFun->getName(), UniqtypeName);
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
    auto CalledVal = I->getCalledValue();
    Function *CalledFun = getActualCalledFunction(CalledVal);

    if (handleAllocation(I)) {
      return false;

    } else if (CalledFun != nullptr &&
               CalledFun->getName() == "__crunch_sizeof__") {
      handleCrunchSizeofCall(I);
      return true;
    } else if (hasType(CalledVal)) {
      // Some function return sizeof expressions.
      propagateType(CalledVal, I);
    }

    return false;
  }

  bool runOnStoreInst(StoreInst *I) {
    Value *Src = I->getValueOperand();
    Value *Dst = I->getPointerOperand();

    propagateType(Src, Dst);

    if (auto AF = getAllocationFunction(Src)) {
      AllocAssigns[Dst] = AllocAssigns[Src] = AF;
    }

    return false;
  }

  bool runOnLoadInst(LoadInst *I) {
    Value *Src = I->getPointerOperand();
    propagateType(Src, I);

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
          propagateType(V1, I);
        } else if (!hasType(V1) && hasType(V2)) {
          propagateType(V2, I);
        } else if (hasType(V1) && hasType(V2)) {
          if (I->getOpcode() == Instruction::Mul) {
            setType(I, Composite::mul(getType(V1), getType(V2)));
          } else {
            setType(I, Composite::add(getType(V1), getType(V2)));
          }
        }
        break;
    }
    return false;
  }

  bool runOnAllocaInst(llvm::AllocaInst *I) {
    Value *Arg = I->getArraySize();
    if (hasType(Arg)) {
      emitAllocSite(I, "__builtin_alloca", getType(Arg));
    }
    return false;
  }

  bool runOnReturnInst(llvm::ReturnInst *I) {
    Value *RetVal = I->getReturnValue();
    if (RetVal != nullptr) {
      propagateType(RetVal, CurrentFunction);

      if (hasType(RetVal)) {
        FunctionTypes[canonicalise(CurrentFunction)] = getType(RetVal);
      }
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
    } else if (auto AllocaI = dyn_cast<AllocaInst>(I)) {
      return runOnAllocaInst(AllocaI);
    } else if (auto RetI = dyn_cast<ReturnInst>(I)) {
      return runOnReturnInst(RetI);
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

  bool runOnFunction(Function &Func) {
    CurrentFunction = &Func;
    auto &BBList = Func.getBasicBlockList();
    bool ret = false;
    for (auto it = BBList.begin();
         it != BBList.end(); ++it) {
      ret = runOnBasicBlock(*it) | ret;
    }

    return ret;
  }

public:
  bool run() {
    pass++;
    bool ret = false;

    /* Initialise the type assignments to known sizeof-returning functions, and
     * the preserved info from __crunch_sizeof__ marker calls. */
    TypeAssigns = FunctionTypes;
    TypeAssigns.insert(SizeofTypes.begin(), SizeofTypes.end());

    auto &FunList = TheModule.getFunctionList();
    for (auto it = FunList.begin(); it != FunList.end(); ++it) {
      ret = runOnFunction(*it) | ret;
    }

    for (auto it = InstructionsToRemove.begin();
         it != InstructionsToRemove.end(); ++it) {
      (*it)->eraseFromParent();
    }
    InstructionsToRemove.clear();

    return ret;
  }

  ModuleHandler(Module &M) :
    TheModule(M), VMContext(M.getContext()), pass(0) {};
};

struct AllocSitesPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  AllocSitesPass() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    bool ret = false;
    ModuleHandler Handler(M);
    TypeMap PrevFunctionTypes;
    do {
      PrevFunctionTypes = Handler.FunctionTypes;
      ret = Handler.run() | ret;
      errs() << "Old:\n";
      Handler.dumpTypeMap(PrevFunctionTypes);
      errs() << "New:\n";
      Handler.dumpTypeMap(Handler.FunctionTypes);
    } while (PrevFunctionTypes != Handler.FunctionTypes);

    return ret;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesAll();
  }
};

} // namespace

llvm::ModulePass *llvm::createAllocSitesSanitizerPass() {
  return new AllocSitesPass();
}

char AllocSitesPass::ID = 0;
static RegisterPass<AllocSitesPass> X("allocs", "Dump allocation sites");
