#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Pass.h"
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

static std::map<std::string, std::ofstream *> OpenFiles;

static std::ofstream *openOutputFile(const std::string &FileName) {

  if (OpenFiles.find(FileName) == OpenFiles.end()) {
    OpenFiles[FileName] = new std::ofstream(FileName,
                                            std::ios::out | std::ios::trunc);
  }
  return OpenFiles[FileName];
}

static void closeOutputFiles() {
  for (auto it = OpenFiles.begin(); it != OpenFiles.end(); ++it) {
    delete it->second;
  }
  OpenFiles.clear();
}

class AllocSite {
private:
  llvm::Instruction   *Instr;
  std::string         Name;
  const Composite::ArithType     AllocType;
  bool                Success;

public:
  AllocSite(llvm::Instruction *_Instr, std::string _Name,
            Composite::ArithType _AllocType, bool _Success) :
    Instr(_Instr), Name(_Name), AllocType(_AllocType), Success(_Success) {};

  void emit(void) {
    if (AllocType.isVoid()) {
      errs() << "Warning: Could not infer type from allocation site.\n";
    }

    auto Loc = Instr->getDebugLoc();
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

    Out << SourceRealPath << "\t" << Line << "\t" << Name << "\t";
    if (AllocType.isComposite()) {
      Out << "structure_type dumpallocs_synthetic_";
      for (char *c = SourceRealPath; *c != '\0'; ++c) {
        if (*c == '/' || *c == '.') {
          *c = '_';
        }
      }
      Out << SourceRealPath << "_" << Line << " ";
    } else {
      Out << "__uniqtype__";
    }
    Out << AllocType << std::endl;
    Out.flush();

    free(SourceRealPath);
  }
};

typedef std::map<Value *, Composite::ArithType> TypeMap;

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
  std::vector<AllocSite> AllocSites;

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

  const Composite::ArithType getType(llvm::Value *Key) {
    Key = canonicalise(Key);
    if (TypeAssigns.find(Key) != TypeAssigns.end()) {
      return TypeAssigns[Key];
    }
    return Composite::ArithType();
  }

  void setType(llvm::Value *Key, const Composite::ArithType Val, bool Store = false) {
    Key = canonicalise(Key);
    /* Generally, things should only be assigned once, since it's SSA.
     * Sizeof-returning functions persist between passes though so may be
     * overwritten. This doesn't apply to store instructions, since the same
     * memory address could be overwritten many times. */
    if (!Store && TypeAssigns.find(Key) != TypeAssigns.end() &&
        FunctionTypes.find(Key) == FunctionTypes.end()) {
      errs() << "Error: Value \'" << Key->getName() << "\' assigned twice!\n";
      Key->dump();
      errs() << "Old type: \'" << getType(Key) << "\'\n";
      errs() << "New type: \'" << Val << "\'\n";
      dumpTypeMap();
      assert(false && "SSA Value assigned twice.");
    }
    TypeAssigns[Key] = Val;
  }

  void propagateType(llvm::Value *Src, llvm::Value *Dst, bool Store = false) {
    if (hasType(Src)) {
      setType(Dst, getType(Src), Store);
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
     * associate the CallInst as well, since we've removed it. */
    auto TypeArg = cast<ConstantDataArray>(I->getArgOperand(0));
    std::string UniqtypeStr = TypeArg->getAsString();
    Composite::ArithType ArithType(UniqtypeStr);
    setType(UniqueSize, ArithType);

    // Add it to SizeofTypes as well so it will be preserved for the next pass.
    assert(SizeofTypes.find(UniqueSize) == SizeofTypes.end());
    SizeofTypes[UniqueSize] = ArithType;
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

    bool success = false;
    Composite::ArithType Uniqtype;

    if (TypeAssigns.find(SizeArg) != TypeAssigns.end()) {
      success = true;
      Uniqtype = getType(SizeArg);
    }

    AllocSite AS(I, AllocFun->getName(), Uniqtype, success);
    AllocSites.push_back(AS);

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

    propagateType(Src, Dst, true);

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

    if (!hasType(V1) && !hasType(V2)) {
      return false;
    }

    switch (I->getOpcode()) {
      case Instruction::Add:
        setType(I, getType(V1).add(getType(V2)));
        break;
      case Instruction::Sub:
        setType(I, getType(V1).sub(getType(V2)));
        break;
      case Instruction::Mul:
        setType(I, getType(V1).mul(getType(V2)));
        break;
      case Instruction::SDiv:
      case Instruction::UDiv:
        setType(I, getType(V1).div(getType(V2)));
        break;
    }
    return false;
  }

  bool runOnAllocaInst(llvm::AllocaInst *I) {
    Value *Arg = I->getArraySize();
    if (hasType(Arg)) {
      AllocSite AS(I, "__builtin_alloca", getType(Arg), true);
      AllocSites.push_back(AS);
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
    AllocSites.clear();

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

  // Write all the allocation sites out.
  void emit() {
    for (auto it = AllocSites.begin(); it != AllocSites.end(); ++it) {
      it->emit();
    }
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
    } while (PrevFunctionTypes != Handler.FunctionTypes);

    Handler.emit();
    closeOutputFiles();

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
