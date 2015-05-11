#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Operator.h"
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

private:
  llvm::Module            &TheModule;
  Function                *CurrentFunction;
  llvm::LLVMContext       &VMContext;
  TypeMap                 TypeAssigns;
  llvm::Function          *SizeofMarker;
  int                     pass; // How many passes have been run.
  std::vector<AllocSite>  AllocSites;

  void dumpTypeMap() {
    errs() << "Types:\n";
    dumpTypeMap(TypeAssigns);
  }

  /* If V is an llvm::Function referring to an allocation function, return its
   * AllocFunction information. */
  Crunch::AllocFunction *getAllocationFunction(llvm::Value *V) {
    V = V->stripPointerCasts();
    std::string Name = V->getName();
    return Crunch::AllocFunction::get(Name);
  }

  bool handleCallInst(CallInst *I, Composite::ArithType &Uniqtype) {
    auto AllocFun = getAllocationFunction(I->getCalledValue());
    if (AllocFun == NULL) {
      // Not an allocation function so did nothing; return false.
      return false;
    }

    AllocSite AS(I, AllocFun->getName(), Uniqtype, true);
    AllocSites.push_back(AS);

    return true;
  }

  Composite::ArithType calcBinOpType(llvm::BinaryOperator *I,
                                     const Composite::ArithType &T1,
                                     const Composite::ArithType &T2)
  {
    assert(I->getNumOperands() == 2);
    switch (I->getOpcode()) {
      case Instruction::Add:
        return T1.add(T2);
      case Instruction::Sub:
        return T1.sub(T2);
      case Instruction::Mul:
        return T1.mul(T2);
      case Instruction::SDiv:
      case Instruction::UDiv:
        return T1.div(T2);
    }
    return Composite::ArithType();
  }

  bool isSizeofMarker(llvm::CallInst *CallI) {
    return CallI->getCalledValue()->stripPointerCasts() == SizeofMarker;
  }

  Composite::ArithType getTypeFromMarker(llvm::CallInst *CallI) {
    auto TypeArg = cast<ConstantDataArray>(CallI->getArgOperand(0));
    std::string UniqtypeStr = TypeArg->getAsString();
    return Composite::ArithType(UniqtypeStr);
  }

  // Recurse backwards down the use-def chain to find out if this has a type.
  Composite::ArithType getType(llvm::Value *Val) {
    // Base case: A potential __crunch_sizeof__ marker call.
    if (auto CallI = dyn_cast<CallInst>(Val)) {
      if (isSizeofMarker(CallI)) {
        return getTypeFromMarker(CallI);
      } // TODO: Could be a sizeof-returning function.
    } else if (auto BinI = dyn_cast<BinaryOperator>(Val)) {
      llvm::Value *V1 = BinI->getOperand(0);
      llvm::Value *V2 = BinI->getOperand(1);
      return calcBinOpType(BinI, getType(V1), getType(V2));
    } else if (auto Inst = dyn_cast<Instruction>(Val)) {
      for (Use &U : Inst->operands()) {
        Composite::ArithType Ty = getType(U.get());
        if (!Ty.isVoid()) {
          return Ty;
        }
      }
    }
    return Composite::ArithType();
  }

  void handleReturnInst(ReturnInst *I, Composite::ArithType &Ty) {
    // FunctionTypes[canonicalise(CurrentFunction)] = Ty;
  }

  void handleBinaryOperator(BinaryOperator *I, llvm::User *From,
                            Composite::ArithType &Ty)
  {
    /* `From' is the operand which already has a type. Find the other one, and
     * see if it has a type as well. */
    llvm::Value *V1 = I->getOperand(0);
    llvm::Value *V2 = I->getOperand(1);
    Composite::ArithType T1, T2;
    // Find the type for the operand we didn't arrive from.
    if (From == V1) { // If we came from the first operand
      T1 = Ty;
      T2 = getType(V2);
    } else if (From == V2) { // We came from the second operand
      T1 = getType(V1);
      T2 = Ty;
    } else {
      assert(false && "`From' not in I->operands");
    }
    Composite::ArithType Result = calcBinOpType(I, T1, T2);
    for (llvm::User *U : I->users()) {
      propagateToUsers(I, U, Result);
    }
  }

  void propagateToUsers(llvm::User *From, llvm::User *To,
                        Composite::ArithType &Ty)
  {
    if (auto Call = dyn_cast<CallInst>(To)) {
      handleCallInst(Call, Ty);
    } else if (auto Ret = dyn_cast<ReturnInst>(To)) {
      handleReturnInst(Ret, Ty);
    } else if (auto Bin = dyn_cast<BinaryOperator>(To)) {
      handleBinaryOperator(Bin, From, Ty);
    } else {
      for (llvm::User *U : To->users()) {
        propagateToUsers(To, U, Ty);
      }
    }
  }

  void propagateSizeofFromMarker(llvm::User *U) {
    /* Get the first argument from the call - this contains the name of the
     * uniqtype. */
    auto CallI = cast<llvm::CallInst>(U);
    assert(isSizeofMarker(CallI));
    Composite::ArithType Type = getTypeFromMarker(CallI);

    for (llvm::User *U : CallI->users()) {
      propagateToUsers(CallI, U, Type);
    }
  }

public:
  bool run() {
    pass++;
    bool ret = false;
    AllocSites.clear();

    // Initialise the type assignments to known sizeof-returning functions.
    TypeAssigns = FunctionTypes;

    // Loop through each use of __crunch__sizeof__().
    for (llvm::User *U : SizeofMarker->users()) {
      if (auto Cast = dyn_cast<llvm::BitCastOperator>(U)) {
        for (llvm::User *CastUser : Cast->users()) {
          propagateSizeofFromMarker(CastUser);
        }
      } else {
        propagateSizeofFromMarker(U);
      }
    }

    return ret;
  }

  // Write all the allocation sites out.
  void emit() {
    for (auto it = AllocSites.begin(); it != AllocSites.end(); ++it) {
      it->emit();
    }
  }

private:
  void removeSizeofMarker(llvm::User *U) {
    auto Call = cast<llvm::CallInst>(U);
    Value *SizeArg = Call->getArgOperand(1);
    Call->replaceAllUsesWith(SizeArg);
    Call->eraseFromParent();
  }

public:
  void removeSizeofMarkers() {
    for (llvm::User *U : SizeofMarker->users()) {
      if (auto Cast = dyn_cast<llvm::BitCastOperator>(U)) {
        for (llvm::User *CastUser : Cast->users()) {
          removeSizeofMarker(CastUser);
        }
      } else {
        removeSizeofMarker(U);
      }
    }
  }

  ModuleHandler(Module &M) :
    TheModule(M), VMContext(M.getContext()), pass(0)
  {
    SizeofMarker = M.getFunction("__crunch_sizeof__");
  }
};

struct AllocSitesPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  AllocSitesPass() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    /* Stop iterating when there have been more passes that there are
     * sizeof-returning function. */
    size_t PassCount = 0;

    ModuleHandler Handler(M);
    TypeMap PrevFunctionTypes;
    do {
      PrevFunctionTypes = Handler.FunctionTypes;
      Handler.run();
    } while (PrevFunctionTypes != Handler.FunctionTypes &&
             PassCount++ < Handler.FunctionTypes.size());

    Handler.emit();
    Handler.removeSizeofMarkers();
    closeOutputFiles();

    return true;
  }

  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.setPreservesCFG();
  }
};

} // namespace

llvm::ModulePass *llvm::createAllocSitesSanitizerPass() {
  return new AllocSitesPass();
}

char AllocSitesPass::ID = 0;
static RegisterPass<AllocSitesPass> X("allocs", "Dump allocation sites");
