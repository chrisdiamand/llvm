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

  bool runOnBinaryOperator(llvm::BinaryOperator *I) {
    assert(I->getNumOperands() == 2);
    /*
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
    */
    return false;
  }

  void handleReturnInst(ReturnInst *I, Composite::ArithType &Ty) {
    // FunctionTypes[canonicalise(CurrentFunction)] = Ty;
  }

  void handleBinaryOperator(BinaryOperator *I, Composite::ArithType &Ty) {
    ;
  }

  void propagateToUsers(llvm::User *From, llvm::User *To,
                        Composite::ArithType &Ty)
  {
    if (auto Call = dyn_cast<CallInst>(To)) {
      handleCallInst(Call, Ty);
    } else if (auto Ret = dyn_cast<ReturnInst>(To)) {
      handleReturnInst(Ret, Ty);
    } else if (auto Bin = dyn_cast<BinaryOperator>(To)) {
      handleBinaryOperator(Bin, Ty);
    } else {
      for (llvm::User *U : To->users()) {
        propagateToUsers(To, U, Ty);
      }
    }
  }

  void propagateSizeofFromMarker(llvm::User *U) {
    /* Get the first argument from the call - this contains the name of the
     * uniqtype. */
    auto Call = cast<llvm::CallInst>(U);
    auto TypeArg = cast<ConstantDataArray>(Call->getArgOperand(0));
    std::string UniqtypeStr = TypeArg->getAsString();
    Composite::ArithType Type(UniqtypeStr);

    for (llvm::User *U : Call->users()) {
      propagateToUsers(Call, U, Type);
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
