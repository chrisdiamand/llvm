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
#include "llvm/Transforms/AllocSites/ArithType.h"

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
  llvm::Instruction         *Instr;
  std::string               Name;
  const Crunch::ArithType   AllocType;

public:
  AllocSite(llvm::Instruction *_Instr, std::string _Name,
            Crunch::ArithType _AllocType) :
    Instr(_Instr), Name(_Name), AllocType(_AllocType) {};

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

class ModuleHandler {
private:
  llvm::Module                &TheModule;
  llvm::LLVMContext           &VMContext;
  llvm::Function              *SizeofMarker;
  std::vector<AllocSite>      AllocSites;
  std::set<llvm::Function *>  VisitedFuncs;

  Crunch::ArithType calcBinOpType(llvm::BinaryOperator *I,
                                     const Crunch::ArithType &T1,
                                     const Crunch::ArithType &T2)
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
    return Crunch::ArithType();
  }

  bool isSizeofMarker(llvm::CallInst *CallI) {
    return CallI->getCalledValue()->stripPointerCasts() == SizeofMarker;
  }

  Crunch::ArithType getTypeFromMarker(llvm::CallInst *CallI) {
    auto TypeArg = cast<ConstantDataArray>(CallI->getArgOperand(0));
    std::string UniqtypeStr = TypeArg->getAsString();
    return Crunch::ArithType(UniqtypeStr);
  }

  Crunch::ArithType getTypeFromCallToSizeofReturningFunc(CallInst *CallI) {
    llvm::Function *CalledFunc = CallI->getCalledFunction();
    if (!CalledFunc) {
      fprintf(stderr,
              "Warning: Call to indirect function in sizeof expression.\n");
      return Crunch::ArithType();
    }

    // Avoid infinite recursion by keeping track of where we've been.
    if (VisitedFuncs.find(CalledFunc) != VisitedFuncs.end()) {
      errs() << "Warning: Recursive sizeof-returning function: \'"
             << CalledFunc->getName() << "()\'\n";
      return Crunch::ArithType();
    }
    VisitedFuncs.insert(CalledFunc);

    Crunch::ArithType ReturnType;

    /* Find return instructions. We don't need to look through every single
     * one, since they can only be at the end of basic blocks. */
    auto &BBList = CalledFunc->getBasicBlockList();
    for (auto it = BBList.begin();
         it != BBList.end(); ++it) {
      auto Term = it->getTerminator();
      if (auto RetI = dyn_cast<ReturnInst>(Term)) {
        if (auto RetVal = RetI->getReturnValue()) {
          auto Ty = getType(RetVal);
          /* If there are multiple return statements with different types,
           * return void. */
          if (!ReturnType.isVoid() && ReturnType != Ty) {
            return Crunch::ArithType();
          }
          ReturnType = Ty;
        }
      }
    }
    return ReturnType;
  }

  // Recurse backwards down the use-def chain to find out if this has a type.
  Crunch::ArithType getType(llvm::Value *Val) {
    // Base case: A potential __crunch_sizeof__ marker call.
    if (auto CallI = dyn_cast<CallInst>(Val)) {
      if (isSizeofMarker(CallI)) {
        return getTypeFromMarker(CallI);
      } else { // It could be a sizeof-returning function.
        return getTypeFromCallToSizeofReturningFunc(CallI);
      }
    } else if (auto BinI = dyn_cast<BinaryOperator>(Val)) {
      llvm::Value *V1 = BinI->getOperand(0);
      llvm::Value *V2 = BinI->getOperand(1);
      return calcBinOpType(BinI, getType(V1), getType(V2));
    } else if (auto Inst = dyn_cast<Instruction>(Val)) {
      for (Use &U : Inst->operands()) {
        Crunch::ArithType Ty = getType(U.get());
        if (!Ty.isVoid()) {
          return Ty;
        }
      }
    }
    return Crunch::ArithType();
  }

  /* Recurse the def-use chain of each allocation function, stopping when an
   * allocation site is reached. */
  void findAllocSite(llvm::Value *Prev, llvm::Value *Val,
                     Crunch::AllocFunction *AF)
  {
    if (auto CallI = dyn_cast<CallInst>(Val)) {
      if (Prev != CallI->getCalledValue()) {
        return;
      }
      // We've found an allocation site.
      assert(AF->getSizeArg() < CallI->getNumArgOperands());
      llvm::Value *SizeArg = CallI->getArgOperand(AF->getSizeArg());
      AllocSite AS(CallI, AF->getName(), getType(SizeArg));
      AllocSites.push_back(AS);

    } else if (auto StoreI = dyn_cast<StoreInst>(Val)) {
      for (User *U : StoreI->getPointerOperand()->users()) {
        if (auto LoadI = dyn_cast<LoadInst>(U)) {
          for (User *LoadU : LoadI->users()) {
            if (LoadU != StoreI) {
              findAllocSite(LoadI, LoadU, AF);
            }
          }
        }
      }
    }
  }

public:
  void run() {
    if (SizeofMarker == nullptr) {
      return;
    }

    AllocSites.clear();

    // Loop through all the allocation functions.
    for (auto it = Crunch::AllocFunction::getAll().begin();
         it != Crunch::AllocFunction::getAll().end(); ++it)
    {
      llvm::Function *Func = TheModule.getFunction(it->first);
      if (Func == nullptr) {
        continue;
      }
      for (llvm::User *U : Func->users()) {
        VisitedFuncs.clear();
        findAllocSite(Func, U, it->second);
      }
    }
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
    if (SizeofMarker == nullptr) {
      return;
    }

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
    TheModule(M), VMContext(M.getContext())
  {
    SizeofMarker = M.getFunction("__crunch_sizeof__");
  }
};

struct AllocSitesPass : public ModulePass {
  static char ID; // Pass identification, replacement for typeid
  AllocSitesPass() : ModulePass(ID) {}

  bool runOnModule(Module &M) override {
    ModuleHandler Handler(M);
    Handler.run();
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
