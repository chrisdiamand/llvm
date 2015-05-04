add_llvm_library(LLVMAllocSites
  AllocFunction.cpp
  AllocPass.cpp
  Composite.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms
  )

add_dependencies(LLVMAllocSites intrinsics_gen)
