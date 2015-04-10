add_llvm_library(LLVMCrunch
  AllocFunction.cpp
  Crunch.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_INCLUDE_DIR}/llvm/Transforms
  )

add_dependencies(LLVMCrunch intrinsics_gen)
