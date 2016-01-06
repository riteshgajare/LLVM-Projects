add_llvm_library(LLVMLC3Desc
  LC3MCTargetDesc.cpp
  LC3MCAsmInfo.cpp
  )

add_dependencies(LLVMLC3Desc LC3CommonTableGen)
