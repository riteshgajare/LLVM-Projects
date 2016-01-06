include_directories( ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/.. )

add_llvm_library(LLVMLC3Info
  LC3TargetInfo.cpp
  )

add_dependencies(LLVMLC3Info LC3CommonTableGen)
