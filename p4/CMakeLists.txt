include_directories(.)
add_executable(p4 main.cpp)

# Figure out where the libraries are that we will link against
if ("${LLVM_PACKAGE_VERSION}" VERSION_GREATER "3.4.2")
  llvm_map_components_to_libnames(llvm_libs analysis bitreader bitwriter codegen core ipa asmparser irreader instcombine instrumentation mc objcarcopts scalaropts support ipo target transformutils vectorize)
else()
  llvm_map_components_to_libraries(llvm_libs bitreader bitwriter asmparser irreader instrumentation scalaropts ipo vectorize)
endif()

target_link_libraries(p4 ${llvm_libs} CSE summary dominance transform)
