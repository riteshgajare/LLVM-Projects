
if(use_c)
  add_library(LICM STATIC LICM_C.c)
else()
  add_library(LICM STATIC LICM_Cpp.cpp)
endif()
