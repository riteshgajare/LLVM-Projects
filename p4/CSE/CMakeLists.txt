
if(use_c)
  add_library(CSE STATIC CSE_C.c)
else()
  add_library(CSE STATIC CSE_Cpp.cpp)
endif()
