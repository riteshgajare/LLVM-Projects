# C library is built by default

if (use_c)
  # add additional files to end of this list
  add_library(summary STATIC summary.c)
else(use_c++)
  # add additional files to end of this list
  add_library(summary STATIC summary.cpp)
else()
  message("Error configuring library. Either C or C++ must be selected.")
endif()

