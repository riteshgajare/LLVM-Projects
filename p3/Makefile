##===- projects/sample/tools/sample/Makefile ---------------*- Makefile -*-===##

#
# Indicate where we are relative to the top of the source tree.
#
LEVEL=../..

#
# Give the name of the tool.
#
TOOLNAME=p3

#
# List libraries that we'll need
# We use LIBS because sample is a dynamic library.
#
USEDLIBS=summary.a dominance.a

LINK_COMPONENTS := bitreader bitwriter analysis

#
# Include Makefile.common so we know what to do.
#
include $(LEVEL)/Makefile.common

