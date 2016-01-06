##===- projects/sample/tools/sample/Makefile ---------------*- Makefile -*-===##

#
# Indicate where we are relative to the top of the source tree.
#
LEVEL=../..

#
# Give the name of the tool.
#
TOOLNAME=p5

#
# List libraries that we'll need
# We use LIBS because sample is a dynamic library.
#
USEDLIBS = summary.a SummaryCpp.a CSE.a LICM.a dominance.a cfg.a transform.a

LINK_COMPONENTS := irreader bitreader bitwriter asmparser instrumentation scalaropts ipo vectorize

#
# Include Makefile.common so we know what to do.
#
include $(LEVEL)/Makefile.common

