##===- projects/sample/lib/sample/Makefile -----------------*- Makefile -*-===##

#
# Indicate where we are relative to the top of the source tree.
#
LEVEL=../..

#
# Give the name of a library.  This will build a dynamic version.
#
LIBRARYNAME=LC3
TARGET = LC3

BUILT_SOURCES = LC3GenRegisterInfo.inc LC3GenInstrInfo.inc \
                LC3GenAsmWriter.inc LC3GenDAGISel.inc \
                LC3GenSubtargetInfo.inc LC3GenCallingConv.inc


DIRS = TargetInfo MCTargetDesc

#
# Include Makefile.common so we know what to do.
#
include $(LEVEL)/Makefile.common
