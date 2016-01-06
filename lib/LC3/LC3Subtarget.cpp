//===-- LC3Subtarget.cpp - SPARC Subtarget Information ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the SPARC specific subclass of TargetSubtargetInfo.
//
//===----------------------------------------------------------------------===//

#include "LC3Subtarget.h"
#include "LC3.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "LC3GenSubtargetInfo.inc"

using namespace llvm;

void LC3Subtarget::anchor() { }

LC3Subtarget::LC3Subtarget(const std::string &TT, const std::string &CPU,
                               const std::string &FS) :
  LC3GenSubtargetInfo(TT, CPU, FS)
{  
  // Parse features string.
  ParseSubtargetFeatures(CPU, FS);
}
