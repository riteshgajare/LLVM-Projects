//===-- LC3TargetInfo.cpp - LC3 Target Implementation -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "LC3.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target llvm::TheLC3Target;

static unsigned LC3_TripleMatchQuality(const std::string &TT) {
  // This class always works, but shouldn't be the default in most cases.
  return 1;
}

extern "C" void LLVMInitializeLC3TargetInfo() { 
  TargetRegistry::RegisterTarget(TheLC3Target, "lc3", "LC3",&LC3_TripleMatchQuality);
}
