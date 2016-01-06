//===-- LC3SelectionDAGInfo.cpp - LC3 SelectionDAG Info ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the LC3SelectionDAGInfo class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "LC3-selectiondag-info"
#include "LC3TargetMachine.h"
using namespace llvm;

LC3SelectionDAGInfo::LC3SelectionDAGInfo(const LC3TargetMachine &TM)
  : TargetSelectionDAGInfo(TM) {
}

LC3SelectionDAGInfo::~LC3SelectionDAGInfo() {
}
