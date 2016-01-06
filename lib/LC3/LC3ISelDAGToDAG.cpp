//===-- LC3ISelDAGToDAG.cpp - A dag to dag inst selector for LC3 ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines an instruction selector for the LC3 target.
//
//===----------------------------------------------------------------------===//

#include "LC3TargetMachine.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

//===----------------------------------------------------------------------===//
// Instruction Selector Implementation
//===----------------------------------------------------------------------===//

//===--------------------------------------------------------------------===//
/// LC3DAGToDAGISel - LC3 specific code to select LC3 machine
/// instructions for SelectionDAG operations.
///
namespace {
class LC3DAGToDAGISel : public SelectionDAGISel {
  //LC3TargetMachine& TM;
public:
  explicit LC3DAGToDAGISel(LC3TargetMachine &tm)
    : SelectionDAGISel(tm) //,
      //TM(tm) {
  {
  }

  // Must implement to make class concrete
  SDNode *Select(SDNode *N);

  virtual const char *getPassName() const {
    return "LC3 DAG->DAG Pattern Instruction Selection";
  }

  bool SelectADDRri(SDValue N, SDValue &Base, SDValue &Offset);

  // Include the pieces autogenerated from the target description.
#include "LC3GenDAGISel.inc"

private:
  //SDNode* getGlobalBaseReg();
};
}  // end anonymous namespace

SDNode *LC3DAGToDAGISel::Select(SDNode *N) {
  if (N->isMachineOpcode())
    return NULL;   // Already selected.
  return SelectCode(N);
}

/// createLC3ISelDag - This pass converts a legalized DAG into a
/// LC3-specific DAG, ready for instruction scheduling.
///
FunctionPass *llvm::createLC3ISelDag(LC3TargetMachine &TM) {
  return new LC3DAGToDAGISel(TM);
}

bool LC3DAGToDAGISel::SelectADDRri(SDValue Addr,
                                     SDValue &Base, SDValue &Offset) {
  if (FrameIndexSDNode *FIN = dyn_cast<FrameIndexSDNode>(Addr)) {
    Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), MVT::i32);
    Offset = CurDAG->getTargetConstant(0, MVT::i32);
    return true;
  }
  if (Addr.getOpcode() == ISD::TargetExternalSymbol ||
      Addr.getOpcode() == ISD::TargetGlobalAddress)
    return false;  // direct calls.

  if (Addr.getOpcode() == ISD::ADD) {
    if (ConstantSDNode *CN = dyn_cast<ConstantSDNode>(Addr.getOperand(1))) {
      if (isInt<13>(CN->getSExtValue())) {
        if (FrameIndexSDNode *FIN =
                dyn_cast<FrameIndexSDNode>(Addr.getOperand(0))) {
          // Constant offset from frame ref.
          Base = CurDAG->getTargetFrameIndex(FIN->getIndex(), MVT::i32);
        } else {
          Base = Addr.getOperand(0);
        }
        Offset = CurDAG->getTargetConstant(CN->getZExtValue(), MVT::i32);
        return true;
      }
    }
  }
  Base = Addr;
  Offset = CurDAG->getTargetConstant(0, MVT::i32);
  return true;
}