//===-- LC3FrameLowering.h - Define frame lowering for LC3 --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//
//===----------------------------------------------------------------------===//

#ifndef LC3_FRAMEINFO_H
#define LC3_FRAMEINFO_H

#include "LC3.h"
#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
  class LC3Subtarget;

class LC3FrameLowering : public TargetFrameLowering {
public:
  explicit LC3FrameLowering()
    : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 8, 0) {
  }

  /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
  /// the function.
  void emitPrologue(MachineFunction &MF) const;
  void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const;

  bool hasFP(const MachineFunction &MF) const { return false; }


  virtual void
  eliminateCallFramePseudoInstr(MachineFunction &MF,
                                MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI) const;

};

} // End llvm namespace

#endif
