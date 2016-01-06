//===-- LC3FrameLowering.cpp - LC3 Frame Information ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LC3 implementation of TargetFrameLowering class.
//
//===----------------------------------------------------------------------===//

#include "LC3FrameLowering.h"
#include "LC3InstrInfo.h"
#include "LC3MachineFunctionInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

void LC3FrameLowering::emitPrologue(MachineFunction &MF) const {

  MachineBasicBlock &MBB = MF.front();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  const LC3InstrInfo &TII =
    *static_cast<const LC3InstrInfo*>(MF.getTarget().getInstrInfo());
  MachineBasicBlock::iterator MBBI = MBB.begin();
  DebugLoc dl = MBBI != MBB.end() ? MBBI->getDebugLoc() : DebugLoc();

  // Get the number of bytes to allocate from the FrameInfo
  int NumBytes = (int) MFI->getStackSize();

  // Emit the correct instructions based on the number of bytes in
  // the frame. 
  //    1 word for return value or address 
  //    1 word for return address register, R7
  //    1 word for old frame pointer
  // +  words for passing parameters on the stack
  // ----------
  //   3 * 4 bytes per word = 12 bytes
  NumBytes += 12;

  // Round up to next doubleword boundary -- a word boundary
  // is required by the ABI.
  NumBytes = (NumBytes + 3) & ~3;
  NumBytes = -NumBytes;

    BuildMI(MBB, MBBI, dl, TII.get(LC3::STR)).addReg(LC3::R7).addReg(LC3::R6)
      .addImm(-1*4);

    BuildMI(MBB, MBBI, dl, TII.get(LC3::STR)).addReg(LC3::R5).addReg(LC3::R6)
      .addImm(-2*4);
    
    BuildMI(MBB, MBBI, dl, TII.get(LC3::ADDrr), LC3::R5)
      .addReg(LC3::R6).addImm(-12);

  if (NumBytes >= -64) {        
    BuildMI(MBB, MBBI, dl, TII.get(LC3::ADDrr), LC3::R6)
      .addReg(LC3::R6).addImm(NumBytes);   
  } else {
    assert(NumBytes >= -512);

    BuildMI(MBB, MBBI, dl, TII.get(LC3::LI), LC3::R0)
      .addImm(NumBytes);
    BuildMI(MBB, MBBI, dl, TII.get(LC3::ADDrr), LC3::R6)
      .addReg(LC3::R6).addReg(LC3::R0);
  }
}

void LC3FrameLowering::emitEpilogue(MachineFunction &MF,
                                  MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const LC3InstrInfo &TII =
    *static_cast<const LC3InstrInfo*>(MF.getTarget().getInstrInfo());
  MachineFrameInfo *MFI = MF.getFrameInfo();

  DebugLoc dl = MBBI->getDebugLoc();

  assert(MBBI->getOpcode() == LC3::RET &&
         "Can only put epilog before 'RET' instruction!");
  
  int NumBytes = (int) MFI->getStackSize();

  NumBytes += 8;

  // Round up to next doubleword boundary -- a word boundary
  // is required by the ABI.
  NumBytes = (NumBytes + 3) & ~3;

  if (NumBytes < 64) {        
    BuildMI(MBB, MBBI, dl, TII.get(LC3::ADDrr), LC3::R6)
      .addReg(LC3::R6).addImm(NumBytes);   
  } else {
    BuildMI(MBB, MBBI, dl, TII.get(LC3::LI), LC3::R0).addImm(NumBytes);
    BuildMI(MBB, MBBI, dl, TII.get(LC3::ADDrr), LC3::R6)
      .addReg(LC3::R6).addReg(LC3::R0);
  }

  BuildMI(MBB, MBBI, dl, TII.get(LC3::LDR),LC3::R5).addReg(LC3::R6)
    .addImm(-2*4);
  
    BuildMI(MBB, MBBI, dl, TII.get(LC3::LDR),LC3::R7).addReg(LC3::R6)
      .addImm(-1*4);
}

void
LC3FrameLowering::eliminateCallFramePseudoInstr(MachineFunction &MF,
						MachineBasicBlock &MBB,
						MachineBasicBlock::iterator I) const
{
  const LC3InstrInfo &TII =
    *static_cast<const LC3InstrInfo*>(MF.getTarget().getInstrInfo());

  MachineInstr &MI = *I;
  DebugLoc dl = MI.getDebugLoc();
  int Size = MI.getOperand(0).getImm();
  if (MI.getOpcode() == LC3::ADJCALLSTACKDOWN)
    Size = -Size;
  if (Size)
    BuildMI(MBB, I, dl, TII.get(LC3::ADDri), LC3::R6).addReg(LC3::R6).addImm(Size);
  MBB.erase(I);
}
