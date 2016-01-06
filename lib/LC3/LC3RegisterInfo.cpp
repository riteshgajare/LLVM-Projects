#include "LC3RegisterInfo.h"
#include "LC3.h"
#include "llvm/IR/Type.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"

#define GET_REGINFO_TARGET_DESC
#include "LC3GenRegisterInfo.inc"

using namespace llvm;

LC3RegisterInfo::LC3RegisterInfo(const TargetInstrInfo &tii)
  : LC3GenRegisterInfo(LC3::R7), TII(tii) {
}

const uint16_t* LC3RegisterInfo::getCalleeSavedRegs(const MachineFunction *MF)
                                                                         const {
  static const uint16_t CalleeSavedRegs[] = { 0 };
  return CalleeSavedRegs;
}

BitVector LC3RegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(LC3::R4);
  Reserved.set(LC3::R5);
  Reserved.set(LC3::R6);
  Reserved.set(LC3::NZP);
  return Reserved;
}

void LC3RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                   int SPAdj, unsigned FIOperandNum,
					  RegScavenger *RS) const {
//void
//LC3RegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
//                                       int SPAdj, RegScavenger *RS) const {

  assert(SPAdj == 0 && "Unexpected");

  unsigned i = 0;
  MachineInstr &MI = *II;
  DebugLoc dl = MI.getDebugLoc();
  while (!MI.getOperand(i).isFI()) {
    ++i;
    assert(i < MI.getNumOperands() && "Instr doesn't have FrameIndex operand!");
  }

  int FrameIndex = MI.getOperand(i).getIndex();

  // Addressable stack objects are accessed using neg. offsets from %fp
  MachineFunction &MF = *MI.getParent()->getParent();
  int Offset = MF.getFrameInfo()->getObjectOffset(FrameIndex) +
               MI.getOperand(i+1).getImm();

  // Replace frame index with a frame pointer reference.
  if (Offset >= -64 && Offset <= 64) {
    // If the offset is small enough to fit in the immediate field, directly
    // encode it.
    MI.getOperand(i).ChangeToRegister(LC3::R5, false);
    MI.getOperand(i+1).ChangeToImmediate(Offset);
  } else {
    assert ((Offset < (1<<18)) && "Oops! really big stack offset not supported for LC3");

    // LI should be good enough
    BuildMI(*MI.getParent(), II, dl, TII.get(LC3::LI), LC3::R0).addImm(Offset);
    // Emit R0 = R0 + R5
    BuildMI(*MI.getParent(), II, dl, TII.get(LC3::ADDrr), LC3::R0).addReg(LC3::R0)
      .addReg(LC3::R5);

    // Insert: R0+0 into the user.
    MI.getOperand(i).ChangeToRegister(LC3::R0, false);
    MI.getOperand(i+1).ChangeToImmediate(0);
  }
}

unsigned LC3RegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  return LC3::R5;
}

/*
void LC3RegisterInfo::eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator I) const {
  MachineInstr &MI = *I;
  DebugLoc dl = MI.getDebugLoc();
  int Size = MI.getOperand(0).getImm();
  if (MI.getOpcode() == LC3::ADJCALLSTACKDOWN)
    Size = -Size;
  if (Size)
    BuildMI(MBB, I, dl, TII.get(LC3::ADDri), LC3::R6).addReg(LC3::R6).addImm(Size);
  MBB.erase(I);
  }*/
