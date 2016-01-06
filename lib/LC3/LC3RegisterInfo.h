#ifndef LC3REGISTERINFO_H
#define LC3REGISTERINFO_H

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "LC3GenRegisterInfo.inc"

namespace llvm {

class TargetInstrInfo;
class Type;

struct LC3RegisterInfo : public LC3GenRegisterInfo {
  const TargetInstrInfo &TII;

  LC3RegisterInfo(const TargetInstrInfo &tii);

  /// Code Generation virtual methods...
  const uint16_t *getCalleeSavedRegs(const MachineFunction *MF = 0) const;

  BitVector getReservedRegs(const MachineFunction &MF) const;

  //void eliminateFrameIndex(MachineBasicBlock::iterator II,
  //                         int SPAdj, RegScavenger *RS = NULL) const;

  virtual void eliminateFrameIndex(MachineBasicBlock::iterator MI,
                                   int SPAdj, unsigned FIOperandNum,
                                   RegScavenger *RS = NULL) const;

  /*  virtual void eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
      MachineBasicBlock::iterator I) const;*/

  // Debug information queries.
  unsigned getFrameRegister(const MachineFunction &MF) const;
};

} // end namespace llvm

#endif
