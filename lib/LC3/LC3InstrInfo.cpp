//===-- LC3InstrInfo.cpp - LC3 Instruction Information ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the LC3 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "LC3InstrInfo.h"
#include "LC3.h"
#include "LC3MachineFunctionInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"

#define GET_INSTRINFO_CTOR_DTOR
#include "LC3GenInstrInfo.inc"

using namespace llvm;

static LC3CC::CondCodes GetOppositeBranchCondition(LC3CC::CondCodes CC)
{
  switch(CC) {
  case LC3CC::CC_NE: return LC3CC::CC_EQ;
    case LC3CC::CC_EQ: return LC3CC::CC_EQ;
    case LC3CC::CC_LE: return LC3CC::CC_POS;
    case LC3CC::CC_GE: return LC3CC::CC_NEG;
    case LC3CC::CC_POS: return LC3CC::CC_LE;
    case LC3CC::CC_NEG: return LC3CC::CC_GE;      
  }
  llvm_unreachable("Invalid cond code");
}

LC3InstrInfo::LC3InstrInfo()
  : LC3GenInstrInfo(LC3::ADJCALLSTACKDOWN, LC3::ADJCALLSTACKUP),
    RI(*this) {
}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned LC3InstrInfo::isLoadFromStackSlot(const MachineInstr *MI,
                                             int &FrameIndex) const {
#if 0
  if (MI->getOpcode() == SP::LDri ||
      MI->getOpcode() == SP::LDFri ||
      MI->getOpcode() == SP::LDDFri) {
    if (MI->getOperand(1).isFI() && MI->getOperand(2).isImm() &&
        MI->getOperand(2).getImm() == 0) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
  }
#endif
  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned LC3InstrInfo::isStoreToStackSlot(const MachineInstr *MI,
                                            int &FrameIndex) const {
#if 0
  if (MI->getOpcode() == SP::STri ||
      MI->getOpcode() == SP::STFri ||
      MI->getOpcode() == SP::STDFri) {
    if (MI->getOperand(0).isFI() && MI->getOperand(1).isImm() &&
        MI->getOperand(1).getImm() == 0) {
      FrameIndex = MI->getOperand(0).getIndex();
      return MI->getOperand(2).getReg();
    }
  }
#endif
  return 0;
}

/*static bool IsIntegerCC(unsigned CC)
{
  return true;
  }*/


MachineInstr *
LC3InstrInfo::emitFrameIndexDebugValue(MachineFunction &MF,
                                         int FrameIx,
                                         uint64_t Offset,
                                         const MDNode *MDPtr,
                                         DebugLoc dl) const {
  MachineInstrBuilder MIB = BuildMI(MF, dl, get(LC3::DBG_VALUE))
    .addFrameIndex(FrameIx).addImm(0).addImm(Offset).addMetadata(MDPtr);
  return &*MIB;
}


bool LC3InstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                   MachineBasicBlock *&TBB,
                                   MachineBasicBlock *&FBB,
                                   SmallVectorImpl<MachineOperand> &Cond,
                                   bool AllowModify) const
{
  MachineBasicBlock::iterator I = MBB.end();
  MachineBasicBlock::iterator UnCondBrIter = MBB.end();
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    //When we see a non-terminator, we are done
    if (!isUnpredicatedTerminator(I))
      break;

    //Terminator is not a branch
    if (!I->isBranch())
      return true;

    //Handle Unconditional branches
    if (I->getOpcode() == LC3::BRA) {
      UnCondBrIter = I;

      if (!AllowModify) {
        TBB = I->getOperand(0).getMBB();
        continue;
      }

      while (llvm::next(I) != MBB.end())
        llvm::next(I)->eraseFromParent();

      Cond.clear();
      FBB = 0;

      if (MBB.isLayoutSuccessor(I->getOperand(0).getMBB())) {
        TBB = 0;
        I->eraseFromParent();
        I = MBB.end();
        UnCondBrIter = MBB.end();
        continue;
      }

      TBB = I->getOperand(0).getMBB();
      continue;
    }

    unsigned Opcode = I->getOpcode();
    if (Opcode != LC3::BRCOND)
      return true; //Unknown Opcode

    LC3CC::CondCodes BranchCode = (LC3CC::CondCodes)I->getOperand(1).getImm();

    if (Cond.empty()) {
      MachineBasicBlock *TargetBB = I->getOperand(0).getMBB();
      if (AllowModify && UnCondBrIter != MBB.end() &&
          MBB.isLayoutSuccessor(TargetBB)) {

        //Transform the code
        //
        //    brCC L1
        //    ba L2
        // L1:
        //    ..
        // L2:
        //
        // into
        //
        //   brnCC L2
        // L1:
        //   ...
        // L2:
        //
        BranchCode = GetOppositeBranchCondition(BranchCode);
        MachineBasicBlock::iterator OldInst = I;
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(Opcode))
          .addMBB(UnCondBrIter->getOperand(0).getMBB()).addImm(BranchCode);
        BuildMI(MBB, UnCondBrIter, MBB.findDebugLoc(I), get(LC3::BRA))
          .addMBB(TargetBB);

        OldInst->eraseFromParent();
        UnCondBrIter->eraseFromParent();

        UnCondBrIter = MBB.end();
        I = MBB.end();
        continue;
      }
      FBB = TBB;
      TBB = I->getOperand(0).getMBB();
      Cond.push_back(MachineOperand::CreateImm(BranchCode));
      continue;
    }
    //FIXME: Handle subsequent conditional branches
    //For now, we can't handle multiple conditional branches
    return true;
  }
  return false;
}

unsigned
LC3InstrInfo::InsertBranch(MachineBasicBlock &MBB,MachineBasicBlock *TBB,
                             MachineBasicBlock *FBB,
                             const SmallVectorImpl<MachineOperand> &Cond,
                             DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 1 || Cond.size() == 0) &&
         "LC3 branch conditions should have one component!");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(LC3::BRA)).addMBB(TBB);
    return 1;
  }

  //Conditional branch
  unsigned CC = Cond[0].getImm();


  BuildMI(&MBB, DL, get(LC3::BRCOND)).addMBB(TBB).addImm(CC);

  if (!FBB)
    return 1;

  BuildMI(&MBB, DL, get(LC3::BRA)).addMBB(FBB);

  return 2;
}

unsigned LC3InstrInfo::RemoveBranch(MachineBasicBlock &MBB) const
{
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    if (I->getOpcode() != LC3::BRA
        && I->getOpcode() != LC3::BRCOND)
      break; // Not a branch

    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

void LC3InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator I, DebugLoc DL,
                                 unsigned DestReg, unsigned SrcReg,
                                 bool KillSrc) const {

  BuildMI(MBB, I, DL, get(LC3::ADDri), DestReg)
    .addReg(SrcReg, getKillRegState(KillSrc)).addImm(0);
}

void LC3InstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();
  
  // On the order of operands here: think "[FrameIdx + 0] = SrcReg".
  BuildMI(MBB, I, DL, get(LC3::STR)).addReg(SrcReg, getKillRegState(isKill)).addFrameIndex(FI).addImm(0);
    
}

void LC3InstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     unsigned DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end()) DL = I->getDebugLoc();

  BuildMI(MBB, I, DL, get(LC3::LDR), DestReg).addFrameIndex(FI).addImm(0);
}

unsigned LC3InstrInfo::getGlobalBaseReg(MachineFunction *MF) const
{
#if 0
  LC3MachineFunctionInfo *LC3FI = MF->getInfo<LC3MachineFunctionInfo>();
  unsigned GlobalBaseReg = LC3FI->getGlobalBaseReg();
  if (GlobalBaseReg != 0)
    return GlobalBaseReg;

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();

  GlobalBaseReg = RegInfo.createVirtualRegister(&SP::IntRegsRegClass);


  DebugLoc dl;

  BuildMI(FirstMBB, MBBI, dl, get(SP::GETPCX), GlobalBaseReg);
  LC3FI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
#endif
  return 0;
}

