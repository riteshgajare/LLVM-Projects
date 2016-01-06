//===-- LC3AsmPrinter.cpp - LC3 LLVM assembly writer ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format SPARC assembly language.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "asm-printer"
#include "LC3.h"
#include "LC3InstrInfo.h"
#include "LC3TargetMachine.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Target/Mangler.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

const char *llvm::LC3CondCodeToString(LC3CC::CondCodes CC) {
  switch (CC) {
  case LC3CC::CC_NE: return "np";
  case LC3CC::CC_EQ: return "z";
  case LC3CC::CC_LE: return "nz";
  case LC3CC::CC_GE: return "zp";
  case LC3CC::CC_POS: return "p";
  case LC3CC::CC_NEG: return "n";
    //default:  llvm_unreachable("Invalid cond code"); break;
  }
  return 0;
}

namespace {
  class LC3AsmPrinter : public AsmPrinter {
  public:
    explicit LC3AsmPrinter(TargetMachine &TM, MCStreamer &Streamer)
      : AsmPrinter(TM, Streamer) {}

    virtual const char *getPassName() const {
      return "LC3 Assembly Printer";
    }

    void printOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);
    void printMemOperand(const MachineInstr *MI, int opNum, raw_ostream &OS,
                         const char *Modifier = 0);
    void printCCOperand(const MachineInstr *MI, int opNum, raw_ostream &OS);

    virtual void EmitInstruction(const MachineInstr *MI) {
      SmallString<128> Str;
      raw_svector_ostream OS(Str);
      printInstruction(MI, OS);
      OutStreamer.EmitRawText(OS.str());
    }
    void printInstruction(const MachineInstr *MI, raw_ostream &OS);// autogen'd.
    static const char *getRegisterName(unsigned RegNo);

    bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                         unsigned AsmVariant, const char *ExtraCode,
                         raw_ostream &O);
    bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                               unsigned AsmVariant, const char *ExtraCode,
                               raw_ostream &O);

    bool printGetPCX(const MachineInstr *MI, unsigned OpNo, raw_ostream &OS);
    
    virtual bool isBlockOnlyReachableByFallthrough(const MachineBasicBlock *MBB)
                       const;

    virtual MachineLocation getDebugValueLocation(const MachineInstr *MI) const;
  };
} // end of anonymous namespace

#include "LC3GenAsmWriter.inc"

void LC3AsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                   raw_ostream &O) {
  const MachineOperand &MO = MI->getOperand (opNum);
  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << "%" << StringRef(getRegisterName(MO.getReg())).lower();
    break;

  case MachineOperand::MO_Immediate:
    O << (int)MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    O << *MO.getMBB()->getSymbol();
    return;
  case MachineOperand::MO_GlobalAddress:    
    O << *getSymbol(MO.getGlobal());
    break;
  case MachineOperand::MO_ExternalSymbol:
    O << MO.getSymbolName();
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << MAI->getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << "_"
      << MO.getIndex();
    break;
  default:
    llvm_unreachable("<unknown operand type>");
  }
}

void LC3AsmPrinter::printMemOperand(const MachineInstr *MI, int opNum,
                                      raw_ostream &O, const char *Modifier) {
  printOperand(MI, opNum, O);

  // If this is an ADD operand, emit it like normal operands.
  O << ", ";
  printOperand(MI, opNum+1, O);  
}

bool LC3AsmPrinter::printGetPCX(const MachineInstr *MI, unsigned opNum,
                                  raw_ostream &O) {
#if 0
  std::string operand = "";
  const MachineOperand &MO = MI->getOperand(opNum);
  switch (MO.getType()) {
  default: llvm_unreachable("Operand is not a register");
  case MachineOperand::MO_Register:
    assert(TargetRegisterInfo::isPhysicalRegister(MO.getReg()) &&
           "Operand is not a physical register ");
    assert(MO.getReg() != SP::O7 && 
           "%o7 is assigned as destination for getpcx!");
    operand = "%" + StringRef(getRegisterName(MO.getReg())).lower();
    break;
  }

  unsigned mfNum = MI->getParent()->getParent()->getFunctionNumber();
  unsigned bbNum = MI->getParent()->getNumber();

  O << '\n' << ".LLGETPCH" << mfNum << '_' << bbNum << ":\n";
  O << "\tcall\t.LLGETPC" << mfNum << '_' << bbNum << '\n' ;

  O << "\t  sethi\t"
    << "%hi(_GLOBAL_OFFSET_TABLE_+(.-.LLGETPCH" << mfNum << '_' << bbNum 
    << ")), "  << operand << '\n' ;

  O << ".LLGETPC" << mfNum << '_' << bbNum << ":\n" ;
  O << "\tor\t" << operand  
    << ", %lo(_GLOBAL_OFFSET_TABLE_+(.-.LLGETPCH" << mfNum << '_' << bbNum
    << ")), " << operand << '\n';
  O << "\tadd\t" << operand << ", %o7, " << operand << '\n'; 
  #endif
  return true;
}

void LC3AsmPrinter::printCCOperand(const MachineInstr *MI, int opNum,
                                     raw_ostream &O) {
  int CC = (int)MI->getOperand(opNum).getImm();
  O << LC3CondCodeToString((LC3CC::CondCodes)CC);
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool LC3AsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                      unsigned AsmVariant,
                                      const char *ExtraCode,
                                      raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    if (ExtraCode[1] != 0) return true; // Unknown modifier.

    switch (ExtraCode[0]) {
    default: return true;  // Unknown modifier.
    case 'r':
     break;
    }
  }

  printOperand(MI, OpNo, O);

  return false;
}

bool LC3AsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                            unsigned OpNo, unsigned AsmVariant,
                                            const char *ExtraCode,
                                            raw_ostream &O) {
  llvm_unreachable("PrintAsmMemoryOperand");
#if 0
  if (ExtraCode && ExtraCode[0])
    return true;  // Unknown modifier

  O << '[';
  printMemOperand(MI, OpNo, O);
  O << ']';
#endif
  return false;
}

/// isBlockOnlyReachableByFallthough - Return true if the basic block has
/// exactly one predecessor and the control transfer mechanism between
/// the predecessor and this block is a fall-through.
///
/// This overrides AsmPrinter's implementation to handle delay slots.
bool LC3AsmPrinter::
isBlockOnlyReachableByFallthrough(const MachineBasicBlock *MBB) const {
#if 0
  // If this is a landing pad, it isn't a fall through.  If it has no preds,
  // then nothing falls through to it.
  if (MBB->isLandingPad() || MBB->pred_empty())
    return false;
  
  // If there isn't exactly one predecessor, it can't be a fall through.
  MachineBasicBlock::const_pred_iterator PI = MBB->pred_begin(), PI2 = PI;
  ++PI2;
  if (PI2 != MBB->pred_end())
    return false;
  
  // The predecessor has to be immediately before this block.
  const MachineBasicBlock *Pred = *PI;
  
  if (!Pred->isLayoutSuccessor(MBB))
    return false;
  
  // Check if the last terminator is an unconditional branch.
  MachineBasicBlock::const_iterator I = Pred->end();
  while (I != Pred->begin() && !(--I)->isTerminator())
    ; // Noop
  return I == Pred->end() || !I->isBarrier();
#endif
  return false;
}

MachineLocation LC3AsmPrinter::
getDebugValueLocation(const MachineInstr *MI) const {
  assert(MI->getNumOperands() == 4 && "Invalid number of operands!");
  assert(MI->getOperand(0).isReg() && MI->getOperand(1).isImm() &&
         "Unexpected MachineOperand types");
  return MachineLocation(MI->getOperand(0).getReg(),
                         MI->getOperand(1).getImm());
}

// Force static initialization.
extern "C" void LLVMInitializeLC3AsmPrinter() { 
  RegisterAsmPrinter<LC3AsmPrinter> X(TheLC3Target);
}
