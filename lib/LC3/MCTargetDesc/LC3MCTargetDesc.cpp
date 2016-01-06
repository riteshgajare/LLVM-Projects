//===-- LC3MCTargetDesc.cpp - LC3 Target Descriptions -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides LC3 specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "LC3MCTargetDesc.h"
#include "LC3MCAsmInfo.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "LC3GenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "LC3GenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "LC3GenRegisterInfo.inc"

using namespace llvm;

static MCInstrInfo *createLC3MCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitLC3MCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createLC3MCRegisterInfo(StringRef TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitLC3MCRegisterInfo(X, LC3::R7);
  return X;
}

static MCCodeGenInfo *createLC3MCCodeGenInfo(StringRef TT, Reloc::Model RM,
                                               CodeModel::Model CM,
                                               CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  X->InitMCCodeGenInfo(RM, CM, OL);
  return X;
}

static MCAsmInfo *createLC3MCAsmInfo(const MCRegisterInfo &MRI, StringRef TT) {
  MCAsmInfo *MAI = new LC3MCAsmInfo(TT);
  return MAI;
}


extern "C" void LLVMInitializeLC3TargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(TheLC3Target, createLC3MCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheLC3Target,
                                       createLC3MCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheLC3Target, createLC3MCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheLC3Target, createLC3MCRegisterInfo);
}
