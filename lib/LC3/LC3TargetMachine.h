#ifndef LC3TARGETMACHINE_H
#define LC3TARGETMACHINE_H

#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetFrameLowering.h"


#include "LC3FrameLowering.h"
#include "LC3InstrInfo.h"
#include "LC3SelectionDAGInfo.h"
#include "LC3ISelLowering.h"
#include "LC3Subtarget.h"

namespace llvm {

class Module;

class LC3TargetMachine : public LLVMTargetMachine {
  LC3Subtarget Subtarget;
  const DataLayout DL;       // Calculates type size & alignment
  LC3TargetLowering TLInfo;
  LC3SelectionDAGInfo TSInfo;
  LC3InstrInfo InstrInfo;
  LC3FrameLowering FrameLowering;
public:
  LC3TargetMachine(const Target &T, StringRef TT,
                     StringRef CPU, StringRef FS, const TargetOptions &Options,
                     Reloc::Model RM, CodeModel::Model CM,
                     CodeGenOpt::Level OL);

  virtual const LC3InstrInfo *getInstrInfo() const { return &InstrInfo; }
  virtual const TargetFrameLowering  *getFrameLowering() const {
    return &FrameLowering;
  }
  virtual const LC3RegisterInfo *getRegisterInfo() const {
    return &InstrInfo.getRegisterInfo();
  }
  virtual const LC3TargetLowering* getTargetLowering() const {
    return &TLInfo;
  }
  virtual const LC3SelectionDAGInfo* getSelectionDAGInfo() const {
    return &TSInfo;
  }
  virtual const DataLayout       *getDataLayout() const { return &DL; }

  virtual const LC3Subtarget *getSubtargetImpl() const { return &Subtarget; }

  // Pass Pipeline Configuration
  virtual TargetPassConfig *createPassConfig(PassManagerBase &PM);

};

} // end namespace llvm


#endif
