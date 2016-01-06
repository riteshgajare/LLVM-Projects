//
#include "LC3TargetMachine.h"
#include "LC3.h"
#include "llvm/PassManager.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/IR/DataLayout.h"
using namespace llvm;

extern "C" void LLVMInitializeLC3Target() {
  // Register the target.
  RegisterTargetMachine<LC3TargetMachine> X(TheLC3Target);
}

/// LC3TargetMachine ctor - Create an ILP32 architecture model
///
LC3TargetMachine::LC3TargetMachine(const Target &T, StringRef TT,
                                       StringRef CPU, StringRef FS,
                                       const TargetOptions &Options,
                                       Reloc::Model RM, CodeModel::Model CM,
                                       CodeGenOpt::Level OL)
  : LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL),
    Subtarget(TT,CPU,FS),
    DL("e-p:32:32:32-i8:8:8-i16:16:16-i32:32:32"),
    TLInfo(*this), TSInfo(*this), InstrInfo(),
    FrameLowering() {
  initAsmInfo();
}

namespace {
/// LC3 Code Generator Pass Configuration Options.
class LC3PassConfig : public TargetPassConfig {
public:
  LC3PassConfig(LC3TargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  LC3TargetMachine &getLC3TargetMachine() const {
    return getTM<LC3TargetMachine>();
  }

  virtual bool addInstSelector();
  //TODO: virtual bool addPreEmitPass();
};
} // namespace

TargetPassConfig *LC3TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new LC3PassConfig(this, PM);
}

bool LC3PassConfig::addInstSelector() {
  addPass(createLC3ISelDag(getLC3TargetMachine()));
  return false;
}

/// addPreEmitPass - This pass may be implemented by targets that want to run
/// passes immediately before machine code is emitted.  This should return
/// true if -print-machineinstrs should print out the code after the passes.
//bool LC3PassConfig::addPreEmitPass(){
  //PM->add(createLC3FPMoverPass(getLC3TargetMachine()));
  //PM->add(createLC3DelaySlotFillerPass(getLC3TargetMachine()));
  //return true;
//}

