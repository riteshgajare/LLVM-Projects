#ifndef TARGET_LC3_H
#define TARGET_LC3_H

#include "MCTargetDesc/LC3MCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class FunctionPass;
  class LC3TargetMachine;
  class formatted_raw_ostream;

  FunctionPass *createLC3ISelDag(LC3TargetMachine &TM);
  FunctionPass *createLC3DelaySlotFillerPass(TargetMachine &TM);
  FunctionPass *createLC3FPMoverPass(TargetMachine &TM);

} // end namespace llvm;

namespace llvm {
  // Enums corresponding to LC3 condition codes, both icc's and fcc's.  These
  // values must be kept in sync with the ones in the .td file.
  namespace LC3CC {
    enum CondCodes {
      //ICC_A   =  8   ,  // Always
      //ICC_N   =  0   ,  // Never
      CC_NE  =  5   ,  // Not Equal
      CC_EQ  =  2   ,  // Equal
      CC_LE  =  6   ,  // Less or Equal
      CC_GE  =  3   ,  // Greater or Equal
      CC_POS =  1   ,  // Positive
      CC_NEG =  4     // Negative
    };
  }
  #if 1
  const char *LC3CondCodeToString(LC3CC::CondCodes CC);
#endif
}  // end namespace llvm
#endif
