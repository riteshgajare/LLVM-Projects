include "llvm/Target/Target.td"

//===----------------------------------------------------------------------===//
// Register File, Calling Conv, Instruction Descriptions
//===----------------------------------------------------------------------===//

include "LC3RegisterInfo.td"
include "LC3CallingConv.td"
include "LC3InstrInfo.td"

def LC3InstrInfo : InstrInfo;

//===----------------------------------------------------------------------===//
// LC3 processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name>
 : Processor<Name, NoItineraries, []>;

def : Proc<"generic">;

//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def LC3 : Target {
  // Pull in Instruction Info:
  let InstructionSet = LC3InstrInfo;
}
