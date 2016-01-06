//===-- LC3ISelLowering.h - LC3 DAG Lowering Interface ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that LC3 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LC3_ISELLOWERING_H
#define LC3_ISELLOWERING_H

#include "LC3.h"
#include "llvm/Target/TargetLowering.h"

namespace llvm {

  namespace LC3ISD {
    enum {
      FIRST_NUMBER = ISD::BUILTIN_OP_END,
      CALL,
      BRCC,
      RET_FLAG,
      CMPCC,
      SELECT_CC
    };
  }

  class LC3TargetLowering : public TargetLowering {
  public:
    LC3TargetLowering(TargetMachine &TM);
    virtual SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const;

    /// computeMaskedBitsForTargetNode - Determine which of the bits specified
    /// in Mask are known to be either zero or one and return them in the
    /// KnownZero/KnownOne bitsets.
    virtual void computeMaskedBitsForTargetNode(const SDValue Op,
                                                APInt &KnownZero,
                                                APInt &KnownOne,
                                                const SelectionDAG &DAG,
                                                unsigned Depth = 0) const;

    virtual MachineBasicBlock *
    EmitInstrWithCustomInserter(MachineInstr *MI,
				MachineBasicBlock *MBB) const;

    MachineBasicBlock *
    EmitMULInstr(MachineInstr *MI,
				MachineBasicBlock *MBB) const;
    
    virtual const char *getTargetNodeName(unsigned Opcode) const;

    //ConstraintType getConstraintType(const std::string &Constraint) const;
    //std::pair<unsigned, const TargetRegisterClass*>
    //getRegForInlineAsmConstraint(const std::string &Constraint, EVT VT) const;

    virtual bool isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const;

    virtual SDValue
      LowerFormalArguments(SDValue Chain,
                           CallingConv::ID CallConv,
                           bool isVarArg,
                           const SmallVectorImpl<ISD::InputArg> &Ins,
                           SDLoc dl, SelectionDAG &DAG,
                           SmallVectorImpl<SDValue> &InVals) const;

  virtual SDValue
    LowerCall(CallLoweringInfo &/*CLI*/,
              SmallVectorImpl<SDValue> &/*InVals*/) const;

    /*    virtual SDValue
      LowerCall(SDValue Chain, SDValue Callee, CallingConv::ID CallConv,
                bool isVarArg, bool doesNotRet, bool &isTailCall,
                const SmallVectorImpl<ISD::OutputArg> &Outs,
                const SmallVectorImpl<SDValue> &OutVals,
                const SmallVectorImpl<ISD::InputArg> &Ins,
                DebugLoc dl, SelectionDAG &DAG,
                SmallVectorImpl<SDValue> &InVals) const;
    */

    virtual SDValue
      LowerReturn(SDValue Chain,
                  CallingConv::ID CallConv, bool isVarArg,
                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                  const SmallVectorImpl<SDValue> &OutVals,
                  SDLoc dl, SelectionDAG &DAG) const;

    SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerConstantPool(SDValue Op, SelectionDAG &DAG) const;
    SDValue LowerConstant(SDValue Op, SelectionDAG &DAG) const;

    unsigned getSRetArgSize(SelectionDAG &DAG, SDValue Callee) const;
  };
} // end namespace llvm

#endif    // LC3_ISELLOWERING_H
