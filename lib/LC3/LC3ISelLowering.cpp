//===-- LC3ISelLowering.cpp - LC3 DAG Lowering Implementation ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the interfaces that LC3 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#include "LC3ISelLowering.h"
#include "LC3TargetMachine.h"
#include "LC3MachineFunctionInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/Support/ErrorHandling.h"
using namespace llvm;


//===----------------------------------------------------------------------===//
// Calling Convention Implementation
//===----------------------------------------------------------------------===//

static bool CC_LC3_Assign_SRet(unsigned &ValNo, MVT &ValVT,
                                 MVT &LocVT, CCValAssign::LocInfo &LocInfo,
                                 ISD::ArgFlagsTy &ArgFlags, CCState &State)
{
  assert (ArgFlags.isSRet());

  //Assign SRet argument
  State.addLoc(CCValAssign::getCustomMem(ValNo, ValVT,
                                         0,
                                         LocVT, LocInfo));
  return true;
}


#include "LC3GenCallingConv.inc"

MachineBasicBlock *
LC3TargetLowering::EmitMULInstr(MachineInstr *MI,
				MachineBasicBlock *BB) const
{
  //assert(0);
  const TargetInstrInfo &TII = *getTargetMachine().getInstrInfo();
  DebugLoc dl = MI->getDebugLoc();

  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator It = BB;
  ++It;


  MachineFunction *F = BB->getParent();
  MachineRegisterInfo &R = F->getRegInfo();

  MachineBasicBlock *loop = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *test = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *twos = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *finish = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, finish);  
  F->insert(It, loop);
  F->insert(It, twos);
  F->insert(It, test);

    // Transfer the remainder of BB and its successor edges to sinkMBB.
  finish->splice(finish->begin(), BB,
                  llvm::next(MachineBasicBlock::iterator(MI)),
                  BB->end());
  finish->transferSuccessorsAndUpdatePHIs(BB);

  // Add the true and fallthrough blocks as its successors.
  BB->addSuccessor(test);
  BB->addSuccessor(finish);

  test->addSuccessor(loop);
  test->addSuccessor(twos);

  twos->addSuccessor(loop);

  // Next, add the finish block as a successor of the loop block
  loop->addSuccessor(finish);
  loop->addSuccessor(loop);

  unsigned ZERO = R.createVirtualRegister(&LC3::GPRegsRegClass);
  BuildMI(BB, dl, TII.get(LC3::LI), ZERO)
    .addImm(0);

  
  unsigned ZAMT = R.createVirtualRegister(&LC3::GPRegsRegClass);
  BuildMI(BB, dl, TII.get(LC3::ADDri), ZAMT)
    .addReg(MI->getOperand(2).getReg()).addImm(0);

  // If multiply by zero, we are done
  BuildMI(BB, dl, TII.get(LC3::BRCOND)).addMBB(finish).addImm(LC3CC::CC_EQ);

  // --- 
  unsigned PAMT = R.createVirtualRegister(&LC3::GPRegsRegClass);
  BuildMI(BB, dl, TII.get(LC3::ADDri), PAMT)
    .addReg(MI->getOperand(2).getReg()).addImm(0);

  BuildMI(test, dl, TII.get(LC3::BRCOND)).addMBB(loop).addImm(LC3CC::CC_POS);

  // ---
  // Build two's complement if it's negative
  unsigned NAMT1 = R.createVirtualRegister(&LC3::GPRegsRegClass);
  unsigned NAMT = R.createVirtualRegister(&LC3::GPRegsRegClass);
  
  BuildMI(twos, dl, TII.get(LC3::NOT), NAMT1).addReg(PAMT);
  BuildMI(twos, dl, TII.get(LC3::ADDri), NAMT).addReg(NAMT1).addImm(1);

  // Build actual loop

  unsigned INDEX0 = R.createVirtualRegister(&LC3::GPRegsRegClass);
  unsigned INDEX1 = R.createVirtualRegister(&LC3::GPRegsRegClass);

  unsigned RES0 = R.createVirtualRegister(&LC3::GPRegsRegClass);
  unsigned RES1 = R.createVirtualRegister(&LC3::GPRegsRegClass);

  BuildMI(loop, dl, TII.get(LC3::PHI), INDEX0)
    .addReg(NAMT).addMBB(twos)
    .addReg(PAMT).addMBB(test)
    .addReg(INDEX1).addMBB(loop);

  BuildMI(loop, dl, TII.get(LC3::PHI), RES0)
    .addReg(ZERO).addMBB(twos)
    .addReg(ZERO).addMBB(test)
    .addReg(RES1).addMBB(loop);
  
  BuildMI(loop, dl, TII.get(LC3::ADDrr), RES1)
    .addReg(RES0).addReg(MI->getOperand(1).getReg());

  BuildMI(loop, dl, TII.get(LC3::ADDri), INDEX1)
    .addReg(INDEX0).addImm(-1);

  BuildMI(loop, dl, TII.get(LC3::BRCOND)).addMBB(loop).addImm(LC3CC::CC_POS);  

  //
  BuildMI(*finish, finish->begin(), dl,
          TII.get(LC3::PHI), MI->getOperand(0).getReg())
    .addReg(ZERO).addMBB(BB)
    .addReg(RES1).addMBB(loop);

  MI->eraseFromParent();
  return finish;
}

MachineBasicBlock *
LC3TargetLowering::EmitInstrWithCustomInserter(MachineInstr *MI,
				MachineBasicBlock *BB) const
{
  switch(MI->getOpcode()){
  default: assert(0);
    break;
  case LC3::LC3MUL: 
    return EmitMULInstr(MI,BB);
  case LC3::BRCOND:
    break;
  }
  
  const TargetInstrInfo &TII = *getTargetMachine().getInstrInfo();
  unsigned BROpcode = LC3::BRCOND;
  unsigned CC;
  DebugLoc dl = MI->getDebugLoc();
  // Figure out the conditional branch opcode to use for this select_cc.

  CC = (LC3CC::CondCodes)MI->getOperand(3).getImm();

  // To "insert" a SELECT_CC instruction, we actually have to insert the diamond
  // control-flow pattern.  The incoming instruction knows the destination vreg
  // to set, the condition code register to branch on, the true/false values to
  // select between, and a branch opcode to use.
  const BasicBlock *LLVM_BB = BB->getBasicBlock();
  MachineFunction::iterator It = BB;
  ++It;

  //  thisMBB:
  //  ...
  //   TrueVal = ...
  //   [f]bCC copy1MBB
  //   fallthrough --> copy0MBB
  MachineBasicBlock *thisMBB = BB;
  MachineFunction *F = BB->getParent();
  MachineBasicBlock *copy0MBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *sinkMBB = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, sinkMBB);  
  F->insert(It, copy0MBB);
  
  // Transfer the remainder of BB and its successor edges to sinkMBB.
  sinkMBB->splice(sinkMBB->begin(), BB,
                  llvm::next(MachineBasicBlock::iterator(MI)),
                  BB->end());
  sinkMBB->transferSuccessorsAndUpdatePHIs(BB);
  sinkMBB->setIsLandingPad(true);

  // Add the true and fallthrough blocks as its successors.
  BB->addSuccessor(copy0MBB);
  BB->addSuccessor(sinkMBB);

  BuildMI(BB, dl, TII.get(BROpcode)).addMBB(sinkMBB).addImm(CC);

  //  copy0MBB:
  //   %FalseValue = ...
  //   # fallthrough to sinkMBB
  BB = copy0MBB;

  // Update machine-CFG edges
  BB->addSuccessor(sinkMBB);

  //  sinkMBB:
  //   %Result = phi [ %FalseValue, copy0MBB ], [ %TrueValue, thisMBB ]
  //  ...
  BB = sinkMBB;
  BuildMI(*BB, BB->begin(), dl, TII.get(LC3::PHI), MI->getOperand(0).getReg())
    .addReg(MI->getOperand(2).getReg()).addMBB(copy0MBB)
    .addReg(MI->getOperand(1).getReg()).addMBB(thisMBB);

  MI->eraseFromParent();   // The pseudo instruction is gone now.
  return BB;

}


SDValue
LC3TargetLowering::LowerReturn(SDValue Chain,
                                 CallingConv::ID CallConv, bool isVarArg,
                                 const SmallVectorImpl<ISD::OutputArg> &Outs,
                                 const SmallVectorImpl<SDValue> &OutVals,
                                 SDLoc dl, SelectionDAG &DAG) const {

  MachineFunction &MF = DAG.getMachineFunction();

  // CCValAssign - represent the assignment of the return value to locations.
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), RVLocs, *DAG.getContext());

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_LC3_32);

  // If this is the first return lowered for this function, add the regs to the
  // liveout set for the function.

  // Note: Leaving this here so that 
  if (MF.getRegInfo().livein_empty()) {
    for (unsigned i = 0; i != RVLocs.size(); ++i)
      if (RVLocs[i].isRegLoc())
        MF.getRegInfo().addLiveIn(RVLocs[i].getLocReg());
  }

  SDValue Flag;

  unsigned FramePtrOffset = 12;

  assert(RVLocs.size()<=1);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    if (RVLocs[i].isRegLoc())
      {
	CCValAssign &VA = RVLocs[i];
	assert(VA.isRegLoc() && "Can only return in registers!");
	
	Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(),
				 OutVals[i], Flag);
	
	// Guarantee that all emitted copies are stuck together with flags.
	Flag = Chain.getValue(1);
      }
    else
      {
	CCValAssign &VA = RVLocs[i];

	SDValue StackPtr = DAG.getRegister(LC3::R5, MVT::i32);
	SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset()+FramePtrOffset);
	PtrOff = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr, PtrOff);

	SDValue Store = DAG.getStore(Chain,dl,OutVals[i],PtrOff,MachinePointerInfo(),false,false,0);
	Chain = Store;
      }
  }

  // If the function returns a struct, nothing to do, but make sure it was set up
  // properly.
  if (MF.getFunction()->hasStructRetAttr()) {
    LC3MachineFunctionInfo *SFI = MF.getInfo<LC3MachineFunctionInfo>();
    unsigned Reg = SFI->getSRetReturnReg();
    if (!Reg)
      llvm_unreachable("sret virtual register not created in the entry block");    
  }

  if (Flag.getNode())
    return DAG.getNode(LC3ISD::RET_FLAG, dl, MVT::Other, Chain,
                       Flag);

  return DAG.getNode(LC3ISD::RET_FLAG, dl, MVT::Other, Chain);
}

/// LowerFormalArguments - in LC-3, all arguments are passed on the stack
SDValue
LC3TargetLowering::LowerFormalArguments(SDValue Chain,
                                          CallingConv::ID CallConv, bool isVarArg,
                                          const SmallVectorImpl<ISD::InputArg>
                                            &Ins,
                                          SDLoc dl, SelectionDAG &DAG,
                                          SmallVectorImpl<SDValue> &InVals)
                                            const {

  MachineFunction &MF = DAG.getMachineFunction();
  //MachineRegisterInfo &RegInfo = MF.getRegInfo();
  //LC3MachineFunctionInfo *FuncInfo = MF.getInfo<LC3MachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 getTargetMachine(), ArgLocs, *DAG.getContext());

  // Figure out how all the arguments map into the LC3 stack space
  CCInfo.AnalyzeFormalArguments(Ins, CC_LC3_32);

  unsigned FramePtrOffset = 16;

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    assert(VA.isMemLoc());

    unsigned Offset = VA.getLocMemOffset();

    int FI = MF.getFrameInfo()->CreateFixedObject(4,
                                                  Offset+FramePtrOffset,
                                                  true);

    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy());
    SDValue Load = DAG.getLoad(VA.getValVT(), dl, Chain, FIPtr,
		       MachinePointerInfo(),
		       false, false, false, 0);
    InVals.push_back(Load);
  }

  if (MF.getFunction()->hasStructRetAttr()) {
    //Copy the SRet Argument to SRetReturnReg
    LC3MachineFunctionInfo *SFI = MF.getInfo<LC3MachineFunctionInfo>();
    unsigned Reg = SFI->getSRetReturnReg();
    if (!Reg) {
      Reg = MF.getRegInfo().createVirtualRegister(&LC3::GPRegsRegClass);
      SFI->setSRetReturnReg(Reg);
    }
    // If this function as an SRet, it must be the first argument of the function
    // that goes through memory. So, copy that loaded value from InVals into the 
    // register used in rest of function for the return value. We will use this register
    // later for lowering the return call.  
    // This is done just to simplify finding the name of the register with the pointer.
    // Later, register allocation will map the virtual register into a physical reg in whatever
    // way is best.
    SDValue Copy = DAG.getCopyToReg(DAG.getEntryNode(), dl, Reg, InVals[0]);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, Copy, Chain);
  }

  return Chain;
}

SDValue
LC3TargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                              SmallVectorImpl<SDValue> &InVals) const {

  SelectionDAG &DAG                     = CLI.DAG;
  SDLoc DL                              = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals     = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  bool &isTailCall                      = CLI.IsTailCall;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool isVarArg                         = CLI.IsVarArg;

  // LC3 target does not yet support tail call optimization.
  isTailCall = false;

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_LC3_32);

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState RVInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 DAG.getTarget(), RVLocs, *DAG.getContext());

  RVInfo.AnalyzeCallResult(Ins, RetCC_LC3_32);


  // Get the size of the outgoing arguments stack space requirement.
  unsigned ArgsSize = CCInfo.getNextStackOffset();
  unsigned RetValSize = RVInfo.getNextStackOffset();

  unsigned TotalSize = ArgsSize + RetValSize; 

  // Keep stack frames 4-byte aligned.
  TotalSize = (TotalSize+3) & ~3;

  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();

  //Byval arguments are "pass by value", so make a copy in the local function
  //before passing as a formal argument.  
  //
  //Create local copies for byval args.
  //
  SmallVector<SDValue, 8> ByValArgs;
  for (unsigned i = 0,  e = Outs.size(); i != e; ++i) {
    ISD::ArgFlagsTy Flags = Outs[i].Flags;
    if (!Flags.isByVal())
      continue;

    SDValue Arg = OutVals[i];
    unsigned Size = Flags.getByValSize();
    unsigned Align = Flags.getByValAlign();

    int FI = MFI->CreateStackObject(Size, Align, false);
    SDValue FIPtr = DAG.getFrameIndex(FI, getPointerTy());
    SDValue SizeNode = DAG.getConstant(Size, MVT::i32);

    Chain = DAG.getMemcpy(Chain, DL, FIPtr, Arg, SizeNode, Align,
                          false,        //isVolatile,
                          (Size <= 32), //AlwaysInline if size <= 32
                          MachinePointerInfo(), MachinePointerInfo());
    ByValArgs.push_back(FIPtr);
  }

  //
  // Adjust the stack pointer to make room for arguments on the stack!
  //
  Chain = DAG.getCALLSEQ_START(Chain, DAG.getIntPtrConstant(TotalSize, true), DL);

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  //  const unsigned StackOffset = 0;
  bool hasStructRetAttr = false;

  // This loop does the heavy lifting of copying the formal arguments into the
  // stack for use the by callee.
  
  // Walk the register/memloc assignments, inserting copies/loads.

  for (unsigned i = 0, realArgIdx = 0, byvalArgIdx = 0, e = ArgLocs.size();
       i != e;
       ++i, ++realArgIdx) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[realArgIdx];

    ISD::ArgFlagsTy Flags = Outs[realArgIdx].Flags;

    //Use local copy if it is a byval arg.
    if (Flags.isByVal())
      Arg = ByValArgs[byvalArgIdx++];

    // Per LLVM IR, arguments may have special attributes.  If so, perform necessary
    // promotion before putting on stack or in register

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, DL, VA.getLocVT(), Arg);
      break;
    case CCValAssign::BCvt:
      Arg = DAG.getNode(ISD::BITCAST, DL, VA.getLocVT(), Arg);
      break;
    }


    if (Flags.isSRet()) {
      assert(VA.needsCustom());
      // store SRet argument in %sp-8
      SDValue StackPtr = DAG.getRegister(LC3::R6, MVT::i32);
      SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset()+RetValSize);
      PtrOff = DAG.getNode(ISD::ADD, DL, MVT::i32, StackPtr, PtrOff);
      MemOpChains.push_back(DAG.getStore(Chain, DL, Arg, PtrOff,
                                         MachinePointerInfo(),
                                         false, false, 0));
      hasStructRetAttr = true;
      continue;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      if (VA.getLocVT() != MVT::f32) {
        RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
        continue;
      }
      Arg = DAG.getNode(ISD::BITCAST, DL, MVT::i32, Arg);
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
      continue;
    }

    assert(VA.isMemLoc());

    // Create a store off the stack pointer for this argument.
    SDValue StackPtr = DAG.getRegister(LC3::R6, MVT::i32);
    SDValue PtrOff = DAG.getIntPtrConstant(VA.getLocMemOffset()+RetValSize);
    PtrOff = DAG.getNode(ISD::ADD, DL, MVT::i32, StackPtr, PtrOff);
    MemOpChains.push_back(DAG.getStore(Chain, DL, Arg, PtrOff,
                                       MachinePointerInfo(),
                                       false, false, 0));
  }

  // Emit all stores, make sure the occur before any copies into physregs.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, DL, MVT::Other,
                        &MemOpChains[0], MemOpChains.size());

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    unsigned Reg = RegsToPass[i].first;
    Chain = DAG.getCopyToReg(Chain, DL, Reg, RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  unsigned SRetArgSize = (hasStructRetAttr)? getSRetArgSize(DAG, Callee):0;

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), DL, MVT::i32);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);

  // Returns a chain & a flag for retval copy to use
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);
  if (hasStructRetAttr)
    Ops.push_back(DAG.getTargetConstant(SRetArgSize, MVT::i32));
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    unsigned Reg = RegsToPass[i].first;
    Ops.push_back(DAG.getRegister(Reg, RegsToPass[i].second.getValueType()));
  }
  if (InFlag.getNode())
    Ops.push_back(InFlag);

  Chain = DAG.getNode(LC3ISD::CALL, DL, NodeTys, &Ops[0], Ops.size());
  InFlag = Chain.getValue(1);



  // Return value is left on the stack by the callee.  So, we need to get the
  // return value off the stack before call seq


  // Copy all of the result registers out of their specified physreg or off of the
  // stack
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    if (RVLocs[i].isRegLoc())
      {
	unsigned Reg = RVLocs[i].getLocReg();
	Chain = DAG.getCopyFromReg(Chain, DL, Reg,
				   RVLocs[i].getValVT(), InFlag).getValue(1);
	InFlag = Chain.getValue(2);
	InVals.push_back(Chain.getValue(0));	
      }
    else
      {
	//printf("memOffset = %d\n",RVLocs[i].getLocMemOffset());

	SDValue StackPtr = DAG.getRegister(LC3::R6, MVT::i32);
	SDValue PtrOff = DAG.getIntPtrConstant(RVLocs[i].getLocMemOffset());
	PtrOff = DAG.getNode(ISD::ADD, DL, MVT::i32, StackPtr, PtrOff);
	SDValue Load = DAG.getLoad(getPointerTy(), DL, Chain , PtrOff,
			    MachinePointerInfo(),
			    false, false, false,0);
	InVals.push_back(Load);
	Chain = Load.getValue(1);
	InFlag = Chain.getValue(1);
      }
  }


  // Adjust final stack position to clean up argument and return value passing
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(ArgsSize, true),
                             DAG.getIntPtrConstant(0, true), Chain.getValue(1), DL);

  return Chain;
}

unsigned
LC3TargetLowering::getSRetArgSize(SelectionDAG &DAG, SDValue Callee) const
{
  const Function *CalleeFn = 0;
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee)) {
    CalleeFn = dyn_cast<Function>(G->getGlobal());
  } else if (ExternalSymbolSDNode *E =
             dyn_cast<ExternalSymbolSDNode>(Callee)) {
    const Function *Fn = DAG.getMachineFunction().getFunction();
    const Module *M = Fn->getParent();
    CalleeFn = M->getFunction(E->getSymbol());
  }

  if (!CalleeFn)
    return 0;

  assert(CalleeFn->hasStructRetAttr() &&
         "Callee does not have the StructRet attribute.");

  PointerType *Ty = cast<PointerType>(CalleeFn->arg_begin()->getType());
  Type *ElementTy = Ty->getElementType();
  return getDataLayout()->getTypeAllocSize(ElementTy);
}

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

static LC3CC::CondCodes IntCondCCodeToNZP(ISD::CondCode CC)
{
  switch (CC) {
  default: llvm_unreachable("Unknown integer condition code!");
  case ISD::SETEQ:  return LC3CC::CC_EQ;
  case ISD::SETNE:  return LC3CC::CC_NE;
  case ISD::SETLT:  return LC3CC::CC_NEG;
  case ISD::SETGT:  return LC3CC::CC_POS;
  case ISD::SETLE:  return LC3CC::CC_LE;
  case ISD::SETGE:  return LC3CC::CC_GE;
  }
}


LC3TargetLowering::LC3TargetLowering(TargetMachine &TM)
  : TargetLowering(TM, new TargetLoweringObjectFileELF()) {

  // Only i32 arithmetic supported
  addRegisterClass(MVT::i32, &LC3::GPRegsRegClass);
  
  // Promote i1,8,16 SEXTLOAD to i32
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);

  //setLoadExtAction(ISD::ZEXTLOAD, MVT::i1, Promote);
  //setLoadExtAction(ISD::ZEXTLOAD, MVT::i8, Promote);
  //setLoadExtAction(ISD::ZEXTLOAD, MVT::i16, Promote);
  //setLoadExtAction(ISD::SEXTLOAD, MVT::i8, Promote);
  //setLoadExtAction(ISD::SEXTLOAD, MVT::i16, Promote);
  
  // Custom legalize GlobalAddress nodes.
  setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
  setOperationAction(ISD::ConstantPool , MVT::i32, Custom);
  setOperationAction(ISD::Constant, MVT::i32, Custom);

  setOperationAction(ISD::BRCOND, MVT::Other, Expand);
  setOperationAction(ISD::BRIND, MVT::Other, Expand);
  setOperationAction(ISD::BR_JT, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC, MVT::i32, Custom);
  //setOperationAction(ISD::BR_CC, MVT::f32, Custom);
  //setOperationAction(ISD::BR_CC, MVT::f64, Custom);

 // LC3 has no select or setcc: expand to SELECT_CC.
  setOperationAction(ISD::SELECT, MVT::i32, Expand);
  setOperationAction(ISD::SELECT, MVT::f32, Expand);
  setOperationAction(ISD::SELECT, MVT::f64, Expand);
  setOperationAction(ISD::SETCC, MVT::i32, Expand);
  setOperationAction(ISD::SETCC, MVT::f32, Expand);
  setOperationAction(ISD::SETCC, MVT::f64, Expand);

  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f32, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::f64, Custom);

#if 0
  // Set up the register classes.

  // Turn FP extload into load/fextend
  setLoadExtAction(ISD::EXTLOAD, MVT::f32, Expand);
  // LC3 doesn't have i1 sign extending load
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);
  // Turn FP truncstore into trunc + store.
  setTruncStoreAction(MVT::f64, MVT::f32, Expand);


  // LC3 doesn't have sext_inreg, replace them with shl/sra
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8 , Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i1 , Expand);

  // LC3 has no REM or DIVREM operations.
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  // Custom expand fp<->sint
  setOperationAction(ISD::FP_TO_SINT, MVT::i32, Custom);
  setOperationAction(ISD::SINT_TO_FP, MVT::i32, Custom);

  // Expand fp<->uint
  setOperationAction(ISD::FP_TO_UINT, MVT::i32, Expand);
  setOperationAction(ISD::UINT_TO_FP, MVT::i32, Expand);

  setOperationAction(ISD::BITCAST, MVT::f32, Expand);
  setOperationAction(ISD::BITCAST, MVT::i32, Expand);


  // FIXME: There are instructions available for ATOMIC_FENCE
  // on LC3V8 and later.
  setOperationAction(ISD::MEMBARRIER, MVT::Other, Expand);
  setOperationAction(ISD::ATOMIC_FENCE, MVT::Other, Expand);

  setOperationAction(ISD::FSIN , MVT::f64, Expand);
  setOperationAction(ISD::FCOS , MVT::f64, Expand);
  setOperationAction(ISD::FREM , MVT::f64, Expand);
  setOperationAction(ISD::FMA  , MVT::f64, Expand);
  setOperationAction(ISD::FSIN , MVT::f32, Expand);
  setOperationAction(ISD::FCOS , MVT::f32, Expand);
  setOperationAction(ISD::FREM , MVT::f32, Expand);
  setOperationAction(ISD::FMA  , MVT::f32, Expand);
  setOperationAction(ISD::CTPOP, MVT::i32, Expand);
  setOperationAction(ISD::CTTZ , MVT::i32, Expand);
  setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTLZ , MVT::i32, Expand);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::ROTL , MVT::i32, Expand);
  setOperationAction(ISD::ROTR , MVT::i32, Expand);
  setOperationAction(ISD::BSWAP, MVT::i32, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f64, Expand);
  setOperationAction(ISD::FCOPYSIGN, MVT::f32, Expand);
  setOperationAction(ISD::FPOW , MVT::f64, Expand);
  setOperationAction(ISD::FPOW , MVT::f32, Expand);

  setOperationAction(ISD::SHL_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32, Expand);

  // FIXME: LC3 provides these multiplies, but we don't have them yet.
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);

  setOperationAction(ISD::EH_LABEL, MVT::Other, Expand);

  // VASTART needs to be custom lowered to use the VarArgsFrameIndex.
  setOperationAction(ISD::VASTART           , MVT::Other, Custom);
  // VAARG needs to be lowered to not do unaligned accesses for doubles.
  setOperationAction(ISD::VAARG             , MVT::Other, Custom);

  // Use the default implementation.
  setOperationAction(ISD::VACOPY            , MVT::Other, Expand);
  setOperationAction(ISD::VAEND             , MVT::Other, Expand);
  setOperationAction(ISD::STACKSAVE         , MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE      , MVT::Other, Expand);
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32  , Custom);

  // No debug info support yet.
  setOperationAction(ISD::EH_LABEL, MVT::Other, Expand);


  if (TM.getSubtarget<LC3Subtarget>().isV9())
    setOperationAction(ISD::CTPOP, MVT::i32, Legal);

#endif
  setStackPointerRegisterToSaveRestore(LC3::R6);

  setMinFunctionAlignment(2);

  computeRegisterProperties();
}

const char *LC3TargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default: return "LC3ISD:Unknown";
  case LC3ISD::CALL:      return "LC3ISD::CALL";
  case LC3ISD::SELECT_CC: return "LC3ISD::SELECT_CC";
  case LC3ISD::CMPCC:     return "LC3ISD::CMPCC";
  case LC3ISD::BRCC:      return "LC3ISD::BRCC";
  }
  return 0;
}

/// isMaskedValueZeroForTargetNode - Return true if 'Op & Mask' is known to
/// be zero. Op is expected to be a target specific node. Used by DAG
/// combiner.
void LC3TargetLowering::computeMaskedBitsForTargetNode(const SDValue Op,
                                                         APInt &KnownZero,
                                                         APInt &KnownOne,
                                                         const SelectionDAG &DAG,
                                                         unsigned Depth) const {
#if 0
  APInt KnownZero2, KnownOne2;
  KnownZero = KnownOne = APInt(KnownZero.getBitWidth(), 0);

  switch (Op.getOpcode()) {
  default: break;
  case SPISD::SELECT_ICC:
  case SPISD::SELECT_FCC:
    DAG.ComputeMaskedBits(Op.getOperand(1), KnownZero, KnownOne, Depth+1);
    DAG.ComputeMaskedBits(Op.getOperand(0), KnownZero2, KnownOne2, Depth+1);
    assert((KnownZero & KnownOne) == 0 && "Bits known to be one AND zero?");
    assert((KnownZero2 & KnownOne2) == 0 && "Bits known to be one AND zero?");

    // Only known if known in both the LHS and RHS.
    KnownOne &= KnownOne2;
    KnownZero &= KnownZero2;
    break;
  }
#endif
}

// Look at LHS/RHS/CC and see if they are a lowered setcc instruction.  If so
// set LHS/RHS and SPCC to the LHS/RHS of the setcc and SPCC to the condition.
static void LookThroughSetCC(SDValue &LHS, SDValue &RHS,
                             ISD::CondCode CC, unsigned &SPCC) {
  if (isa<ConstantSDNode>(RHS) &&
      cast<ConstantSDNode>(RHS)->isNullValue() &&
      CC == ISD::SETNE &&
      ((LHS.getOpcode() == LC3ISD::SELECT_CC &&
        LHS.getOperand(3).getOpcode() == LC3ISD::CMPCC) /*||
       (LHS.getOpcode() == LC3ISD::SELECT_FCC &&
       LHS.getOperand(3).getOpcode() == LC3ISD::CMPFCC)*/) &&
      isa<ConstantSDNode>(LHS.getOperand(0)) &&
      isa<ConstantSDNode>(LHS.getOperand(1)) &&
      cast<ConstantSDNode>(LHS.getOperand(0))->isOne() &&
      cast<ConstantSDNode>(LHS.getOperand(1))->isNullValue()) {
    SDValue CMPCC = LHS.getOperand(3);
    SPCC = cast<ConstantSDNode>(LHS.getOperand(2))->getZExtValue();
    LHS = CMPCC.getOperand(0);
    RHS = CMPCC.getOperand(1);
  }
}

SDValue LC3TargetLowering::LowerGlobalAddress(SDValue Op,
                                                SelectionDAG &DAG) const {

  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  // FIXME there isn't really any debug info here
  SDLoc dl(Op);

  SDValue GA = DAG.getTargetGlobalAddress(GV, dl, getPointerTy(), cast<GlobalAddressSDNode>(Op)->getOffset());

  return DAG.getNode(ISD::ADD, dl, MVT::i32, DAG.getRegister(LC3::R4,MVT::i32), GA);
}

SDValue LC3TargetLowering::LowerConstantPool(SDValue Op,
                                               SelectionDAG &DAG) const {
  ConstantPoolSDNode *N = cast<ConstantPoolSDNode>(Op);
  // FIXME there isn't really any debug info here
  SDLoc dl(Op);
  const Constant *C = N->getConstVal();
  SDValue CP = DAG.getTargetConstantPool(C, MVT::i32, N->getAlignment());
  
  return CP;

  SDValue AbsAddr = DAG.getNode(ISD::ADD, dl, MVT::i32, DAG.getRegister(LC3::R4,MVT::i32), CP);
  return DAG.getLoad(getPointerTy(), dl, DAG.getEntryNode(),
                     AbsAddr, MachinePointerInfo(), false, false, false, 0);
}

SDValue LC3TargetLowering::LowerConstant(SDValue Op,
                                               SelectionDAG &DAG) const {
  ConstantSDNode *N = cast<ConstantSDNode>(Op);
  // FIXME there isn't really any debug info here
  SDLoc dl(Op);

  const Constant *C = N->getConstantIntValue();

  SDValue CP = DAG.getTargetConstantPool(C, MVT::i32, 32);
  
  return DAG.getLoad(getPointerTy(), dl, DAG.getEntryNode(),
                     CP, MachinePointerInfo(), false, false, false, 0);
}


#if 0
static SDValue LowerFP_TO_SINT(SDValue Op, SelectionDAG &DAG) {
  DebugLoc dl = Op.getDebugLoc();
  // Convert the fp value to integer in an FP register.
  assert(Op.getValueType() == MVT::i32);
  Op = DAG.getNode(SPISD::FTOI, dl, MVT::f32, Op.getOperand(0));
  return DAG.getNode(ISD::BITCAST, dl, MVT::i32, Op);
}

static SDValue LowerSINT_TO_FP(SDValue Op, SelectionDAG &DAG) {
  DebugLoc dl = Op.getDebugLoc();
  assert(Op.getOperand(0).getValueType() == MVT::i32);
  SDValue Tmp = DAG.getNode(ISD::BITCAST, dl, MVT::f32, Op.getOperand(0));
  // Convert the int value to FP in an FP register.
  return DAG.getNode(SPISD::ITOF, dl, Op.getValueType(), Tmp);
}
#endif

static SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) {
  SDValue Chain = Op.getOperand(0);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
  SDValue LHS = Op.getOperand(2);
  SDValue RHS = Op.getOperand(3);
  SDValue Dest = Op.getOperand(4);
  SDLoc dl(Op);
  unsigned Opc, LC3CC = ~0U;

  // If this is a br_cc of a "setcc", and if the setcc got lowered into
  // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
  LookThroughSetCC(LHS, RHS, CC, LC3CC);

  // Get the condition flag.
  SDValue CompareFlag;
  if (LHS.getValueType() == MVT::i32) {
    std::vector<EVT> VTs;
    VTs.push_back(MVT::i32);
    VTs.push_back(MVT::Glue);
    SDValue Ops[2] = { LHS, RHS };
    CompareFlag = DAG.getNode(LC3ISD::CMPCC, dl, VTs, Ops, 2).getValue(1);
    if (LC3CC == ~0U) LC3CC = IntCondCCodeToNZP(CC);
    Opc = LC3ISD::BRCC;
  } else {
    llvm_unreachable("no support for MVT:f32 or f64 in LowerBR_CC");
    //CompareFlag = DAG.getNode(LC3ISD::CMPFCC, dl, MVT::Glue, LHS, RHS);
    //if (LC3CC == ~0U) LC3CC = FPCondCCodeToFCC(CC);
    //Opc = LC3ISD::BRFCC;
  }
  return DAG.getNode(Opc, dl, MVT::Other, Chain, Dest,
                     DAG.getConstant(LC3CC, MVT::i32), CompareFlag);
}



static SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();
  SDValue TrueVal = Op.getOperand(2);
  SDValue FalseVal = Op.getOperand(3);
  SDLoc dl(Op);
  unsigned Opc, LC3CC = ~0U;

  // If this is a select_cc of a "setcc", and if the setcc got lowered into
  // an CMP[IF]CC/SELECT_[IF]CC pair, find the original compared values.
  LookThroughSetCC(LHS, RHS, CC, LC3CC);

  SDValue CompareFlag;
  if (LHS.getValueType() == MVT::i32) {
    std::vector<EVT> VTs;
    VTs.push_back(LHS.getValueType());   // subcc returns a value
    VTs.push_back(MVT::Glue);
    SDValue Ops[2] = { LHS, RHS };
    CompareFlag = DAG.getNode(LC3ISD::CMPCC, dl, VTs, Ops, 2).getValue(1);
    Opc = LC3ISD::SELECT_CC;
    if (LC3CC == ~0U) LC3CC = IntCondCCodeToNZP(CC);
  } else {
    llvm_unreachable("can't support MVT::f32 or f64");
    //CompareFlag = DAG.getNode(SPISD::CMPFCC, dl, MVT::Glue, LHS, RHS);
    //Opc = SPISD::SELECT_FCC;
    //if (SPCC == ~0U) SPCC = FPCondCCodeToFCC(CC);
  }
  return DAG.getNode(Opc, dl, TrueVal.getValueType(), TrueVal, FalseVal,
                     DAG.getConstant(LC3CC, MVT::i32), CompareFlag);
}

#if 0
static SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) {
  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
  MFI->setFrameAddressIsTaken(true);

  EVT VT = Op.getValueType();
  DebugLoc dl = Op.getDebugLoc();
  unsigned FrameReg = LC3::R5;

  uint64_t depth = Op.getConstantOperandVal(0);

  SDValue FrameAddr;
  FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), dl, FrameReg, VT);

  assert(depth==0);

  return FrameAddr;
}
#endif

#if 0
static SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG,
                            const LC3TargetLowering &TLI) {
  MachineFunction &MF = DAG.getMachineFunction();
  LC3MachineFunctionInfo *FuncInfo = MF.getInfo<LC3MachineFunctionInfo>();

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  DebugLoc dl = Op.getDebugLoc();
  SDValue Offset =
    DAG.getNode(ISD::ADD, dl, MVT::i32,
                DAG.getRegister(SP::I6, MVT::i32),
                DAG.getConstant(FuncInfo->getVarArgsFrameOffset(),
                                MVT::i32));
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), dl, Offset, Op.getOperand(1),
                      MachinePointerInfo(SV), false, false, 0);
}

static SDValue LowerVAARG(SDValue Op, SelectionDAG &DAG) {
  SDNode *Node = Op.getNode();
  EVT VT = Node->getValueType(0);
  SDValue InChain = Node->getOperand(0);
  SDValue VAListPtr = Node->getOperand(1);
  const Value *SV = cast<SrcValueSDNode>(Node->getOperand(2))->getValue();
  SDLoc dl(Node);
  SDValue VAList = DAG.getLoad(MVT::i32, dl, InChain, VAListPtr,
                               MachinePointerInfo(SV), false, false, false, 0);
  // Increment the pointer, VAList, to the next vaarg
  SDValue NextPtr = DAG.getNode(ISD::ADD, dl, MVT::i32, VAList,
                                  DAG.getConstant(VT.getSizeInBits()/8,
                                                  MVT::i32));
  // Store the incremented VAList to the legalized pointer
  InChain = DAG.getStore(VAList.getValue(1), dl, NextPtr,
                         VAListPtr, MachinePointerInfo(SV), false, false, 0);
  // Load the actual argument out of the pointer VAList, unless this is an
  // f64 load.
  if (VT != MVT::f64)
    return DAG.getLoad(VT, dl, InChain, VAList, MachinePointerInfo(),
                       false, false, false, 0);

  // Otherwise, load it as i64, then do a bitconvert.
  SDValue V = DAG.getLoad(MVT::i64, dl, InChain, VAList, MachinePointerInfo(),
                          false, false, false, 0);

  // Bit-Convert the value to f64.
  SDValue Ops[2] = {
    DAG.getNode(ISD::BITCAST, dl, MVT::f64, V),
    V.getValue(1)
  };
  return DAG.getMergeValues(Ops, 2, dl);
}

static SDValue LowerDYNAMIC_STACKALLOC(SDValue Op, SelectionDAG &DAG) {
#if 0
  SDValue Chain = Op.getOperand(0);  // Legalize the chain.
  SDValue Size  = Op.getOperand(1);  // Legalize the size.
  DebugLoc dl = Op.getDebugLoc();

  unsigned SPReg = SP::O6;
  SDValue SP = DAG.getCopyFromReg(Chain, dl, SPReg, MVT::i32);
  SDValue NewSP = DAG.getNode(ISD::SUB, dl, MVT::i32, SP, Size); // Value
  Chain = DAG.getCopyToReg(SP.getValue(1), dl, SPReg, NewSP);    // Output chain

  // The resultant pointer is actually 16 words from the bottom of the stack,
  // to provide a register spill area.
  SDValue NewVal = DAG.getNode(ISD::ADD, dl, MVT::i32, NewSP,
                                 DAG.getConstant(96, MVT::i32));
  SDValue Ops[2] = { NewVal, Chain };
  return DAG.getMergeValues(Ops, 2, dl);
#endif
}



static SDValue LowerRETURNADDR(SDValue Op, SelectionDAG &DAG) {
#if 0
  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
  MFI->setReturnAddressIsTaken(true);

  EVT VT = Op.getValueType();
  DebugLoc dl = Op.getDebugLoc();
  unsigned RetReg = SP::I7;

  uint64_t depth = Op.getConstantOperandVal(0);

  SDValue RetAddr;
  if (depth == 0)
    RetAddr = DAG.getCopyFromReg(DAG.getEntryNode(), dl, RetReg, VT);
  else {
    // flush first to make sure the windowed registers' values are in stack
    SDValue Chain = getFLUSHW(Op, DAG);
    RetAddr = DAG.getCopyFromReg(Chain, dl, SP::I6, VT);

    for (uint64_t i = 0; i != depth; ++i) {
      SDValue Ptr = DAG.getNode(ISD::ADD,
                                dl, MVT::i32,
                                RetAddr,
                                DAG.getIntPtrConstant((i == depth-1)?60:56));
      RetAddr = DAG.getLoad(MVT::i32, dl,
                            Chain,
                            Ptr,
                            MachinePointerInfo(), false, false, false, 0);
    }
  }
  return RetAddr;
#endif
}
#endif

SDValue LC3TargetLowering::
LowerOperation(SDValue Op, SelectionDAG &DAG) const {
  switch (Op.getOpcode()) {
  default: llvm_unreachable("Should not custom lower this!");
  case ISD::BR_CC:              return LowerBR_CC(Op, DAG);
  case ISD::SELECT_CC:          return LowerSELECT_CC(Op, DAG);
  case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
  case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
  case ISD::Constant:           return LowerConstant(Op,DAG);
#if 0
  case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG);
  case ISD::RETURNADDR:         return LowerRETURNADDR(Op, DAG);
  case ISD::GlobalTLSAddress:
    llvm_unreachable("TLS not implemented for LC3.");
  case ISD::FP_TO_SINT:         return LowerFP_TO_SINT(Op, DAG);
  case ISD::SINT_TO_FP:         return LowerSINT_TO_FP(Op, DAG);
  case ISD::VASTART:            return LowerVASTART(Op, DAG, *this);
  case ISD::VAARG:              return LowerVAARG(Op, DAG);
  case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG);
#endif
  }
}


//===----------------------------------------------------------------------===//
//                         LC3 Inline Assembly Support
//===----------------------------------------------------------------------===//

bool
LC3TargetLowering::isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The LC3 target isn't yet aware of offsets.
  return false;
}
