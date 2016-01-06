//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//

include "LC3InstrFormats.td"

def brtarget : Operand<OtherVT>;
def calltarget : Operand<i32>;

def simm5  : PatLeaf<(imm), [{ return isInt<5>(N->getSExtValue()); }]>;
def simm6  : PatLeaf<(imm), [{ return isInt<6>(N->getSExtValue()); }]>;
def simm9  : PatLeaf<(imm), [{ return isInt<9>(N->getSExtValue()); }]>;

def LO14 : SDNodeXForm<imm, [{
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() & ((1<<14)-1),
                                   MVT::i32);
}]>;

def HI18 : SDNodeXForm<imm, [{
  // Transformation function: shift the immediate value down into the low bits.
  return CurDAG->getTargetConstant((unsigned)N->getZExtValue() >> 14, MVT::i32);
}]>;



def ADDRri : ComplexPattern<i32, 2, "SelectADDRri", [frameindex,add], []>;

def MEMri : Operand<i32> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GPRegs, i32imm);
}


// New Types and SDNodes needed for LC-3

def SDTCallType : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;

def call : SDNode<"LC3ISD::CALL", SDTCallType,
                  [SDNPHasChain,  SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;

// Pseudo instructions.
class Pseudo<dag outs, dag ins, string asmstr, list<dag> pattern>
   : LC3Inst<outs, ins, asmstr, pattern>;

let Defs = [NZP] in {

def ADDrr : LC3Inst< (outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
		   "ADD $DR, $SR1, $SR2",
		   [(set GPRegs:$DR, (add GPRegs:$SR1, GPRegs:$SR2))]>;                 

def ADDri : LC3Inst< (outs GPRegs:$DR), (ins GPRegs:$SR1, i32imm:$imm),
		   "ADD $DR, $SR1, $imm",
		   [(set GPRegs:$DR, (add GPRegs:$SR1, simm5:$imm))]>;                 

def ANDrr : LC3Inst< (outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
		   "AND $DR, $SR1, $SR2",
		   [(set GPRegs:$DR, (and GPRegs:$SR1, GPRegs:$SR2))]>;                 

def ANDri : LC3Inst< (outs GPRegs:$DR), (ins GPRegs:$SR1, i32imm:$imm),
		   "AND $DR, $SR1, $imm",
		   [(set GPRegs:$DR, (and GPRegs:$SR1, simm5:$imm))]>;                 

def NOT   : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR),
                "NOT $DR, $SR",
		[(set GPRegs:$DR, (not GPRegs:$SR))]>;

//def SHLrr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "SHL $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (shl GPRegs:$SR1, GPRegs:$SR2))]>;                 

//def SRLrr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "SHR $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (srl GPRegs:$SR1, GPRegs:$SR2))]>;                 

//def SRArr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "SRA $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (sra GPRegs:$SR1, GPRegs:$SR2))]>;                 

//def MULrr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "MUL $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (mul GPRegs:$SR1, GPRegs:$SR2))]>;                 

//def SUBrr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "SUB $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (sub GPRegs:$SR1, GPRegs:$SR2))]>;                 

//def ORrr : LC3Inst<(outs GPRegs:$DR), (ins GPRegs:$SR1, GPRegs:$SR2),
//		   "OR $DR, $SR1, $SR2",
//		   [(set GPRegs:$DR, (or GPRegs:$SR1, GPRegs:$SR2))]>;                 

}


let Defs = [NZP], usesCustomInserter = 1 in { 

  def LC3MUL
   : Pseudo<(outs GPRegs:$RD), (ins GPRegs:$SR1, GPRegs:$SR2),
            "; LC3MUL PSEUDO!",
            [(set GPRegs:$RD, (mul GPRegs:$SR1, GPRegs:$SR2))]>;

  def LC3DIV
   : Pseudo<(outs GPRegs:$RD), (ins GPRegs:$SR1, GPRegs:$SR2),
            "; LC3DIV PSEUDO!",
            [(set GPRegs:$RD, (sdiv GPRegs:$SR1, GPRegs:$SR2))]>;

  def LC3SHL
   : Pseudo<(outs GPRegs:$RD), (ins GPRegs:$SR1, GPRegs:$SR2),
            "; LC3SHL PSEUDO!",
            [(set GPRegs:$RD, (shl GPRegs:$SR1, GPRegs:$SR2))]>;

  def LC3SRL
   : Pseudo<(outs GPRegs:$RD), (ins GPRegs:$SR1, GPRegs:$SR2),
            "; LC3SRL PSEUDO!",
            [(set GPRegs:$RD, (srl GPRegs:$SR1, GPRegs:$SR2))]>;

  def LC3SRA
   : Pseudo<(outs GPRegs:$RD), (ins GPRegs:$SR1, GPRegs:$SR2),
            "; LC3SRA PSEUDO!",
            [(set GPRegs:$RD, (sra GPRegs:$SR1, GPRegs:$SR2))]>;

}
//---------------------------





// Compares
def LC3cmpcc : SDNode<"LC3ISD::CMPCC", SDTIntBinOp, [SDNPOutGlue]>;

// Branches
let PrintMethod = "printCCOperand" in
  def CCOp : Operand<i32>;


// SELECT_CC_* - Used to implement the SELECT_CC DAG operation.  Expanded after
// instruction selection into a branch sequence.  This has to handle all
// permutations of selection between i32/f32/f64 on ICC and FCC.
  // Expanded after instruction selection.

def SDT_LC3_selectcc :
SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i32>]>;

def LC3selectcc : SDNode<"LC3ISD::SELECT_CC", SDT_LC3_selectcc, [SDNPInGlue]>;

let Uses = [NZP], usesCustomInserter = 1 in { 
  def SELECT_CC_NZP
   : Pseudo<(outs GPRegs:$dst), (ins GPRegs:$T, GPRegs:$F, i32imm:$Cond),
            "; SELECT_CC_NZP PSEUDO!",
            [(set GPRegs:$dst, (LC3selectcc GPRegs:$T, GPRegs:$F,
                                             imm:$Cond))]>;
}

def SDT_LC3_brcc : 
SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>, SDTCisVT<1, i32>]>;

def LC3brcc : SDNode<"LC3ISD::BRCC", SDT_LC3_brcc, [SDNPHasChain, SDNPInGlue]>;


let Uses = [NZP] in
   def BRCOND : Branch<(outs),
                       (ins brtarget:$dst, CCOp:$cc), 
                       "BR$cc $dst",
                       [(LC3brcc bb:$dst, imm:$cc)]>;

def BRA : Branch< (outs),
                       (ins brtarget:$dst), 
                       "BR $dst",
                       [(br bb:$dst)]>;

def JMP : Branch<(outs),
                    (ins GPRegs:$BaseR), "JMP $BaseR", [(brind GPRegs:$BaseR)]>;

def LC3retflag       : SDNode<"LC3ISD::RET_FLAG", SDTNone,
                              [SDNPHasChain, SDNPOptInGlue]>;

let isReturn = 1, isTerminator = 1, Uses = [R7] in {
  def RET : Branch<(outs), (ins),
                  "RET", [(LC3retflag)]>;
}


let Uses = [R5,R6],
    hasDelaySlot = 1, isCall = 1,
    Defs = [R0,R1,R2,R3,R7] in 
{
    def JSR : Branch<(outs),
    	               (ins calltarget:$func, variable_ops), 
                       "JSR $func",[]>; 
    		       
}


def LI : LC3Inst<(outs GPRegs:$DR), (ins i32imm:$imm),
         "LI $DR, $imm",
	 [(set GPRegs:$DR, simm9:$imm)]>;

// FIXME: fix instruction encoding
// Loads

let Defs = [NZP] in {
def LDR : LC3Inst<(outs GPRegs:$DR), (ins MEMri:$addr),
                  "LDR   $DR, $addr",
                  [(set GPRegs:$DR, (load ADDRri:$addr))]>;


def LDRH : LC3Inst< 
                  (outs GPRegs:$DR), (ins MEMri:$addr),
                  "LDR.h $DR, $addr",
                  [(set GPRegs:$DR, (sextloadi16 ADDRri:$addr))]>;

def LDRB : LC3Inst<
                 (outs GPRegs:$DR), (ins MEMri:$addr),
                  "LDR.b $DR, $addr",
                 [(set GPRegs:$DR, (sextloadi8 ADDRri:$addr))]>;
}
// Stores

def STR : LC3Inst<(outs),
                  (ins GPRegs:$SR, MEMri:$addr),
                  "STR   $SR, $addr",
                  [(store GPRegs:$SR,ADDRri:$addr)]>;

def STRH : LC3Inst<(outs),
                  (ins GPRegs:$SR, MEMri:$addr),
                  "STR.h  $SR, $addr",
                  [(truncstorei16 GPRegs:$SR,ADDRri:$addr)]>;

def STRB : LC3Inst<(outs),
                  (ins GPRegs:$SR, MEMri:$addr),
                  "STR.b  $SR, $addr",
                  [(truncstorei8 GPRegs:$SR,ADDRri:$addr)]>;




//  These are target-independent nodes, but have target-specific formats.
def SDT_CallSeqStart : SDCallSeqStart<[ SDTCisVT<0, i32> ]>;
def SDT_CallSeqEnd   : SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                        SDTCisVT<1, i32> ]>;


def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_CallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",   SDT_CallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;


let Defs = [R6], Uses = [R6] in {
def ADJCALLSTACKDOWN : Pseudo<(outs), (ins i32imm:$amt),
                               "!ADJCALLSTACKDOWN $amt",
                               [(callseq_start timm:$amt)]>;
def ADJCALLSTACKUP : Pseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "!ADJCALLSTACKUP $amt1",
                            [(callseq_end timm:$amt1, timm:$amt2)]>;
}



// Handle small constants

def : Pat<(i32 simm9:$val), (LI simm9:$val)>; 

def LC3Imm32 : SDNodeXForm<imm,[{
  int iVal = N->getSExtValue();
  SDValue val;
  val = DAG.getNode(ISD::ADD,N->getDebugLoc(),MVT::i32,
             DAG.getTargetConstant(0), DAG.getTargetConstant(256);
  iVal = iVal - 256;
  while(iVal>0) {
     val = DAG.getNode(ISD::ADD,N->getDebugLoc(),MVT::i32,
             val, DAG.getTargetConstant((ival<256)?ival:256);
     iVal = iVal - 256;
  }
  return val;
}]>;


def : PatLeaf<(imm),[{ return isInt<32>(N->getSExtValue()); }],LC3Imm32>;

// Handle arbitrary constant

//def : Pat<(i32 imm:$val),
//          (ORrr (SRLrr (LI (HI18 imm:$val)),18), (LI (LO14 imm:$val)))>;


def : Pat<(xor GPRegs:$SR1, GPRegs:$SR2),
          (NOT (ANDrr (NOT (ANDrr GPRegs:$SR1, (NOT GPRegs:$SR2))), (NOT (ANDrr GPRegs:$SR2, (NOT GPRegs:$SR1)))))>;

def : Pat<(or GPRegs:$SR1, GPRegs:$SR2),
          (NOT (ANDrr (NOT GPRegs:$SR1), (NOT GPRegs:$SR2)))>;

def : Pat<(sub GPRegs:$SR1, GPRegs:$SR2),
          (ADDrr GPRegs:$SR1, (ADDri (NOT GPRegs:$SR2), 1))>;

def : Pat<(LC3cmpcc GPRegs:$SR1, GPRegs:$SR2),
          (ADDrr GPRegs:$SR1, (ADDri (NOT GPRegs:$SR2), 1))>;

def : Pat<(call tglobaladdr:$dst),  (JSR tglobaladdr:$dst)>;
def : Pat<(call texternalsym:$dst), (JSR texternalsym:$dst)>;

def : Pat<(frameindex), (ADDri R5,0)>;
