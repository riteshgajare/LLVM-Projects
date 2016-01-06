class LC3Inst<dag outs, dag ins, string asmstr, list<dag> pattern> : Instruction {
  field bits<32> Inst;

  let Namespace = "LC3";
  bits<8> op;
  let Inst{31-24} = op;               // Top two bits are the 'op' field
  
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = asmstr;
  let Pattern = pattern;
}

//===----------------------------------------------------------------------===//
// ALU Instructions
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// PCOffset
//===----------------------------------------------------------------------===//

class Branch<dag outs, dag ins, string asmstr, list<dag> pattern>
   : LC3Inst<outs, ins, asmstr, pattern> {
   let isBranch = 1;
   let isTerminator = 1;   
}

