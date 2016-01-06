//===-- LC3CallingConv.td - Calling Conventions LC3 ----*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This describes the calling conventions for the LC3 architectures.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Return Value Calling Conventions
//===----------------------------------------------------------------------===//

// LC3 32-bit C return-value convention.
def RetCC_LC3_32 : CallingConv<[
    CCAssignToStack<4, 4>
    //  CCIfType<[i32], CCAssignToReg<[R0]>>
    //  CCIfType<[f32], CC	 Custom<"CC_LC3_Ret_f32">>,
    //  CCIfType<[f64], CCCustom<"CC_LC3_Ret_f64">>
]>;

// LC3 32-bit C Calling convention.
def CC_LC3_32 : CallingConv<[
  //Custom assign SRet to [sp+64].
  CCIfSRet<CCCustom<"CC_LC3_Assign_SRet">>,
  
  // i32 f32 arguments get passed in integer registers if there is space.
  //CCIfType<[i32, f32], CCAssignToReg<[R0,R1]>>,
  // f64 arguments are split and passed through registers or through stack.
  //CCIfType<[f64], CCCustom<"CC_LC3_Assign_f64">>,

  // Alternatively, they are assigned to the stack in 4-byte aligned units.
  CCAssignToStack<4, 4>
]>;
