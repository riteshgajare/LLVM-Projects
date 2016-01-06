/*
 * File: swift_c.c
 *
 * Description:
 *   This is where you implement your project 6 support.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "dominance.h"

#include "FT.h"
#include "cfg.h"

#include "valmap.h"
#include "worklist.h"

static LLVMContextRef Context;

static LLVMValueRef AssertFT;
static LLVMValueRef AssertCFG;

static LLVMBuilderRef Builder;

/* Can the instruction be replicated? */
static int canReplicate(LLVMValueRef inst)
{
  switch(LLVMGetInstructionOpcode(inst))
    {
    case LLVMRet:
    case LLVMBr:
    case LLVMSwitch:
    case LLVMIndirectBr:
    case LLVMInvoke:
    case LLVMUnreachable:
    case LLVMAlloca:
    case LLVMCall:
    case LLVMStore:
      return 0;
    default: break;
    }

  /* more? hint, hint */

  return 1;
}


static LLVMValueRef getPointAfter(LLVMValueRef inst)
{
  if (LLVMIsAPHINode(inst))
    return LLVMFirstInstructionAfterPHI(LLVMGetInstructionParent(inst));
  else    
    return LLVMGetNextInstruction(inst);
}


/* Build prototypes for assert_ft and assert_ft_cfg */
static void BuildAssertPrototype(LLVMModuleRef Module)
{
  /* Build AssertFT and AssertCFG */
  LLVMTypeRef ParamArray[3];
  ParamArray[0] = ParamArray[1] = ParamArray[2] = LLVMInt32TypeInContext(Context);

  LLVMTypeRef FType1, FType2;
  
  FType1 = LLVMFunctionType(LLVMVoidTypeInContext(Context),
			    ParamArray,2,0);
  FType2 = LLVMFunctionType(LLVMVoidTypeInContext(Context),
			    ParamArray,3,0);
  
  AssertFT = LLVMAddFunction(Module,"assert_ft",FType1);
  AssertCFG = LLVMAddFunction(Module,"assert_cfg_ft",FType2);
}

void Swift_C(LLVMModuleRef Module)
{

}
