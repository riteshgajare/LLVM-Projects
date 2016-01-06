/*
 * File: dominance.cpp
 *
 * Description:
 *   This provides a C interface to the dominance analysis in LLVM
 */

#include <stdio.h>
#include <stdlib.h>

/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ProfileInfo.h"
#include "llvm/IR/Type.h"

#include "Profiler.h"

using namespace llvm;

static ProfileInfo *GlobalProfileInfo=NULL;

void llvm::ProfilerInit(ProfileInfo *PI)
{
  GlobalProfileInfo = PI;
}

double llvm::ProfilerGetBlockCount(BasicBlock* BB)
{
  if (GlobalProfileInfo==NULL)
    return -1.;
  
  return GlobalProfileInfo->getExecutionCount(BB);
}

double LLVMProfilerGetBlockCount(LLVMBasicBlockRef BB)
{
  if (GlobalProfileInfo==NULL)
    return -1.0;

  return GlobalProfileInfo->getExecutionCount(unwrap(BB));
}

bool llvm::HasProfileInfo()
{
  return GlobalProfileInfo != NULL;
}

LLVMBool LLVMHasProfileInfo()
{
  return (LLVMBool)(GlobalProfileInfo != NULL);
}

double llvm::ProfilerGetFunctionCount(Function *F)
{
  if (GlobalProfileInfo==NULL)
    return -1.;
  
  return GlobalProfileInfo->getExecutionCount(F);
}

double LLVMProfilerGetFunctionCount(LLVMValueRef F)
{
  Function *Fun = (Function*) unwrap(F);
  if (GlobalProfileInfo==NULL)
    return -1.0;

  return GlobalProfileInfo->getExecutionCount(Fun);
}
