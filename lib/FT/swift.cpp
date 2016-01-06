/* LLVM Header Files */
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/ValueMap.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/InstIterator.h"

#include <map>


using namespace llvm;

#include "FT.h"
#include "valmap.h"

static LLVMContext *Context;

static Function *AssertFT;
static Function *AssertCFG;

bool canReplicate(Instruction *I)
{
  if(I->isTerminator())
    return false;

  switch(I->getOpcode())
    {
    case Instruction::Call:
    case Instruction::Alloca:
    case Instruction::Store:
      return false;
    default:
      break;
    }


  /* more? hint hint */
  
  return true;
}

// Helpful function for finding 
Instruction *findPointAfter(Instruction *I)
{
  BasicBlock *BB = I->getParent();
  BasicBlock::iterator i,prev,e;

  if( isa<PHINode>(I) )
    {
      return  BB->getFirstNonPHI();
    }

  for(i=BB->begin(),e=BB->end();i!=e; i++)
    {
      prev = i;
      if(I == &*i)
	{	  
	  return &*(++i);
	}
    }
  return NULL;
}

// Build prototypes for runtime library calls
void  BuildAssertPrototype(Module *M)
{
  std::vector<Type*> v;
  v.push_back(IntegerType::get(getGlobalContext(),32));
  v.push_back(IntegerType::get(getGlobalContext(),32));

  ArrayRef<Type*> Params(v);
  FunctionType* FunType = FunctionType::get(Type::getVoidTy(*Context),Params,false);

  // Build prototype for assert_ft
  AssertFT = (Function*)M->getOrInsertFunction("assert_ft",FunType);

  v.push_back(IntegerType::get(getGlobalContext(),32));
  ArrayRef<Type*> Params2(v);
  FunctionType* FunType2 = FunctionType::get(Type::getVoidTy(*Context),Params2,false);
  
  // Build prototype for assert_cfg_ft
  AssertCFG = (Function*)M->getOrInsertFunction("assert_cfg_ft",FunType2);
}

// Calculate number of predecessors for a basic block, useful for creating phi nodes
unsigned GetNumPreds(BasicBlock *BB)
{
  unsigned count=0;
  for (pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI) {
    count++;
  }
  return count;
}

void Swift_Cpp(Module *M)
{
  // Instrument each function
}
