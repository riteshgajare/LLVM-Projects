/*
 * File: CSE_C.c
 *
 * Description:
 *   This is where you implement the C version of project 4 support.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "dominance.h"
#include "transform.h"

/* Header file global to this project */
#include "cfg.h"
#include "CSE.h"

typedef struct Counter_Stats {
	int Inst_Total;
	int CSE_Basic;
	int CSE_Dead;
	int CSE_Simplify;
	int CSE_RLoad;
	int CSE_Store2Load;
	int CSE_RStore;
} CounterStats;

CounterStats CS;

LLVMModuleRef GModule;

volatile int deleteInst;

void Removable() {
	deleteInst = 1;
}

void printStats() {
	printf("============== Optimization Counters =============\n");
	printf("CSE Total Instruction.........................%d\n", CS.Inst_Total);
	printf("CSE Basic Instruction.........................%d\n", CS.CSE_Basic);
	printf("CSE Dead Instruction..........................%d\n", CS.CSE_Dead);
	printf("CSE Simplify Instruction......................%d\n",
			CS.CSE_Simplify);
	printf("CSE RLoad Instruction.........................%d\n", CS.CSE_RLoad);
	printf("CSE Store2Load Instruction....................%d\n",
			CS.CSE_Store2Load);
	printf("CSE RStore Instruction........................%d\n", CS.CSE_RStore);
}

int isRemovable() {
	int isRemovable = deleteInst;
	deleteInst = 0;
	return isRemovable;
}

LLVMValueRef RemoveInstruction(LLVMValueRef I) {
	LLVMValueRef rm = I;
	I = LLVMGetNextInstruction(I);
	//printf("Remove instruction:  %s\n", LLVMPrintValueToString(rm));
	LLVMInstructionEraseFromParent(rm);
	return I;
}

static int commonSubexpression(LLVMValueRef I, LLVMValueRef J) {
// return 1 if I and J are common subexpressions
// check these things:
//   - they have the same opcode

	int sameSubExpression = 1;
	if (LLVMGetInstructionOpcode(I) == LLVMGetInstructionOpcode(J)) {
		//   - they have the same type
		if (LLVMTypeOf(I) == LLVMTypeOf(J)) {
			//   - they have the same number of operands
			if (LLVMGetNumOperands(I) == LLVMGetNumOperands(J)) {
				//   - all operands are the same (pointer equivalence) LLVMValueRef
				int OperandsCount = LLVMGetNumOperands(I);
				int var;
				if (OperandsCount > 0) {
					if (LLVMIsAICmpInst(I)) {
						if (LLVMGetICmpPredicate (I) != LLVMGetICmpPredicate (J)) {
							sameSubExpression = 0;
						}
					}
					for (var = 0; var < OperandsCount; ++var) {
						if (sameSubExpression) {
							sameSubExpression =
									(LLVMGetOperand(I, var)
											== LLVMGetOperand(J, var)) ? 1 : 0;
						}
						if (!sameSubExpression)
							break;
					}
					return sameSubExpression;
				}
			}
		}
	}

// if any are false, return 0

// return 0 is always conservative, so by default return 0
	return 0;
}

//Check if the instruction is simplifiable or not.
int isSimplify(LLVMValueRef I) {
	LLVMValueRef SimplifyInst = InstructionSimplify(I);
	if (SimplifyInst != NULL) {
		CS.CSE_Simplify++;
		//printf("The simplified expression for : ");
		//LLVMDumpValue(I);
		//printf(" is : ");
		//LLVMDumpValue(SimplifyInst);
		return 1;
	}
	return 0;
}

// This is taken from Tut 5
int isDead(LLVMValueRef I) {
// Are there uses, if so not dead!
	if (LLVMGetFirstUse(I) != NULL)
		return 0;

	LLVMOpcode opcode = LLVMGetInstructionOpcode(I);
	switch (opcode) {
// All of these instructions are branches or have special meaning
	case LLVMRet:
	case LLVMBr:
	case LLVMSwitch:
	case LLVMIndirectBr:
	case LLVMInvoke:
	case LLVMUnreachable:
	case LLVMFence:
	case LLVMStore:
	case LLVMCall:
	case LLVMAtomicCmpXchg:
	case LLVMAtomicRMW:
	case LLVMResume:
	case LLVMLandingPad:
		return 0;
		// We can remove loads, but only if they are non-volatile
	case LLVMLoad:
		if (LLVMGetVolatile(I))
			return 0;
	default:
		break;
	}

	return 1;
}

void handleRLoads(LLVMValueRef I) {
	LLVMValueRef J = LLVMGetNextInstruction(I);
	while (J != NULL) {
		if (LLVMIsALoadInst(J) && !LLVMGetVolatile(J)) {
			// If both of the loads have the same address then we can perfectly eliminate the laod.
			if ((LLVMTypeOf(J) == LLVMTypeOf(I))
					&& (LLVMGetOperand(J, 0) == LLVMGetOperand(I, 0))) {
				CS.CSE_RLoad++;
				LLVMReplaceAllUsesWith(J, I);
				J = RemoveInstruction(J);
				continue;
			}
		}
		if (LLVMIsAStoreInst(J)) {
			break;
		}

		J = LLVMGetNextInstruction(J);
	}
}

void handleRStores(LLVMValueRef I) {
	LLVMValueRef J = LLVMGetNextInstruction(I);
	while (J != NULL) {
		if (LLVMIsALoadInst(J) && !LLVMGetVolatile(J)) {
			// If both of the loads have the same address then we can perfectly eliminate the laod.
			if ((LLVMTypeOf(J) == LLVMTypeOf(LLVMGetOperand(I, 0)))
					&& (LLVMGetOperand(J, 0) == LLVMGetOperand(I, 1))) {
				LLVMReplaceAllUsesWith(J, LLVMGetOperand(I, 0));
				J = RemoveInstruction(J);
				CS.CSE_Store2Load++;
				continue;
			}
		}
		if (LLVMIsAStoreInst(J)
				&& (LLVMGetOperand(J, 1) == LLVMGetOperand(I, 1))) {
			//S is not volatile
			if (!LLVMGetVolatile(I)) {
				//R and S value operands are the same type
				if (LLVMTypeOf(LLVMGetOperand(I, 0))
						== LLVMTypeOf(LLVMGetOperand(J, 0))) {
					CS.CSE_RStore++;
					Removable();
					break;
				}
			}
		}
		if (LLVMIsALoadInst(J) || LLVMIsAStoreInst(J)) {
			break;
		}
		J = LLVMGetNextInstruction(J);
	}
}

static int canHandle(LLVMValueRef I) {
	return !(LLVMIsALoadInst(I) || LLVMIsAStoreInst(I)
			|| LLVMIsATerminatorInst(I) || LLVMIsACallInst(I)
			|| LLVMIsAPHINode(I) || LLVMIsAAllocaInst(I) || LLVMIsAFCmpInst(I)
			|| LLVMIsAVAArgInst(I) || LLVMIsAExtractValueInst(I)
			|| LLVMIsAInsertValueInst(I));
}

// Perform CSE on I for BB and all dominator-tree children
static void processInst(LLVMBasicBlockRef BB, LLVMValueRef I) {
// do nothing if trivially dead
	if (isDead(I)) {
		// Update the list, so that it can be removed from the Module
		Removable();
		CS.CSE_Dead++;
		return;
	}

	isSimplify(I);

	if (LLVMIsALoadInst(I)) {
		handleRLoads(I);
		return;
	}

	if (LLVMIsAStoreInst(I)) {
		handleRStores(I);
		return;
	}

// CSE Algorithm starts here.
	LLVMValueRef J = NULL;
// bale out if not an optimizable instruction
	if (!canHandle(I)) {
		return;
	}

// CSE w.r.t. to I on BB
	J = LLVMGetNextInstruction(I);
	while (J != NULL) {
		commonSubexpression(I, J);
		int isSame = commonSubexpression(I, J);
		if (isSame) {
			LLVMReplaceAllUsesWith(J, I);
			CS.CSE_Basic++;
			J = RemoveInstruction(J);
			continue;
		}
		J = LLVMGetNextInstruction(J);
	}

// for each dom-tree child of BB:
	LLVMBasicBlockRef child = LLVMFirstDomChild(BB);
	while (child) {
		LLVMValueRef J = LLVMGetFirstInstruction(child);
		while (J != NULL) {
			int isSame = commonSubexpression(I, J);
			if (isSame) {
				LLVMReplaceAllUsesWith(J, I);
				CS.CSE_Basic++;
				J = RemoveInstruction(J);
				continue;
			}
			J = LLVMGetNextInstruction(J);
		}

		child = LLVMNextDomChild(BB, child);  // get next child of BB
	}

// zprocessInst(child)
}

static void FunctionCSE(LLVMValueRef Function) {
// for each bb:
	LLVMBasicBlockRef BasicBlock;
	for (BasicBlock = LLVMGetFirstBasicBlock(Function); BasicBlock != NULL;
			BasicBlock = LLVMGetNextBasicBlock(BasicBlock)) {
		LLVMValueRef Instruction = NULL;
		Instruction = LLVMGetFirstInstruction(BasicBlock);
		while (Instruction != NULL) {
			//Total Instruction
			/*printf("Processing Instruction: %s\n",
			 LLVMPrintValueToString(Instruction));*/
			CS.Inst_Total++;
			//Process each Instruction
			processInst(BasicBlock, Instruction);
			if (isRemovable()) {
				Instruction = RemoveInstruction(Instruction);
				continue;
			}
			Instruction = LLVMGetNextInstruction(Instruction);
		}
	}
}

//void processBB(LLVMBasicBlockRef BB, LLVMValueRef I);

void LLVMCommonSubexpressionElimination(LLVMModuleRef Module) {
// Loop over all functions
	GModule = Module;
	LLVMValueRef Function;
	for (Function = LLVMGetFirstFunction(GModule); Function != NULL; Function =
			LLVMGetNextFunction(Function)) {
		FunctionCSE(Function);
	}
	printStats();
// print out summary of results
}

