/*
 * File: LICM_C.c
 *
 * Description:
 *   Stub for LICM in C. This is where you implement your LICM pass.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "dominance.h"

/* Header file global to this project */
#include "cfg.h"
#include "loop.h"
#include "worklist.h"
#include "valmap.h"

static worklist_t list;

static LLVMBuilderRef Builder = NULL;

unsigned int LICM_Count = 0;
unsigned int LICM_NoPreheader = 0;
unsigned int LICM_AfterLoop = 0;
unsigned int LICM_Load = 0;
unsigned int LICM_BadCall = 0;
unsigned int LICM_BadStore = 0;
unsigned int LICM_AddrLoopIn = 0;
unsigned int LICM_Dominates = 0;
unsigned int LICM_Global = 0;
unsigned int LICM_Invariant = 0;

valmap_t stores_map;

void addBadStore(LLVMValueRef store) {
	if (!valmap_check(stores_map, store)) {
		LICM_BadStore++;
		valmap_insert(stores_map, store, NULL);
	}
}

void addBadCall(LLVMValueRef call) {
	if (!valmap_check(stores_map, call)) {
		LICM_BadCall++;
		valmap_insert(stores_map, call, NULL);
	}
}

int LLVMLoopHasACall(LLVMLoopRef L, LLVMValueRef addr) {
	worklist_t bbs = LLVMGetBlocksInLoop(L);
	int i;
	while (!worklist_empty(bbs)) {
		LLVMBasicBlockRef BB = LLVMValueAsBasicBlock(worklist_pop(bbs));
		LLVMValueRef I;
		for (I = LLVMGetFirstInstruction(BB); I != NULL; I =
				LLVMGetNextInstruction(I)) {

			if (LLVMIsACallInst(I)) {
				for (i = 0; i < LLVMGetNumOperands(I); ++i) {
					if (addr != NULL) {
						if (LLVMGetOperand(I, i) == addr) {
							addBadCall(I);
							return 1;
						}
					} else {
						addBadCall(I);
						return 1;
					}
				}
			}
		}
	}
	return 0;
}
// case 1 is all stores are allocas
// case 2 is all stores have address outside the loop
// case 3 is there are no stores
int LLVMLoopHasAStore(LLVMLoopRef L, LLVMValueRef addr, int case_) {
	worklist_t bbs = LLVMGetBlocksInLoop(L);
	while (!worklist_empty(bbs)) {
		LLVMBasicBlockRef BB = LLVMValueAsBasicBlock(worklist_pop(bbs));

		LLVMValueRef I;

		for (I = LLVMGetFirstInstruction(BB); I != NULL; I =
				LLVMGetNextInstruction(I)) {

			if (LLVMIsACallInst(I)) {
				addBadCall(I);
				return 1;
			}

			if (case_ == 1) {

				if (LLVMIsAStoreInst(I)
						&& !LLVMIsAAllocaInst(LLVMGetOperand(I, 1))
						&& !LLVMIsAConstant(LLVMGetOperand(I, 1))) {
					addBadStore(I);
					return 1;
				}

				if (LLVMIsAStoreInst(I) && LLVMGetOperand(I, 1) == addr) {
					addBadStore(I);
					return 1;
				}
			}

			if (case_ == 2) {
				if (LLVMIsAStoreInst(I)) {
					return 1;
				}
			}
		}
	}
	return 0;
}

int LLVMInstDominatesLoopExit(LLVMLoopRef L, LLVMValueRef I) {
	LLVMBasicBlockRef I_bb = LLVMGetInstructionParent(I);
	LLVMValueRef fun = LLVMGetBasicBlockParent(I_bb);
	worklist_t exitbbs = LLVMGetExitBlocks(L);
	if (worklist_empty(exitbbs)) {
		return 0;
	}
	while (!worklist_empty(exitbbs)) {
		LLVMBasicBlockRef exit_bb = LLVMValueAsBasicBlock(
				worklist_pop(exitbbs));
		if (!LLVMDominates(fun, I_bb, exit_bb)) {
			return 0;
		}
	}
	return 1;
}

int canMoveOutOfLoop(LLVMLoopRef L, LLVMValueRef I, LLVMValueRef addr) {

// addr is a constant and there are no stores to addr in L
	if ((LLVMIsAConstant(addr) || LLVMIsAGlobalValue(addr))
			&& !LLVMLoopHasAStore(L, addr, 1)) {
		return 1;
	}

	// addr is an AllocaInst and no stores to addr in L and AllocaInst is not inside the loop
	if (LLVMIsAAllocaInst(addr) && !LLVMLoopHasAStore(L, addr, 1)
			&& !LLVMLoopContainsInst(L, addr)) {
		return 1;
	}

	// Third condition
	if (!LLVMLoopHasAStore(L, NULL, 2) && !LLVMLoopContainsInst(L, addr)
			&& LLVMInstDominatesLoopExit(L, I)) {
		LICM_Dominates++;
		return 1;
	}

	return 0;
}

void print_loop(LLVMLoopRef L) {
	worklist_t bbs = LLVMGetBlocksInLoop(L);
	while (!worklist_empty(bbs)) {
		printf("%s\n", LLVMPrintValueToString(worklist_pop(bbs)));
	}
}

void LICM(LLVMLoopRef L) {
	//printf("******** Starting LICM ************\n");
	//print_loop(L);
	LLVMBasicBlockRef PH = LLVMGetPreheader(L);
	if (PH == NULL) {
		LICM_NoPreheader++;
		return;
	}

	LLVMPositionBuilderBefore(Builder, LLVMGetLastInstruction(PH));

	worklist_t bbs = LLVMGetBlocksInLoop(L);
	if (worklist_empty(bbs)) {
		return;
	}
	while (!worklist_empty(bbs)) {
		LLVMBasicBlockRef BB = LLVMValueAsBasicBlock(worklist_pop(bbs));

		LLVMValueRef I = LLVMGetFirstInstruction(BB);
		while (I != NULL) {
			if (LLVMMakeLoopInvariant(L, I)) {
				LICM_Count++;
				LICM_Invariant++;
			} else if (LLVMIsALoadInst(I) && !LLVMGetVolatile(I)) {
				LLVMValueRef addr = LLVMGetOperand(I, 0);
				if (canMoveOutOfLoop(L, I, addr)) {
					LICM_Count++;
					LICM_Load++;
					LLVMValueRef clone = LLVMCloneInstruction(I);
					LLVMValueRef rm = I;
					LLVMInsertIntoBuilder(Builder, clone);
					LLVMReplaceAllUsesWith(I, clone);
					I = LLVMGetNextInstruction(I);
					LLVMInstructionEraseFromParent(rm);
					continue;
				}
			}
			I = LLVMGetNextInstruction(I);
		}
	}
}

void LICMOnFunction(LLVMValueRef funs) {
	LLVMLoopInfoRef LoopInfoRef = LLVMCreateLoopInfoRef(funs);
	LLVMLoopRef Loop;
	for (Loop = LLVMGetFirstLoop(LoopInfoRef); Loop != NULL; Loop =
			LLVMGetNextLoop(LoopInfoRef, Loop)) {
		LICM(Loop);
	}
}

void LoopInvariantCodeMotion_C(LLVMModuleRef Module) {
	LLVMValueRef funs;
	Builder = LLVMCreateBuilder();
	list = worklist_create();
	for (funs = LLVMGetFirstFunction(Module); funs != NULL; funs =
			LLVMGetNextFunction(funs)) {
		if (LLVMCountBasicBlocks(funs) > 0) {
			stores_map = valmap_create();
			LICMOnFunction(funs);
			valmap_destroy(stores_map);
		}
	}
	LLVMDisposeBuilder(Builder);
	Builder = NULL;
	fprintf(stderr, "LICM_Count      =%d\n", LICM_Count);
	fprintf(stderr, "LICM_NoPreheader=%d\n", LICM_NoPreheader);
	fprintf(stderr, "LICM_Load       =%d\n", LICM_Load);
	fprintf(stderr, "LICM_BadCall    =%d\n", LICM_BadCall);
	fprintf(stderr, "LICM_BadStore   =%d\n", LICM_BadStore);
	fprintf(stderr, "LICM_AddrLoopIn   =%d\n", LICM_AddrLoopIn);
	fprintf(stderr, "LICM_Dominates   =%d\n", LICM_Dominates);
	fprintf(stderr, "LICM_Global    =%d\n", LICM_Global);
	fprintf(stderr, "LICM_Invariant    =%d\n", LICM_Invariant);
}
