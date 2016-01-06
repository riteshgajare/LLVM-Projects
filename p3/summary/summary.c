/*
 * File: summary.c
 *
 * Description:
 *   This is where you implement your project 3 support.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "dominance.h"
/* Header file global to this project */
#include "summary.h"

typedef struct Stats_def {
	int functions;
	int globals;
	int bbs;
	int insns;
	int insns_bb_deps;
	int insns_g_deps;
	int branches;
	int loads;
	int stores;
	int calls;
	int loops; //approximated by backedges
} Stats;

void pretty_print_stats(FILE *f, Stats s, int spaces) {
	char spc[128];
	int i;

	// insert spaces before each line
	for (i = 0; i < spaces; i++)
		spc[i] = ' ';
	spc[i] = '\0';

	fprintf(f, "%sFunctions.......................%d\n", spc, s.functions);
	fprintf(f, "%sGlobal Vars.....................%d\n", spc, s.globals);
	fprintf(f, "%sBasic Blocks....................%d\n", spc, s.bbs);
	fprintf(f, "%sInstructions....................%d\n", spc, s.insns);
	fprintf(f, "%sInstructions (bb deps)..........%d\n", spc, s.insns_bb_deps);
	fprintf(f, "%sInstructions (g/c deps).........%d\n", spc, s.insns_g_deps);
	fprintf(f, "%sInstructions - Branches.........%d\n", spc, s.branches);
	fprintf(f, "%sInstructions - Loads............%d\n", spc, s.loads);
	fprintf(f, "%sInstructions - Stores...........%d\n", spc, s.stores);
	fprintf(f, "%sInstructions - Calls............%d\n", spc, s.calls);
	fprintf(f, "%sInstructions - Other............%d\n", spc,
			s.insns - s.branches - s.loads - s.stores);
	fprintf(f, "%sLoops...........................%d\n", spc, s.loops);
}

void print_csv_file(const char *filename, Stats s, const char *id) {
	FILE *f = fopen(filename, "w");
	fprintf(f, "id,%s\n", id);
	fprintf(f, "functions,%d\n", s.functions);
	fprintf(f, "globals,%d\n", s.globals);
	fprintf(f, "bbs,%d\n", s.bbs);
	fprintf(f, "insns,%d\n", s.insns);
	fprintf(f, "insns_bb_deps,%d\n", s.insns_bb_deps);
	fprintf(f, "insns_g_deps,%d\n", s.insns_g_deps);
	fprintf(f, "branches,%d\n", s.branches);
	fprintf(f, "loads,%d\n", s.loads);
	fprintf(f, "stores,%d\n", s.stores);
	fprintf(f, "calls,%d\n", s.calls);
	fprintf(f, "loops,%d\n", s.loops);
	fclose(f);
}

Stats MyStats;

typedef struct loop_info {
	LLVMBasicBlockRef loop_header;
	struct loop_info* next;
} LoopInfo;

LoopInfo* root = NULL;

void createLoopList() {
	root = NULL;
	root = malloc(sizeof(LoopInfo));
	root->next = NULL;
}

void push(LLVMBasicBlockRef val) {
	LoopInfo* current = root;
	int IsLoopHeaderPresent = 0;
	while (current->next != NULL) {
		if (current->next->loop_header == val) {
			IsLoopHeaderPresent = 1;
			break;
		} else {
			current = current->next;
		}
	}
	if (!IsLoopHeaderPresent) {
		MyStats.loops++;
		current->next = malloc(sizeof(LoopInfo));
		current->next->loop_header = val;
		current->next->next = NULL;
	}
}

void checkLoops(LLVMValueRef I, LLVMValueRef Fn) {
	LLVMBasicBlockRef CurrBB = LLVMGetInstructionParent(I);
	int OperandsCount = LLVMGetNumOperands(I);
	int var;
	LLVMBasicBlockRef OpBB;
	LLVMValueRef Operand;
	for (var = 0; var < OperandsCount; ++var) {
		Operand = LLVMGetOperand(I, var);
		if (LLVMValueIsBasicBlock(Operand)) {
			OpBB = LLVMValueAsBasicBlock(Operand);
			if (LLVMDominates(Fn, OpBB, CurrBB)) {
				push(OpBB);
				break;
			}
		}
	}
}

void analyzeOperands(LLVMValueRef I, LLVMValueRef Fn) {
	LLVMOpcode opcode = LLVMGetInstructionOpcode(I);
	switch (opcode) {
	// All of these instructions are branches or have special meaning
	case LLVMRet:
		break;
	case LLVMIndirectBr:
		break;
	case LLVMInvoke:
		break;
	case LLVMUnreachable:
		break;
	case LLVMBr:
		checkLoops(I, Fn);
		if (LLVMGetNumOperands(I) > 1) {
			MyStats.branches++;
		}
		break;
	case LLVMStore:
		MyStats.stores++;
		break;
	case LLVMCall:
		MyStats.calls++;
		break;
	case LLVMPHI:
		break;
	case LLVMLoad:
		MyStats.loads++;
		break;
	default:
		break;
	}
}

void IsAGlobalAndConstantOp(LLVMValueRef I) {
	int OperandsCount = LLVMGetNumOperands(I);
	int var;
	int isAGAndCOp = 1;
	LLVMValueRef Operand;
	if (OperandsCount > 0) {
		for (var = 0; var < OperandsCount; ++var) {
			Operand = LLVMGetOperand(I, var);
			if (isAGAndCOp) {
				isAGAndCOp =
						(LLVMIsAGlobalValue(Operand) || LLVMIsAConstant(Operand)) ?
								1 : 0;
			}
			if (!isAGAndCOp)
				break;
		}
		if (isAGAndCOp)
			MyStats.insns_g_deps++;
	}
}

void HasOpInSameBB(LLVMValueRef I, LLVMBasicBlockRef CurrentBlock) {
	int OperandsCount = LLVMGetNumOperands(I);
	int var;
	LLVMValueRef Operand;
	LLVMBasicBlockRef opBB;
	for (var = 0; var < OperandsCount; ++var) {
		Operand = LLVMGetOperand(I, var);
		if (LLVMIsAInstruction(Operand)) {
			opBB = LLVMGetInstructionParent(Operand);
			if (opBB == CurrentBlock) {
				MyStats.insns_bb_deps++;
				break;
			}
		}
	}
}

void analyzeInstruction(LLVMValueRef I, LLVMBasicBlockRef CurrentBlock) {
	HasOpInSameBB(I, CurrentBlock);
	IsAGlobalAndConstantOp(I);
}

void Summarize(LLVMModuleRef Module, const char *id, const char* filename) {
	LLVMValueRef fn_iter; // iterator
	//First we count the number of global variables.
	LLVMValueRef global_iter;
	for (global_iter = LLVMGetFirstGlobal(Module); global_iter != NULL;
			global_iter = LLVMGetNextGlobal(global_iter)) {
		LLVMValueRef Initializer = LLVMGetInitializer(global_iter);
		if (Initializer != NULL) {
			MyStats.globals++;
		}
	}
	for (fn_iter = LLVMGetFirstFunction(Module); fn_iter != NULL; fn_iter =
			LLVMGetNextFunction(fn_iter)) {
		// fn_iter points to a function
		createLoopList();
		LLVMBasicBlockRef bb_iter; /* points to each basic block
		 one at a time */
		if (LLVMCountBasicBlocks(fn_iter) != 0) {
			MyStats.functions++;
		}
		for (bb_iter = LLVMGetFirstBasicBlock(fn_iter); bb_iter != NULL;
				bb_iter = LLVMGetNextBasicBlock(bb_iter)) {
			MyStats.bbs++;
			LLVMValueRef inst_iter; /* points to each instruction */
			for (inst_iter = LLVMGetFirstInstruction(bb_iter);
					inst_iter != NULL;
					inst_iter = LLVMGetNextInstruction(inst_iter)) {
				// get the basic block of this instruction
				MyStats.insns++;
				LLVMBasicBlockRef ref = LLVMGetInstructionParent(inst_iter);
				analyzeInstruction(inst_iter, ref);
				analyzeOperands(inst_iter, fn_iter);
			}
		}
	}
	print_csv_file(filename, MyStats, id);
}
