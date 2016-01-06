/*
 * File: SLP_C.c
 *
 * Description:
 *   Stub for SLP in C. This is where you implement your SLP pass.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* LLVM Header Files */
#include "llvm-c/Core.h"
#include "dominance.h"

/* Header file global to this project */
#include "cfg.h"
#include "loop.h"
#include "worklist.h"
#include "valmap.h"

// dom:  does a dom b?
//
//       a and b can be instructions. This is different
//       from the LLVMDominates API which requires basic
//       blocks.

static int dom(LLVMValueRef a, LLVMValueRef b) {
	if (LLVMGetInstructionParent(a) != LLVMGetInstructionParent(b)) {
		LLVMValueRef fun = LLVMGetBasicBlockParent(LLVMGetInstructionParent(a));
		// a dom b?
		return LLVMDominates(fun, LLVMGetInstructionParent(a),
				LLVMGetInstructionParent(b));
	}

	// a and b must be in same block
	// which one comes first?
	LLVMValueRef t = a;
	while (t != NULL) {
		if (t == b)
			return 1;
		t = LLVMGetNextInstruction(t);
	}
	return 0;
}

static LLVMBuilderRef Builder;

typedef struct VectorPairDef {
	LLVMValueRef pair[2];
	int insertAt0;
	struct VectorPairDef *next;
	struct VectorPairDef *prev;
	LLVMValueRef insertV;
} VectorPair;

typedef struct VectorListDef {
	VectorPair *head;
	VectorPair *tail;
	valmap_t visited;
	valmap_t sliceA;
	int size;
	int score;
} VectorList;

static VectorList* VectorList_Create() {
	VectorList *new = (VectorList*) malloc(sizeof(VectorList));
	new->head = NULL;
	new->tail = NULL;
	new->visited = valmap_create();
	new->sliceA = valmap_create();
	new->size = 0;
	return new;
}

static void VectorList_Print(VectorList *list) {
	printf("Store-Chain: \n");
	printf("Pair [1]: \n");
	VectorPair* head = list->head;
	while (head) {
		// Check if all the pairs[0] doms pair[1]
		assert(dom(head->pair[0], head->pair[1]) == 1);

		LLVMDumpValue(head->pair[0]);
		head = head->next;
	}
	head = list->head;
	printf("Pair [2]: \n");
	while (head) {
		LLVMDumpValue(head->pair[1]);
		head = head->next;
	}
	printf("\n");
}

static void VectorList_Destroy(VectorList *list) {
	valmap_destroy(list->visited);
	valmap_destroy(list->sliceA);
	VectorPair *head = list->head;
	VectorPair *tmp;
	while (head) {
		tmp = head;
		head = head->next;
		free(tmp);
	}
	free(list);
}

static int VectorList_CheckIfPresent(VectorList* list, LLVMValueRef a,
		LLVMValueRef b) {
	if (valmap_check(list->visited, a)) {
		return 1;
	}
	if (valmap_check(list->visited, b)) {
		return 1;
	}
	return 0;
}

VectorList* global_list;

int count_sizes_1, count_sizes_2, count_sizes_3, count_sizes_4, count_sizes_5,
		count_store_chain, count_load_chain;

//
// add a and b to the current chain of vectorizable instructions
//
static VectorPair *VectorList_AppendPair(VectorList *list, LLVMValueRef a,
		LLVMValueRef b) {
	//printf("Adding new instructions \n");
	VectorPair *new = (VectorPair*) malloc(sizeof(VectorPair));
	new->pair[0] = a;
	new->pair[1] = b;
	new->insertAt0 = 1;
	valmap_insert(list->visited, a, (void*) 1);
	valmap_insert(list->visited, b, (void*) 1);
	new->next = NULL;
	new->prev = NULL;
	// insert at head
	if (list->head == NULL) {
		list->head = new;
		list->tail = new;
	} else {
		// find right place to insert
		VectorPair *temp = list->head;
		VectorPair *prev = NULL;

		while (temp && dom(temp->pair[0], a)) {
			prev = temp;
			temp = temp->next;
		}
		if (prev) {
			new->next = temp;
			new->prev = prev;
			prev->next = new;
			if (temp) // if not at end
				temp->prev = new;
			else
				list->tail = new;
		} else {
			list->head->prev = new;
			new->next = list->head;
			list->head = new;
		}
	}
	list->size++;
	return new;
}

int Isomorphic(LLVMValueRef I, LLVMValueRef J) {
	//printf("Isomorphic() --> \n");
	if (!LLVMIsAInstruction(I))
		return 0;
	if (!LLVMIsAInstruction(J))
		return 0;

	int i;
	if (LLVMGetInstructionOpcode(I) != LLVMGetInstructionOpcode(J))
		return 0;
	if (LLVMTypeOf(I) != LLVMTypeOf(J))
		return 0;
	if (LLVMGetNumOperands(I) != LLVMGetNumOperands(J))
		return 0;
	for (i = 0; i < LLVMGetNumOperands(I); ++i) {
		if (LLVMTypeOf(LLVMGetOperand(I, i))
				!= LLVMTypeOf(LLVMGetOperand(J, i)))
			return 0;
	}
	return 1;
}

int AdjacentLoads(LLVMValueRef I, LLVMValueRef J) {

	LLVMValueRef gep1 = LLVMGetOperand(I, 0);
	LLVMValueRef gep2 = LLVMGetOperand(J, 0);
	int var;
	if (!LLVMIsAGetElementPtrInst(gep1) || !LLVMIsAGetElementPtrInst(gep2))
		return 0;

	for (var = 0; var < LLVMGetNumOperands(gep1) - 1; ++var) {
		if (LLVMGetOperand(gep1, var) != LLVMGetOperand(gep2, var))
			return 0;
	}

	if (LLVMGetOperand(gep1, 0) == LLVMGetOperand(gep2, 0)) {
		int lastopnum_gep1 = LLVMGetNumOperands(gep1) - 1;
		int lastopnum_gep2 = LLVMGetNumOperands(gep2) - 1;
		if (LLVMIsAConstant(LLVMGetOperand(gep1, lastopnum_gep1))
				&& LLVMIsAConstant(LLVMGetOperand(gep2, lastopnum_gep2))) {
			int gep1_lastOp = LLVMConstIntGetSExtValue(
					LLVMGetOperand(gep1, lastopnum_gep1));
			int gep2_lastOp = LLVMConstIntGetSExtValue(
					LLVMGetOperand(gep2, lastopnum_gep2));

			if (((gep1_lastOp - gep2_lastOp) == 1)
					|| ((gep1_lastOp - gep2_lastOp) == -1)) {
				return 1;
			}
		}
	}
	return 0;
}

int AdjacentStores(LLVMValueRef I, LLVMValueRef J) {

	int var;
	LLVMValueRef gep1 = LLVMGetOperand(I, 1);
	LLVMValueRef gep2 = LLVMGetOperand(J, 1);

	if (!LLVMIsAGetElementPtrInst(gep1) || !LLVMIsAGetElementPtrInst(gep2))
		return 0;

	for (var = 0; var < LLVMGetNumOperands(gep1) - 1; ++var) {
		if (LLVMGetOperand(gep1, var) != LLVMGetOperand(gep2, var))
			return 0;
	}

	if (LLVMGetOperand(gep1, 0) == LLVMGetOperand(gep2, 0)) {

		int lastopnum_gep1 = LLVMGetNumOperands(gep1) - 1;
		int lastopnum_gep2 = LLVMGetNumOperands(gep2) - 1;
		if (LLVMIsAConstant(LLVMGetOperand(gep1, lastopnum_gep1))
				&& LLVMIsAConstant(LLVMGetOperand(gep2, lastopnum_gep2))) {
			int gep1_lastOp = LLVMConstIntGetSExtValue(
					LLVMGetOperand(gep1, lastopnum_gep1));
			int gep2_lastOp = LLVMConstIntGetSExtValue(
					LLVMGetOperand(gep2, lastopnum_gep2));

			if (((gep1_lastOp - gep2_lastOp) == 1)
					|| ((gep1_lastOp - gep2_lastOp) == -1)) {
				return 1;
			}
		}
	}
	return 0;
}

int isIndependent(LLVMValueRef I, LLVMValueRef J) {
	int i, independent_bool;
	LLVMValueRef op, temp;

	if (I == J)
		return 0;

	if (dom(I, J) && I != J) {
		worklist_t insts = worklist_create();
		int i;
		worklist_insert(insts, J);
		while (!worklist_empty(insts)) {
			temp = worklist_pop(insts);

			for (i = 0; i < LLVMGetNumOperands(temp); ++i) {

				op = LLVMGetOperand(temp, i);

				if (!LLVMIsAInstruction(op))
					continue;

				if (op == I) {
					return 0;
				}
				if ((LLVMGetInstructionParent(op) == LLVMGetInstructionParent(I))
						&& dom(I, op)) {
					worklist_insert(insts, op);
				}
			}
		}

	}

	assert(I != J);
	//assert(dom(I, J));

	if (LLVMIsAStoreInst(I)) {

		if (dom(I, J)) {
			temp = LLVMGetNextInstruction(I);
			while (temp != J) {

				if (LLVMIsAStoreInst(temp)
						&& (LLVMGetOperand(I, 1) == LLVMGetOperand(temp, 1)))
					return 0;

				if (LLVMIsALoadInst(temp)
						&& (LLVMGetOperand(I, 1) == LLVMGetOperand(temp, 0)))
					return 0;

				temp = LLVMGetNextInstruction(temp);
			}

		}
	}

	if (LLVMIsALoadInst(I)) {

		if (dom(I, J)) {
			temp = LLVMGetNextInstruction(I);
			while (temp != J) {

				if (LLVMIsAStoreInst(temp)
						&& (LLVMGetOperand(I, 0) == LLVMGetOperand(temp, 1))) {
					return 0;
				}

				if (LLVMIsACallInst(temp))
					return 0;

				temp = LLVMGetNextInstruction(temp);
			}

		}
	}

	return 1;
}

int shouldVectorize(LLVMValueRef I, LLVMValueRef J) {

	//printf("shouldVectorize() -->\n");
	//printf("I : %s\n", LLVMPrintValueToString(I));
	//printf("J : %s\n", LLVMPrintValueToString(J));
	if ((!LLVMIsAInstruction(I)) || (!LLVMIsAInstruction(J)))
		return 0;

	if (!LLVMIsAStoreInst(I)) {

		if (!(LLVMGetTypeKind(LLVMTypeOf(I)) == LLVMIntegerTypeKind
				|| LLVMGetTypeKind(LLVMTypeOf(I)) == LLVMPointerTypeKind
				|| LLVMGetTypeKind(LLVMTypeOf(I)) == LLVMFloatTypeKind))
			return 0;

	}

	if (LLVMGetInstructionParent(I) != LLVMGetInstructionParent(J))
		return 0;

	if (LLVMIsATerminatorInst(I)) // Terminator)
		return 0;

	if ((LLVMIsALoadInst(I) || LLVMIsAStoreInst(I)) && LLVMGetVolatile(I))
		return 0;

	// Call, PHI, ICmp, FCmp, Extract*, Insert*, AddrSpaceCast
	if (LLVMIsACallInst(I) || LLVMIsAPHINode(I) || LLVMIsAICmpInst(I)
			|| LLVMIsAFCmpInst(I) || LLVMIsAAddrSpaceCastInst(I)
			|| LLVMIsAInsertElementInst(I) || LLVMIsAExtractElementInst(I))
		return 0;

	// Atomic
	if ((LLVMGetInstructionOpcode(I) == LLVMAtomicCmpXchg)
			|| LLVMGetInstructionOpcode(I) == LLVMAtomicRMW
			|| LLVMGetInstructionOpcode(I) == LLVMFence)
		return 0;

	if (LLVMIsALoadInst(I) && LLVMIsALoadInst(J)) {
		if (!AdjacentLoads(I, J))
			return 0;
	}

	if (LLVMIsAStoreInst(I) && LLVMIsAStoreInst(J)) {
		if (!AdjacentStores(I, J))
			return 0;
	}

	if (!isIndependent(I, J)) {
		return 0;
	}

	if (LLVMIsAExtractValueInst(I))
		return 0;

	if (LLVMIsAInsertValueInst(I))
		return 0;

	if (LLVMIsAGetElementPtrInst(I))
		return 0;

	if (LLVMIsAShuffleVectorInst(I))
		return 0;

	if (LLVMIsALandingPadInst(I))
		return 0;

	return 1;

}

int canMoveInstruction(LLVMValueRef I, LLVMValueRef J) {

	if (!dom(I, J))
		return 0;

	int i, j;

	LLVMValueRef earliest;

	for (i = 0; i < LLVMGetNumOperands(J); ++i) {

		if (!LLVMIsAInstruction(LLVMGetOperand(J, i)))
			continue;

		if (LLVMGetInstructionParent(J)
				== LLVMGetInstructionParent(LLVMGetOperand(J, i))) {
			j = 1;
			break;
		}
	}

	earliest = LLVMGetNextInstruction(I);

	if (!j) {
		LLVMPositionBuilderBefore(Builder, earliest);
		return 1;
	}

	while (dom(earliest, J)) {

		if (earliest == J) {
			printf("Cannot move the instructions earlier: \n");
			LLVMDumpValue(J);
			return 0;
		}

		for (i = 0; i < LLVMGetNumOperands(J); ++i) {

			if (!LLVMIsAInstruction(LLVMGetOperand(J, i)))
				continue;

			if (dom(earliest, LLVMGetOperand(J, i))) {
				earliest = LLVMGetOperand(J, i);
				break;
			} else {
				continue;
			}
		}

		earliest = LLVMGetNextInstruction(earliest);

		if (i == LLVMGetNumOperands(I))
			break;
	}

	if (earliest == J)
		return 0;

	printf("Moved the instructions earlier: \n");
	LLVMDumpValue(earliest);

	LLVMPositionBuilderBefore(Builder, earliest);
	return 1;

}

VectorList* collectIsomorphicInstructions(VectorList* L, LLVMValueRef I,
		LLVMValueRef J) {

	//printf("collectIsomorphicInstructions() --> \n");
	int i;
	LLVMValueRef temp = J;

	if (!shouldVectorize(I, J))
		return L;

	if (VectorList_CheckIfPresent(L, I, J))
		return L;

	LLVMValueRef clone;

	//printf("collectIsomorphicInstructions()::dom(I, J) \n");
	if (dom(I, J)) {
		VectorList_AppendPair(L, I, J);
		VectorList_AppendPair(global_list, I, J);
	} else {
		VectorList_AppendPair(L, J, I);
		VectorList_AppendPair(global_list, J, I);
	}

	for (i = 0; i < LLVMGetNumOperands(I); ++i) {
		if (Isomorphic(LLVMGetOperand(I, i), LLVMGetOperand(J, i))) {
			collectIsomorphicInstructions(L, LLVMGetOperand(I, i),
					LLVMGetOperand(J, i));
		}
	}

	return L;

}

// AssembleVector: Helper function for generating vector code.
//               It figures out how to assemble a vector from two inputs under
//               the assumption that the inputs are either constants or registers.
//               If constants, it builds a constant vector.  If registers,
//               it emits the insertelement instructions.
//  
//               This is only helpful for generating vector code, not for detecting
//               vectorization opportunities

static LLVMValueRef AssembleVector(LLVMValueRef a, LLVMValueRef b) {
	LLVMTypeRef type = LLVMTypeOf(a);
	LLVMValueRef ret;

// if they are constants...
	if (LLVMIsAConstant(a) && LLVMIsAConstant(b)) {
		// Build constant vector
		LLVMValueRef vec[2] = { a, b };
		ret = LLVMConstVector(vec, 2);
		LLVMDumpValue(ret);
	} else {
		// Otherwise, one of them is a register

		LLVMTypeRef vtype = LLVMVectorType(type, 2);
		LLVMValueRef c = LLVMConstNull(vtype);

		// Insert a into null vector
		ret = LLVMBuildInsertElement(Builder, c, a,
				LLVMConstInt(LLVMInt32Type(), 0, 0), "v.ie");

		LLVMDumpValue(ret);

		// Insert b into ret
		ret = LLVMBuildInsertElement(Builder, ret, b,
				LLVMConstInt(LLVMInt32Type(), 1, 0), "v.ie");

		LLVMDumpValue(ret);
	}

// Return new vector as input for a new vector instruction
	return ret;

}

LLVMValueRef BuildInstruction(LLVMValueRef lower, LLVMOpcode opcode,
		LLVMTypeRef type, worklist_t ops) {

	printf("Building Instruction\n");
	LLVMValueRef ret, operands[3];
	LLVMTypeRef vtype;
	int i = 0;
	while (!worklist_empty(ops)) {
		operands[i] = worklist_pop(ops);
		i++;
	}
	ret = NULL;

	switch (opcode) {
	case LLVMAdd:
		ret = LLVMBuildAdd(Builder, operands[0], operands[1], "v.add");
		break;
	case LLVMFAdd:
		ret = LLVMBuildFAdd(Builder, operands[0], operands[1], "v.fadd");
		break;
	case LLVMSub:
		ret = LLVMBuildSub(Builder, operands[0], operands[1], "v.sub");
		break;
	case LLVMFSub:
		ret = LLVMBuildFSub(Builder, operands[0], operands[1], "v.fsub");
		break;
	case LLVMMul:
		ret = LLVMBuildMul(Builder, operands[0], operands[1], "v.mul");
		break;
	case LLVMFMul:
		ret = LLVMBuildFMul(Builder, operands[0], operands[1], "v.fmul");
		break;
	case LLVMUDiv:
		ret = LLVMBuildUDiv(Builder, operands[0], operands[1], "v.udiv");
		break;
	case LLVMSDiv:
		ret = LLVMBuildSDiv(Builder, operands[0], operands[1], "v.sdiv");
		break;
	case LLVMFDiv:
		ret = LLVMBuildFDiv(Builder, operands[0], operands[1], "v.fdiv");
		break;
	case LLVMURem:
		ret = LLVMBuildURem(Builder, operands[0], operands[1], "v.urem");
		break;
	case LLVMSRem:
		ret = LLVMBuildSRem(Builder, operands[0], operands[1], "v.srem");
		break;
	case LLVMFRem:
		ret = LLVMBuildFRem(Builder, operands[0], operands[1], "v.frem");
		break;
	case LLVMShl:
		ret = LLVMBuildShl(Builder, operands[0], operands[1], "v.shl");
		break;
	case LLVMLShr:
		ret = LLVMBuildLShr(Builder, operands[0], operands[1], "v.lshr");
		break;
	case LLVMAShr:
		ret = LLVMBuildAShr(Builder, operands[0], operands[1], "v.ashr");
		break;
	case LLVMAnd:
		ret = LLVMBuildAnd(Builder, operands[0], operands[1], "v.and");
		break;
	case LLVMOr:
		ret = LLVMBuildOr(Builder, operands[0], operands[1], "v.or");
		break;
	case LLVMXor:
		ret = LLVMBuildXor(Builder, operands[0], operands[1], "v.xor");
		break;
	case LLVMAlloca:
		ret = LLVMBuildAlloca(Builder, type, "v.alloca");
		break;

	case LLVMLoad:
		vtype = LLVMPointerType(LLVMVectorType(type, 2), 0);
		LLVMDumpType(vtype);
		LLVMDumpValue(LLVMGetOperand(lower, 0));
		ret = LLVMBuildBitCast(Builder, LLVMGetOperand(lower, 0), vtype,
				"v.addr");
		LLVMDumpValue(ret);
		ret = LLVMBuildLoad(Builder, operands[0], "v.load");
		break;

	case LLVMStore:
		vtype = LLVMPointerType(LLVMVectorType(type, 2), 0);
		LLVMDumpType(vtype);
		LLVMDumpValue(LLVMGetOperand(lower, 0));
		ret = LLVMBuildBitCast(Builder, LLVMGetOperand(lower, 0), vtype,
				"v.addr");
		LLVMDumpValue(ret);
		ret = LLVMBuildStore(Builder, operands[0], ret);
		break;
		//case LLVMGetElementPtr: ret = LLVMBuildGEP(Builder, operands[0],  LLVMValueRef *Indices, unsigned NumIndices, "v.gep");
		//break;
	case LLVMTrunc:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildTrunc(Builder, operands[0], type, "v.trunc");
		break;
	case LLVMZExt:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildZExt(Builder, operands[0], type, "v.zext");
		break;
	case LLVMSExt:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildSExt(Builder, operands[0], type, "v.sext");
		break;
	case LLVMFPToUI:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildFPToUI(Builder, operands[0], type, "v.fptoui");
		break;
	case LLVMFPToSI:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildFPToSI(Builder, operands[0], type, "v.fptosi");
		break;
	case LLVMUIToFP:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildUIToFP(Builder, operands[0], type, "v.uitofp");
		break;
	case LLVMSIToFP:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildSIToFP(Builder, operands[0], type, "v.sitofp");
		break;
	case LLVMFPTrunc:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildFPTrunc(Builder, operands[0], type, "v.fptrunc");
		break;
	case LLVMFPExt:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildFPExt(Builder, operands[0], type, "v.fpext");
		break;
	case LLVMPtrToInt:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildPtrToInt(Builder, operands[0], type, "v.ptrtoptr");
		break;
	case LLVMIntToPtr:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildIntToPtr(Builder, operands[0], type, "v.inttoptr");
		break;
		//case LLVMSwitch:
	case LLVMSelect:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildSelect(Builder, operands[0], operands[1], operands[2],
				"v.select");
		break;
	case LLVMBitCast:
		type = LLVMVectorType(type, 2);
		ret = LLVMBuildBitCast(Builder, operands[0], type, "v.bitcast");
		break;
	default:
		printf("Cannot build the instruction yet for opcode %d: !\n", opcode);
		break;
	}
	printf("The instruction is vectorized to : %s\n",
			LLVMPrintValueToString(ret));
	return ret;
}

void Vectorize(VectorList* list) {

	if (list->size == 0)
		return;

	printf("Basic block on which list is vectorized!\n");

	LLVMDumpValue(
			LLVMBasicBlockAsValue(
					LLVMGetInstructionParent(list->head->pair[0])));

	int var;
	valmap_t vmap = valmap_create();
	VectorPair* pair = list->head;
	LLVMValueRef lower;
	while (pair != NULL) {
		LLVMValueRef I = pair->pair[0];
		LLVMValueRef J = pair->pair[1];

		if (!dom(pair->pair[0], pair->pair[1])) {
			assert(1);
			I = pair->pair[1];
			J = pair->pair[0];
		}

		printf("Begin vectorization: \n");
		printf("I: %s	J: %s\n\n", LLVMPrintValueToString(I),
				LLVMPrintValueToString(J));

		worklist_t ops = worklist_create();
		for (var = LLVMGetNumOperands(I) - 1; var >= 0; var--) {
			LLVMValueRef av;
			if (!valmap_check(vmap, LLVMGetOperand(I, var))) {
				av = AssembleVector(LLVMGetOperand(I, var),
						LLVMGetOperand(J, var));

				valmap_insert(vmap, LLVMGetOperand(I, var), av);
				valmap_insert(vmap, LLVMGetOperand(J, var), av);
			} else {
				av = valmap_find(vmap, LLVMGetOperand(I, var));
			}
			worklist_insert(ops, av);
		}

		// check which is the later instruction
		//FIXME if i dom j, then i will be the later instruction;.. Right ?
		//******** FIXME *********
		LLVMPositionBuilderBefore(Builder, pair->insertV);
		LLVMTypeRef type = LLVMTypeOf(I);

		if (LLVMIsAStoreInst(I)) {
			LLVMValueRef gep1 = LLVMGetOperand(I, 1);
			LLVMValueRef gep2 = LLVMGetOperand(J, 1);
			int lastopnum_gep1 = LLVMGetNumOperands(gep1) - 1;
			int lastopnum_gep2 = LLVMGetNumOperands(gep2) - 1;
			if (LLVMIsAConstant(LLVMGetOperand(gep1, lastopnum_gep1))
					&& LLVMIsAConstant(LLVMGetOperand(gep2, lastopnum_gep2))) {
				int gep1_lastOp = LLVMConstIntGetSExtValue(
						LLVMGetOperand(gep1, lastopnum_gep1));
				int gep2_lastOp = LLVMConstIntGetSExtValue(
						LLVMGetOperand(gep2, lastopnum_gep2));

				lower = ((gep1_lastOp - gep2_lastOp) == 1) ? gep2 : gep1;
				type = LLVMTypeOf(LLVMGetOperand(I, 0));
			}
		}

		if (LLVMIsALoadInst(I)) {
			LLVMValueRef gep1 = LLVMGetOperand(I, 0);
			LLVMValueRef gep2 = LLVMGetOperand(J, 0);
			int lastopnum_gep1 = LLVMGetNumOperands(gep1) - 1;
			int lastopnum_gep2 = LLVMGetNumOperands(gep2) - 1;
			if (LLVMIsAConstant(LLVMGetOperand(gep1, lastopnum_gep1))
					&& LLVMIsAConstant(LLVMGetOperand(gep2, lastopnum_gep2))) {
				int gep1_lastOp = LLVMConstIntGetSExtValue(
						LLVMGetOperand(gep1, lastopnum_gep1));
				int gep2_lastOp = LLVMConstIntGetSExtValue(
						LLVMGetOperand(gep2, lastopnum_gep2));

				lower = ((gep1_lastOp - gep2_lastOp) == 1) ? gep2 : gep1;
				type = LLVMTypeOf(LLVMGetOperand(I, 0));
			}
		}

		if (!LLVMIsALoadInst(I)) {

			LLVMValueRef newInst = BuildInstruction(lower,
					LLVMGetInstructionOpcode(I), type, ops);

			if (newInst != NULL) {
				valmap_insert(vmap, I, newInst);
				valmap_insert(vmap, J, newInst);
			}
		}

		pair = pair->next;
	}

	pair = list->head;

	while (pair != NULL && dom(pair->pair[0], pair->pair[1])) {

		LLVMValueRef I = pair->pair[0];
		LLVMValueRef J = pair->pair[1];

		if (LLVMIsALoadInst(I)){
			pair = pair->next;
			continue;
		}

		if (LLVMGetFirstUse(I) != NULL) {
			LLVMPositionBuilderBefore(Builder, LLVMGetNextInstruction(I));
			// FIXME : Position Builder at respective place
			LLVMValueRef ev = LLVMBuildExtractElement(Builder,
					valmap_find(vmap, I), LLVMConstInt(LLVMInt32Type(), 0, 0),
					"v.ev");
			LLVMReplaceAllUsesWith(I, ev);
			LLVMInstructionEraseFromParent(I);
		}
		if (LLVMGetFirstUse(J) != NULL) {
			LLVMPositionBuilderBefore(Builder, LLVMGetNextInstruction(J));
			// FIXME : Position Builder at respective place
			LLVMValueRef ev = LLVMBuildExtractElement(Builder,
					valmap_find(vmap, J), LLVMConstInt(LLVMInt32Type(), 1, 0),
					"v.ev");
			LLVMReplaceAllUsesWith(J, ev);
			LLVMInstructionEraseFromParent(J);
		}
		pair = pair->next;
	}

	// TODO: Remove any dead extract elements that we inserted.
}

static void update_stats(VectorList* list) {
	if (list->size == 1)
		count_sizes_1++;
	if (list->size == 2)
		count_sizes_2++;
	if (list->size == 3)
		count_sizes_3++;
	if (list->size == 4)
		count_sizes_4++;
	if (list->size > 4)
		count_sizes_5++;
}

static void print_stats() {
	printf("SLP Results:\n");
	printf("Size: Count\n");
	printf("   1: %d\n", count_sizes_1);
	printf("   2: %d\n", count_sizes_2);
	printf("   3: %d\n", count_sizes_3);
	printf("   4: %d\n", count_sizes_4);
	printf("  >5: %d\n", count_sizes_5);
	printf("Store-chain: %d\n", count_store_chain);
	printf("Load-chain: %d\n", count_load_chain);
}

int transformable(VectorList* list) {

	assert(list != NULL);

	if (list->size < 2)
		return 0;

	VectorPair* vpair = list->head;

	vpair = list->head;
	while (vpair != NULL) {

		LLVMValueRef I = vpair->pair[0];
		LLVMValueRef J = vpair->pair[1];

		if (LLVMGetInstructionParent(I) != LLVMGetInstructionParent(J))
			return 0;

		if (LLVMIsAInstruction(I)) {
			vpair->insertV = dom(I, J) ? J : I;
			vpair = vpair->next;
			continue;
		}

		LLVMValueRef vinst, usei, usej, beforeInst;
		int i;
		LLVMBasicBlockRef bb = LLVMGetInstructionParent(I);

		printf("Doing the transformation check for : %s\n",
				LLVMPrintValueToString(I));

		for (vinst = LLVMGetFirstInstruction(bb); vinst != NULL; vinst =
				LLVMGetNextInstruction(vinst)) {

			usei = LLVMGetUser(LLVMGetFirstUse(I));

			usej = LLVMGetUser(LLVMGetFirstUse(J));

			if ((LLVMGetInstructionParent(usei) != LLVMGetInstructionParent(I))
					|| (LLVMGetInstructionParent(usej)
							!= LLVMGetInstructionParent(J))) {
				printf(
						"Error: Use should be in the same basic block as that of the instruction\n");
			}

			if (dom(vinst, usei) && dom(vinst, usej)) {
				for (i = 0; i < LLVMGetNumOperands(I); i++) {

					if (!LLVMIsAInstruction(LLVMGetOperand(I, i)))
						continue;

					if (!LLVMIsAInstruction(LLVMGetOperand(J, i)))
						continue;

					if (dom(LLVMGetOperand(I, i), vinst)
							&& dom(LLVMGetOperand(J, i), vinst))
						;
					else
						break;
				}
				if (i != LLVMGetNumOperands(I))
					continue;
				else
					break;
			}

		}

		printf("The vectorized instruction is put before : %s\n",
				LLVMPrintValueToString(vinst));

		if (vinst != NULL) {
			vpair->insertV = LLVMGetNextInstruction(vinst);
			vpair = vpair->next;
		} else
			return 0;
	}
	return 1;
}

static void SLPOnBasicBlock(LLVMBasicBlockRef BB) {
	LLVMValueRef I;
	int changed;
	int i = 0;
	global_list = VectorList_Create();
	do {
		changed = 0;
		for (I = LLVMGetLastInstruction(BB); I != NULL; I =
				LLVMGetPreviousInstruction(I)) {
			VectorList* list;
			list = VectorList_Create();
			if (LLVMIsAStoreInst(I)
					&& !VectorList_CheckIfPresent(global_list, I, I)) { // if I is a store and it is not already vectorized:

				LLVMValueRef J = LLVMGetPreviousInstruction(I);
				while (J != NULL) {
					if (LLVMIsAStoreInst(J)
							&& !VectorList_CheckIfPresent(global_list, J, J)) { // for each store, J, in BB, such that J comes before I:
						if (AdjacentStores(I, J) && Isomorphic(I, J)) {
							list = collectIsomorphicInstructions(list, I, J);
						}

						if (list->size >= 2) {
							printf("Store-chain: %d\n", count_store_chain);
							count_store_chain++;
							VectorList_Print(list);
							break;
						}
					}
					J = LLVMGetPreviousInstruction(J);
				}
			}

			if (LLVMIsALoadInst(I)
					&& !VectorList_CheckIfPresent(global_list, I, I)) { // if I is a store and it is not already vectorized:

				LLVMValueRef J = LLVMGetNextInstruction(I);
				while (J != NULL) {
					if (LLVMIsALoadInst(J)) { // for each store, J, in BB, such that J comes before I:
						if (AdjacentLoads(I, J) && Isomorphic(I, J)) {
							list = collectIsomorphicInstructions(list, I, J);
						}

						if (list->size >= 2) {
							printf("Load-chain: %d\n", count_load_chain);
							count_load_chain++;
							VectorList_Print(list);
							//
							break;
						}
					}
					J = LLVMGetNextInstruction(J);
				}
			}

			update_stats(list);
			if (transformable(list)) {
				//VectorList_Print(list);
				//printf("Begin Vectorization: \n");
				changed = 1;
				Vectorize(list);
				LLVMDumpValue(
						LLVMBasicBlockAsValue(
								LLVMGetInstructionParent(list->head->pair[0])));
				printf("========= Done with vectorization ========== \n");
				VectorList_Destroy(list);
				break;
			}

			VectorList_Destroy(list);
		}
	} while (changed);

	VectorList_Destroy(global_list);
}

static void SLPOnFunction(LLVMValueRef F) {
	LLVMBasicBlockRef BB;

	for (BB = LLVMGetFirstBasicBlock(F); BB != NULL;
			BB = LLVMGetNextBasicBlock(BB)) {
		SLPOnBasicBlock(BB);
	}
}

void SLP_C(LLVMModuleRef Module) {
	LLVMValueRef F;
	Builder = LLVMCreateBuilder();
	count_sizes_1 = count_sizes_2 = count_sizes_3 = count_sizes_4 =
			count_sizes_5 = 0;
	for (F = LLVMGetFirstFunction(Module); F != NULL;
			F = LLVMGetNextFunction(F)) {
		SLPOnFunction(F);
	}
	print_stats();

}
