%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"

#include "list.h"
#include "symbol.h"

int num_errors;

extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror();
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;
 LLVMBuilderRef Builder;

LLVMValueRef Function=NULL;
LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params);

LLVMValueRef getZero(LLVMValueRef valType){
	return LLVMConstInt(LLVMTypeOf(valType), 0, 1);
}

%}

/* Data structure for tree nodes*/

%union {
  int num;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN 
%token BREAK CONTINUE
%token INT VOID

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER
/* NUMBER and ID have values associated with them returned from lex*/

%token <num> NUMBER /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/

%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> constant_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <value> primary_expression
%type <value> constant
%type <type>  type_specifier
%type <value> expr_opt
%type <value> function_definition
/* 
   The grammar used here is largely borrowed from Kernighan and Ritchie's "The C
   Programming Language," 2nd Edition, Prentice Hall, 1988. 

   But, some modifications have been made specifically for MiniC!
 */

%%

/* 
   Beginning of grammar: Rules
*/

translation_unit:	  external_declaration
			| translation_unit external_declaration
;

external_declaration:	  function_definition
{
  /* finish compiling function */
	
  if(num_errors>100)
    {
      minic_abort();
    }
  else if(num_errors==0)
    {
      
    }
}
                        | declaration 
{ 
  /* nothing to be done here */
}
;

function_definition:	  type_specifier ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction($1,$2,$4);  
} 
                          compound_stmt 
{ 
  /* This is the rule completion */
  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
	  LLVMDumpModule(Module);
	  
      LLVMBuildRet(Builder,LLVMConstInt(LLVMInt32TypeInContext(Context),
					0,(LLVMBool)1));
    }
  printf("the module is : %s\n", LLVMPrintModuleToString(Module));
  symbol_pop_scope();
  /* make sure basic block has a terminator (a return statement) */
}
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  BuildFunction(LLVMPointerType($1,0),$3,$5);
} 
                          compound_stmt 
{ 
  /* This is the rule completion */


  /* make sure basic block has a terminator (a return statement) */

  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      printf("the module is : %s\n", LLVMPrintModuleToString(Module));
	  LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType(LLVMInt32TypeInContext(Context),0)));
    }
  printf("the module is : %s\n", LLVMPrintModuleToString(Module));
  symbol_pop_scope();
}
;

declaration:    type_specifier STAR declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,LLVMPointerType($1,0),$3);
    } 
  else
    {
      symbol_insert($3,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,LLVMPointerType($1,0),$3), /* build alloca */
		    0);  /* not an arg */
    }

} 
              | type_specifier declarator SEMICOLON
{
  if (is_global_scope())
    {
      LLVMAddGlobal(Module,$1,$2);
    }
  else
    {
      symbol_insert($2,  /* map name to alloca */
		    LLVMBuildAlloca(Builder,$1,$2), /* build alloca */
		    0);  /* not an arg */
    }
} 
;

declaration_list:	   declaration
{

}
                         | declaration_list declaration  
{

}
;


type_specifier:		  INT 
{
  $$ = LLVMInt32TypeInContext(Context);
}
;


declarator:		  ID
{
  $$ = $1;
}
;

param_list_opt:           
{ 
  $$ = NULL;
}
                        | param_list
{ 
  $$ = $1;
}
;

param_list:	
			  param_list COMMA type_specifier declarator
{
  $$ = push_param($1,$4,$3);
}
			| param_list COMMA type_specifier STAR declarator
{
  $$ = push_param($1,$5,LLVMPointerType($3,0));
}
                        | param_list COMMA type_specifier
{
  $$ = push_param($1,NULL,$3);
}
			|  type_specifier declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $2, $1);
}
			| type_specifier STAR declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $3, LLVMPointerType($1,0));
}
                        | type_specifier
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, NULL, $1);
}
;


statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
;

expr_stmt:	           SEMICOLON            
{ 
}
			|  expression SEMICOLON       
{ 
}
;

compound_stmt:		  LBRACE declaration_list_opt statement_list_opt RBRACE 
{
}
;

declaration_list_opt:	
{
}
			| declaration_list
{
}
;

statement_list_opt:	
{
}
			| statement_list
{
}
;

statement_list:		statement
{
}
			| statement_list statement
{
}
;

break_stmt:               BREAK SEMICOLON
{
	loop_info_t info = get_loop();
	LLVMBuildBr(Builder, info.exit);
	LLVMBasicBlockRef breakBlock = LLVMAppendBasicBlock(Function, "after.break.block");
	LLVMPositionBuilderAtEnd(Builder, breakBlock);
};

continue_stmt:            CONTINUE SEMICOLON
{
	loop_info_t info = get_loop();
	LLVMBuildBr(Builder, info.expr);
	LLVMBasicBlockRef breakBlock = LLVMAppendBasicBlock(Function, "after.break.block");
	LLVMPositionBuilderAtEnd(Builder, breakBlock);
};

selection_stmt:		  
  IF LPAREN expression 
  {
	  LLVMBasicBlockRef ifBlock = LLVMAppendBasicBlock(Function, "if.block");
	  LLVMBasicBlockRef elseBlock = LLVMAppendBasicBlock(Function, "else.block");
	  LLVMValueRef Cond = LLVMBuildICmp(Builder, LLVMIntNE, $3, getZero($3), "");
	  LLVMBuildCondBr(Builder, Cond, ifBlock, elseBlock);
	  //Add a condition to add either if block or else block in depending upon the condition.
	  LLVMPositionBuilderAtEnd(Builder, ifBlock);
	  //else
	  
	  $<bb>$ = elseBlock;
  }
  RPAREN statement 
  {
	  LLVMBasicBlockRef joinBlock = LLVMAppendBasicBlock(Function, "join.block");
	  LLVMBasicBlockRef elseBlock = $<bb>4;
	  LLVMBuildBr(Builder, joinBlock);
	  LLVMPositionBuilderAtEnd(Builder, elseBlock);
	  $<bb>$ = joinBlock;
  }
  ELSE statement 
  {
	  LLVMBasicBlockRef joinBlock = $<bb>7;
	  LLVMBuildBr(Builder, joinBlock);
	  LLVMPositionBuilderAtEnd(Builder, joinBlock);
  }
;

iteration_stmt:		  WHILE LPAREN { 
  /* set up header basic block
     make it the new insertion point */
	LLVMBasicBlockRef exprBlock = LLVMAppendBasicBlock(Function, "expr.block");
	LLVMBuildBr(Builder, exprBlock);
	LLVMPositionBuilderAtEnd(Builder, exprBlock);
	$<bb>$ = exprBlock;
} expression RPAREN { 
  /* set up loop body */
	LLVMBasicBlockRef bodyBlock = LLVMAppendBasicBlock(Function, "body.block");
	LLVMBasicBlockRef exprBlock = $<bb>3;
  /* create new body and exit blocks */
	LLVMBasicBlockRef exitBlock = LLVMAppendBasicBlock(Function, "exit.block");
	LLVMValueRef Cond = LLVMBuildICmp(Builder, LLVMIntNE, $4, getZero($4), "");
	LLVMBuildCondBr(Builder, LLVMBuildZExt(Builder, Cond, LLVMInt1TypeInContext(Context), ""), bodyBlock, exitBlock);
	LLVMPositionBuilderAtEnd(Builder, bodyBlock);
  /* to support nesting: */
	push_loop(exprBlock, bodyBlock, bodyBlock, exitBlock);
} 
  statement
{
  /* finish loop */
  loop_info_t info = get_loop();
  LLVMBuildBr(Builder, info.expr);
  LLVMPositionBuilderAtEnd(Builder, info.exit);
  pop_loop();
}
| FOR LPAREN expr_opt 
{
  // build a new basic block for the cond expression (LLVMAppendBasicBlock)
	LLVMBasicBlockRef condBlock = LLVMAppendBasicBlock(Function, "cond.block");
	// remember the cond block for this for loop (stack of nested loops)
	
	// insert a branch from the current basic to the cond basic block (LLVMBuildBr)
	LLVMBuildBr(Builder, condBlock);
	// set builder to insert inside the cond block LLVMPositionBuilderAtEnd(Builder,cond block)
	LLVMPositionBuilderAtEnd(Builder, condBlock);
    $<bb>$ = condBlock;
 } 
SEMICOLON expr_opt 
{
  // build a new block
	LLVMBasicBlockRef condBlock = $<bb>4;
	LLVMBasicBlockRef reinitBlock = LLVMAppendBasicBlock(Function, "reinit.block");
	LLVMBasicBlockRef stateBlock = LLVMAppendBasicBlock(Function,"state.block");
	LLVMBasicBlockRef exitBlock = LLVMAppendBasicBlock(Function,"exit.block");
	push_loop(condBlock, stateBlock, reinitBlock, exitBlock);
  // position builder in this block
	LLVMValueRef Condition = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $6, getZero($6), ""),  LLVMInt1TypeInContext(Context), "");
	LLVMBuildCondBr(Builder, Condition, stateBlock, exitBlock);
	LLVMPositionBuilderAtEnd(Builder, reinitBlock);
  // add the branch back to the cond block
    $<bb>$ = stateBlock;
} 
SEMICOLON expr_opt 
{
  // build a new block for the beginning of the statement
  // add a branch from the cond block to the statement block
  LLVMBasicBlockRef condBlock = $<bb>4;
  LLVMBasicBlockRef stateBlock = $<bb>7;
  LLVMPositionBuilderAtEnd(Builder, stateBlock);
  LLVMBuildBr(Builder,condBlock);
  // set insert piont int the new statement block  
}
RPAREN statement
{
  /* 566: add mid-rule actions to support for loop */
  loop_info_t info = get_loop();
  LLVMBuildBr(Builder, info.reinit);
  LLVMPositionBuilderAtEnd(Builder, info.exit);
  // connect current block ($12) the re-init block 
  pop_loop();
}
;

expr_opt:		
{ 

}
			| expression
{ 

}
;

jump_stmt:		  RETURN SEMICOLON
{ 
	LLVMBuildRetVoid(Builder);

}
			| RETURN expression SEMICOLON
{
	LLVMBuildRet(Builder,$2);
}
;

expression:               assignment_expression
{ 
  $$ = $1;
}
;

assignment_expression:    conditional_expression
{
  $$ = $1;
}
                        | lhs_expression ASSIGN assignment_expression
{
  /* Implement */
  //TODO: Store the value using LLVMBuildStore
  LLVMValueRef valueStored = $3;
  if (LLVMIsAConstantInt($3)){
  	 		 if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMPointerTypeKind && (LLVMConstIntGetZExtValue($3)==0)) {
  	 			LLVMValueRef Null = LLVMConstPointerNull(LLVMPointerType(LLVMInt32Type(), 0));
  	 			valueStored = Null;
  	 	  }
  	  }
  if (LLVMTypeOf($1) == LLVMPointerType(LLVMTypeOf(valueStored), 0))
  {
	  $$ = LLVMBuildStore(Builder, valueStored, $1);
  } 
  else if (LLVMTypeOf($1) == LLVMPointerType(LLVMTypeOf($3), 0))
  {
	  $$ = LLVMBuildStore(Builder, $3, $1);
	  
  } 
  else 
  {
	  exit(parser_error("Assigning to 'int *' from incompatible type 'int', take the address with &"));
  }
}
;


conditional_expression:   logical_OR_expression
{
  $$ = $1;
}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
{
  LLVMValueRef ifCondition = LLVMBuildICmp(Builder, LLVMIntNE, $1, getZero($1), "");
  $$ = LLVMBuildSelect(Builder, ifCondition, $3, $5, "");
  
}
;

constant_expression:       conditional_expression
{ $$ = $1; }
;

logical_OR_expression:    logical_AND_expression
{
  $$ = $1;
}
                        | logical_OR_expression LOGICAL_OR logical_AND_expression
{
  /* Implement */
  LLVMValueRef logicalOp1 = $1;
  LLVMValueRef logicalOp2 = $3;
  if(LLVMIsALoadInst($1)) 
    {
  	  logicalOp1 = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $1, getZero($1), ""),  LLVMInt1TypeInContext(Context), "");
    }
  if(LLVMIsALoadInst($3)) 
    {
  	  logicalOp2 = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $3, getZero($3), ""),  LLVMInt1TypeInContext(Context), "");
    }
  $$ = LLVMBuildZExt(Builder, LLVMBuildOr(Builder, logicalOp1, logicalOp2, ""),  LLVMInt32TypeInContext(Context), "");
  
};

logical_AND_expression:   inclusive_OR_expression
{
  $$ = $1;
}
                        | logical_AND_expression LOGICAL_AND inclusive_OR_expression
{
  /* Implement */
  LLVMValueRef logicalOp1 = $1;
  LLVMValueRef logicalOp2 = $3;
  // If any of the operand is a load instruction, then convert it to Logical
  if(LLVMIsALoadInst($1)) 
  {
	  logicalOp1 = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $1, getZero($1), ""),  LLVMInt1TypeInContext(Context), "");
  }
  if(LLVMIsALoadInst($3)) 
  {
	  logicalOp2 = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $3, getZero($3), ""),  LLVMInt1TypeInContext(Context), "");
  }
  $$ = LLVMBuildZExt(Builder, LLVMBuildAnd(Builder, logicalOp1, logicalOp2, ""),  LLVMInt32TypeInContext(Context), "");
}
;

inclusive_OR_expression:  exclusive_OR_expression
{
    $$=$1;
}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
{
  /* Implement */
  $$ = LLVMBuildOr(Builder, $1, $3, "");
}
;

exclusive_OR_expression:  AND_expression
{
  $$ = $1;
}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
{
  $$ = LLVMBuildXor(Builder, $1, $3, "");
  
}
;

AND_expression:           equality_expression
{
  $$ = $1;
}
                        | AND_expression AMPERSAND equality_expression
{
  /* Implement */
}
;

equality_expression:      relational_expression
{
  $$ = $1;
}
                        | equality_expression EQ relational_expression
{
  /* Implement: use icmp */
  $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntEQ, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
                        | equality_expression NEQ relational_expression
{
  /* Implement: use icmp */
  $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntNE, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
;

relational_expression:    shift_expression
{
    $$ = $1;
}
                        | relational_expression LT shift_expression
{
  /* Implement: use icmp */
   $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntSLT, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
                        | relational_expression GT shift_expression
{
  /* Implement: use icmp */
   $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntSGT, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
                        | relational_expression LTE shift_expression
{
  /* Implement: use icmp */
  $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntSLE, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
                        | relational_expression GTE shift_expression
{
  /* Implement: use icmp */
  $$ = LLVMBuildZExt(Builder, LLVMBuildICmp(Builder, LLVMIntSGE, $1, $3, ""),  LLVMInt1TypeInContext(Context), "");
}
;

shift_expression:         additive_expression
{
    $$=$1;
}
                        | shift_expression LSHIFT additive_expression
{
  /* Implement */
  $$ = LLVMBuildShl(Builder, $1, $3, "");
}
                        | shift_expression RSHIFT additive_expression
{
  /* Implement */
  $$ = LLVMBuildAShr(Builder, $1, $3, "");
}
;

additive_expression:      multiplicative_expression
{
  $$ = $1;
}
                        | additive_expression PLUS multiplicative_expression
{
  /* Implement */
  	LLVMValueRef indices[1] = {LLVMConstInt(LLVMInt32Type(),1,0)};
    if (LLVMTypeOf($1) == LLVMPointerType(LLVMTypeOf($3), 0))
    {
    	int NumIndices;
    	if (LLVMIsAConstantInt($3)) {
    		NumIndices = LLVMConstIntGetZExtValue($3);
    	} 
    	else
    	{
    		//NumIndices = LLVMConstIntGetZExtValue(LLVMConstSExt($3, LLVMInt32Type()));
    	}
    		$$ = LLVMBuildGEP(Builder, $1, indices, NumIndices, "");
    }
    else if (LLVMTypeOf($3) == LLVMPointerType(LLVMTypeOf($1), 0))
    {
    	int NumIndices;
    	if (LLVMIsAConstantInt($3)) {
    		NumIndices = LLVMConstIntGetZExtValue($3);
    	} 
    	else
    	{
    		//NumIndices = LLVMConstIntGetZExtValue(LLVMConstSExt($3, LLVMInt32Type()));
    	}
    		$$ = LLVMBuildGEP(Builder, $3, indices, NumIndices, "");
    }
    else {
    	$$ = LLVMBuildAdd(Builder, $1, $3, "");
    }
  
  
}
                        | additive_expression MINUS multiplicative_expression
{
  LLVMValueRef indices[1] = {LLVMConstInt(LLVMInt32Type(),1,0)};
      if (LLVMTypeOf($1) == LLVMPointerType(LLVMTypeOf($3), 0))
      {
      	int NumIndices;
      	if (LLVMIsAConstantInt($3)) {
      		NumIndices = -1 * LLVMConstIntGetZExtValue($3);
      	} 
      	else
      	{
      		//NumIndices = LLVMConstIntGetZExtValue(LLVMConstSExt($3, LLVMInt32Type()));
      	}
      	$$ = LLVMBuildGEP(Builder, $1, indices, NumIndices, "");
      }
      else if (LLVMTypeOf($3) == LLVMPointerType(LLVMTypeOf($1), 0))
      {
      	int NumIndices;
      	if (LLVMIsAConstantInt($3)) {
      		NumIndices = -1 * LLVMConstIntGetZExtValue($3);
      	} 
      	else
      	{
      		//NumIndices = LLVMConstIntGetZExtValue(LLVMConstSExt($3, LLVMInt32Type()));
      	}
      		$$ = LLVMBuildGEP(Builder, $3, indices, NumIndices, "");
      } else 
      {
    	  LLVMDumpValue(LLVMBuildSub(Builder, $1, $3, "")); 
    	  $$ = LLVMBuildSub(Builder, $1, $3, "");    	  
      }
  
}
;

multiplicative_expression:  cast_expression
{
  $$ = $1;
}
                        | multiplicative_expression STAR cast_expression
{
  $$ = LLVMBuildMul(Builder, $1, $3, "");
}
                        | multiplicative_expression DIV cast_expression
{
  /* Implement */
  $$ = LLVMBuildSDiv(Builder, $1, $3, "");

}
                        | multiplicative_expression MOD cast_expression
{
  $$ = LLVMBuildSRem(Builder, $1, $3, "");
}
;

cast_expression:          unary_expression
{ $$ = $1; }
;

lhs_expression:           ID 
{
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  if (val == NULL)
    {
	  exit(parser_error("Use of undefined variable"));
    }
  else
    $$ = val;
}
                        | STAR ID
{
  int isArg=0;
  LLVMValueRef val = symbol_find($2,&isArg);
  if (val == NULL)
    {
	  exit(parser_error("Use of undefined variable"));
    }
  else
    $$ = LLVMBuildLoad(Builder,val,"");
}
;

unary_expression:         postfix_expression
{
  printf("unary_expression: postfix_expression\n");
  $$ = $1;
}
                        | AMPERSAND primary_expression
{
  /* Implement */
    //If soemthing is loaded it can be an address.
    //You can only take address of something which has a load
    //If you dont have a load then perhaps its incorrect.
	if (LLVMIsALoadInst($2) ) {
		$$ = LLVMGetOperand($2, 0);
	} 
	else
	{
		exit(parser_error("Invalid use of address '&' operand"));
	}
}
                        | STAR primary_expression
{
  $$ = LLVMBuildLoad(Builder, $2, "");
}
                        | MINUS unary_expression
{
  $$ = LLVMBuildNeg(Builder, $2, "");
}
                        | PLUS unary_expression
{
  $$ = $2;
}
                        | BITWISE_INVERT unary_expression
{
  LLVMValueRef MinusOne = LLVMConstInt(LLVMInt32TypeInContext(Context), -1, (LLVMBool)1);
  $$ = LLVMBuildXor(Builder, $2, MinusOne, "");
}
                        | NOT unary_expression
{
  LLVMValueRef CompNEZ = LLVMBuildICmp(Builder, LLVMIntNE, $2, getZero($2), "");
  $$ = LLVMBuildZExt(Builder, LLVMBuildNot(Builder, CompNEZ, ""), LLVMInt32TypeInContext(Context), "");
}
;


postfix_expression:       primary_expression
{
  $$ = $1;
}
;

primary_expression:       ID 
{ 
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  if (val == NULL)
	  exit(parser_error("Use of undefined variable"));
  else
    $$ = LLVMBuildLoad(Builder,val,"");
}
                        | constant
{
  $$ = $1;
}
                        | LPAREN expression RPAREN
{
  $$ = $2;
}
;

constant:	          NUMBER  
{ 
  /* Implement */
  $$ = LLVMConstInt(LLVMInt32TypeInContext(Context), $1, (LLVMBool) 1);
} 
;

%%

LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);
  LLVMTypeRef *ParamArray = malloc(sizeof(LLVMTypeRef)*size);
  LLVMTypeRef FunType;
  LLVMBasicBlockRef BasicBlock;

  paramlist_t *tmp = params;
  /* Build type for function */
  for(i=size-1; i>=0; i--) 
    {
      ParamArray[i] = tmp->type;
      tmp = next_param(tmp);
    }
  
  FunType = LLVMFunctionType(RetType,ParamArray,size,0);

  Function = LLVMAddFunction(Module,name,FunType);
  
  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);

  tmp = params;
  for(i=size-1; i>=0; i--)
    {
      LLVMValueRef alloca = LLVMBuildAlloca(Builder,tmp->type,tmp->name);
      LLVMBuildStore(Builder,LLVMGetParam(Function,i),alloca);
      symbol_insert(tmp->name,alloca,0);
      tmp=next_param(tmp);
    }

  return Function;
}

extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror()
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
