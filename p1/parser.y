%{
#include <stdio.h>
#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"
#include <string.h>

#include "uthash.h"

#include <errno.h>
  //#include <search.h>

extern FILE *yyin;
int yylex(void);
int yyerror(const char *);

extern char *fileNameOut;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;

LLVMValueRef Function;
LLVMBasicBlockRef BasicBlock;
LLVMBuilderRef Builder;

//struct hsearch_data params;
//struct hsearch_data tmps;

int params_cnt=0;

struct TmpMap{
  char *key;                  /* key */
  LLVMValueRef val;                /* data */
  UT_hash_handle hh;         /* makes this structure hashable */
};
 

struct TmpMap *map = NULL;    /* important! initialize to NULL */

void add_tmp(char *tmp, LLVMValueRef val) { 
  struct TmpMap *s; 
  s = malloc(sizeof(struct TmpMap)); 
  s->key = strdup(tmp); 
  s->val = val; 
  HASH_ADD_KEYPTR( hh, map, s->key, strlen(s->key), s ); 
}

LLVMValueRef get_val(char *tmp) {
  struct TmpMap *s;
  HASH_FIND_STR( map, tmp, s );  /* s: output pointer */
  if (s) 
    return s->val;
  else 
    return NULL; // returns NULL if not found
}

%}

%union {
  char *tmp;
  int num;
  char *id;
  LLVMValueRef val;
}

%token ASSIGN SEMI COMMA MINUS PLUS VARS
%token <tmp> TMP 
%token <num> NUM 
%token <id> ID
%type <val> expr stmt stmtlist;

%left PLUS MINUS
%left MULTIPLY DIVIDE

%start program

%%

program: decl stmtlist 
{ 
 
  LLVMBuildRet(Builder,
	       LLVMConstInt(LLVMInt64TypeInContext(Context),0,(LLVMBool)1));
}
  ;

decl: VARS varlist SEMI 
{  
  /* Now we know how many parameters we need.  Create a function type
     and add it to the Module */

  LLVMTypeRef Integer = LLVMInt64TypeInContext(Context);

  LLVMTypeRef *IntRefArray = malloc(sizeof(LLVMTypeRef)*params_cnt);
  int i;
  
  /* Build type for function */
  for(i=0; i<params_cnt; i++)
    IntRefArray[i] = Integer;

  LLVMBool var_arg = 0; /* false */
  LLVMTypeRef FunType = LLVMFunctionType(Integer,IntRefArray,params_cnt,var_arg);

  /* Found in LLVM-C -> Core -> Modules */
  char *tmp, *out = fileNameOut;

  if ((tmp=strchr(out,'.'))!='\0')
    {
      *tmp = 0;
    }

  /* Found in LLVM-C -> Core -> Modules */
  Function = LLVMAddFunction(Module,out,FunType);

  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);
}
;

varlist:   varlist COMMA ID 
{
  /* IMPLEMENT: remember ID and its position so that you can
     reference the parameter later
   */
  params_cnt++;
}
| ID
{
  /* IMPLEMENT: remember ID and its position for later reference*/
  params_cnt++;
}
;

stmtlist:  stmtlist stmt { $$ = $2; }
| stmt                   { $$ = $1; }
;         

stmt: TMP ASSIGN expr SEMI
{
  /* IMPLEMENT: remember temporary and associated expression $3 */
  $$ = $3;
}
;

expr:   expr MINUS expr
{
  /* IMPLEMENT: subtraction */
} 
     | expr PLUS expr
{
  /* IMPLEMENT: addition */
}
      | MINUS expr 
{
  /* IMPLEMENT: negation */
}
      | expr MULTIPLY expr
{
  /* IMPLEMENT: multiply */
}
      | expr DIVIDE expr
{
  /* IMPLEMENT: divide */
}
      | NUM
{ 
  /* IMPLEMENT: constant */
}
      | ID
{
  /* IMPLEMENT: get reference to function parameter
     Hint: LLVMGetParam(...)
   */
}
      | TMP
{
  /* IMPLEMENT: get expression associated with TMP */
}
;

%%


void initialize()
{
  /* IMPLEMENT: add something here if needed */
}

int yyerror(const char *msg)
{
  printf("%s",msg);
  return 0;
}
