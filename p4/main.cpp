#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/FileSystem.h"
//#include "llvm/IR/LegacyPassManager.h"
#include "llvm/PassManager.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/ManagedStatic.h"
#include <memory>
#include <algorithm>

#include "llvm/Support/SourceMgr.h"
#include <memory>
#include <algorithm>
using namespace llvm;

//#define UseC

#ifdef USE_C
#include "summary.h"
#else
#include "Summary.hpp"
#endif

#include "CSE.h"

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string>
OutputFilename(cl::Positional, cl::desc("<output bitcode>"), cl::init("out.bc"));

static cl::opt<bool>
Mem2Reg("mem2reg",
  cl::desc("Perform memory to register promotion before CSE."),
  cl::init(false));


static cl::opt<bool>
NoCSE("no-cse",
  cl::desc("Do not perform CSE Optimization."),
  cl::init(false));

static cl::opt<bool>
DumpSummary("summary",
  cl::desc("Dump summary stats."),
  cl::init(false));



static inline std::string
GetFileNameRoot(const std::string &InputFilename) {
  std::string IFN = InputFilename;
  std::string outputFilename;
  int Len = IFN.length();
  if ((Len > 2) &&
      IFN[Len-3] == '.' &&
      ((IFN[Len-2] == 'b' && IFN[Len-1] == 'c') ||
       (IFN[Len-2] == 'l' && IFN[Len-1] == 'l'))) {
    outputFilename = std::string(IFN.begin(), IFN.end()-3); // s/.bc/.s/
  } else {
    outputFilename = IFN;
  }
  return outputFilename;
}


int
main (int argc, char ** argv)
{
  cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");

  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
  LLVMContext &Context = getGlobalContext();

  // LLVM idiom for constructing output file.
  std::unique_ptr<tool_output_file> Out;  
  std::string ErrorInfo;
  std::error_code EC;
  Out.reset(new tool_output_file(OutputFilename.c_str(), EC,
  				 sys::fs::F_None));

  SMDiagnostic Err;
  std::unique_ptr<Module> M;
  M = parseIRFile(InputFilename, Err, getGlobalContext());

  if (M.get() == 0) {
    Err.print(argv[0], errs());
    return 1;
  }

  {
    legacy::PassManager Passes;
    Passes.add(createVerifierPass());
    if (Mem2Reg) 
      {
	Passes.add(createPromoteMemoryToRegisterPass());
      }
    //Passes.add(createDeadCodeEliminationPass());
    Passes.run(*M.get());
  }

  if (!NoCSE)
#ifdef USE_C
    LLVMCommonSubexpressionElimination(wrap(M.get()));
#else
    LLVMCommonSubexpressionElimination_Cpp(M.get());
#endif

 if (DumpSummary)
   {
      char filename[1024]; 
      sprintf(filename,"%s.stats",OutputFilename.c_str());
#ifdef USE_C
      Summarize(wrap(M.get()),"p4",filename);
#else
      Summarize(M.get(),"p4",filename);
#endif
   }
   {
     legacy::PassManager Passes;
     Passes.add(createDeadCodeEliminationPass());
     Passes.add(createCFGSimplificationPass()); 
     Passes.run(*M.get());
   }
   
   WriteBitcodeToFile(M.get(),Out->os());
   Out->keep();
   
   return 0;
}
