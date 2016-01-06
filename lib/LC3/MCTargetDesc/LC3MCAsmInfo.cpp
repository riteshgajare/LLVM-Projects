//===-- LC3MCAsmInfo.cpp - LC3 asm properties -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations of the LC3MCAsmInfo properties.
//
//===----------------------------------------------------------------------===//

#include "LC3MCAsmInfo.h"
#include "llvm/ADT/Triple.h"

using namespace llvm;

void LC3MCAsmInfo::anchor() { }

LC3MCAsmInfo::LC3MCAsmInfo(StringRef TT) {
  IsLittleEndian = true;
  Triple TheTriple(TT);

  Data16bitsDirective = "\t.half\t";
  Data32bitsDirective = "\t.word\t";
  Data64bitsDirective = 0;  // .xword is only supported by V9.
  ZeroDirective = "\t.skip\t";
  CommentString = "!";
  
  SupportsDebugInformation = true;
  
  WeakRefDirective = "\t.weak\t";

  PrivateGlobalPrefix = ".L";
}


