//===-- LC3MCAsmInfo.h - LC3 asm properties ----------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the LC3MCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LC3TARGETASMINFO_H
#define LC3TARGETASMINFO_H

#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class StringRef;
  class Target;

  class LC3MCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit LC3MCAsmInfo(StringRef TT);
  };

} // namespace llvm

#endif
