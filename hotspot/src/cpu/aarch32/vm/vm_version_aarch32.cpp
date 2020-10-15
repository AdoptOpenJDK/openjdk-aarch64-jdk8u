/*
 * Copyright (c) 1997, 2012, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, Red Hat Inc. All rights reserved.
 * Copyright (c) 2015, Linaro Ltd. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/java.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "vm_version_aarch32.hpp"
#ifdef TARGET_OS_FAMILY_linux
# include "os_linux.inline.hpp"
#endif
#include "compiler/disassembler.hpp"

enum ProcessorFeatures VM_Version::_features = FT_NONE;
const char* VM_Version::_cpu_features = "";

static BufferBlob* stub_blob;
static const int stub_size = 550;
volatile bool VM_Version::_is_determine_features_test_running = false;

extern "C" {
  typedef void (*getPsrInfo_stub_t)(void*);
}
static getPsrInfo_stub_t getPsrInfo_stub = NULL;


bool VM_Version::identify_procline(const char *tag, char **line) {
  char *i = *line;
  const char EOT = '\t', EOT2 = ':'; // the longest has no tabs
  for(; '\0' != *i && EOT != *i && EOT2 != *i; i++);
  if(EOT == *i || EOT2 == *i) {
    if(!memcmp(*line, tag, i - *line)) {
      for(i++; (EOT == *i || EOT2 == *i || ' ' == *i) && '\0' != *i; i++);
      if('\0' != *i) {
        *line = i;
        return true;
      }
    }
  }
  return false;
}

void VM_Version::get_processor_features() {
  _supports_cx8 = true;
  _supports_atomic_getset4 = true;
  _supports_atomic_getadd4 = true;
  _supports_atomic_getset8 = true;
  _supports_atomic_getadd8 = true;

  if (FLAG_IS_DEFAULT(AllocatePrefetchDistance))
    FLAG_SET_DEFAULT(AllocatePrefetchDistance, 256);
  if (FLAG_IS_DEFAULT(AllocatePrefetchStepSize))
    FLAG_SET_DEFAULT(AllocatePrefetchStepSize, 64);
  FLAG_SET_DEFAULT(PrefetchScanIntervalInBytes, 256);
  FLAG_SET_DEFAULT(PrefetchFieldsAhead, 256);
  FLAG_SET_DEFAULT(PrefetchCopyIntervalInBytes, 256);

  enum ProcessorFeatures f = FT_NONE;

  // Allocate space for the code.
  const int code_size = 10 * Assembler::instruction_size;
  ResourceMark rm;
  CodeBuffer cb("detect_cpu_features", code_size, 0);
  MacroAssembler* a = new MacroAssembler(&cb);
  jlong test_area;

  // Must be set to true so we can generate the test code.
  _features = FT_ALL;
  // Emit code.
  uint32_t *const code = (uint32_t *)a->pc();
  void (*test)(address addr, uintptr_t offset)=(void(*)(address addr, uintptr_t nonzero))(void *)code;

  a->udiv(r3, r2, r1);     // FT_HW_DIVIDE
  a->bfc(r1, 1, 1);        // FT_ARMV6T2
  a->vneg_f64(d0, d0);     // FT_VFPV2
  a->vmov_f64(d0, 1.);     // FT_VFPV3
  a->dmb(Assembler::ISH);  // FT_ARMV7
  a->ldrexd(r2, r0);       // FT_ARMV6K
  a->vmov_f64(d0, 0.0);    // FT_AdvSIMD
  a->crc32b(r3, r2, r1);   // FT_CRC32
  a->b(lr);

  uint32_t *const code_end = (uint32_t *)a->pc();
  a->flush();
  _features = FT_NONE;

  // Print the detection code.
  if (PrintAssembly) {
    ttyLocker ttyl;
    tty->print_cr("Decoding cpu-feature detection stub at " INTPTR_FORMAT " before execution:", p2i(code));
    Disassembler::decode((u_char*)code, (u_char*)code_end, tty);
  }
  // Execute code. Illegal instructions will be replaced by 0 in the signal handler.
  VM_Version::_is_determine_features_test_running = true;
  (*test)((address)&test_area, 1);
  VM_Version::_is_determine_features_test_running = false;

  uint32_t *insn = code;
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_HW_DIVIDE);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_ARMV6T2);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_VFPV2);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_VFPV3);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_ARMV7);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_ARMV6K);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_AdvSIMD);
  if (*insn++ != Assembler::nop_insn) f = (ProcessorFeatures) (f | FT_CRC32);

  int ncores = 0, cpu, variant, model, revision;
  char buf[2048], *i;
  if (FILE * fp = fopen("/proc/cpuinfo", "r")) {
    while ((i = fgets(buf, 2048, fp))) {
      if (identify_procline("processor", &i)) {
        ncores++;
      } else if (identify_procline("CPU implementer", &i)) {
        cpu = strtol(i, NULL, 0);
      } else if (identify_procline("CPU variant", &i)) {
        variant = strtol(i, NULL, 0);
      } else if (identify_procline("CPU part", &i)) {
        model = strtol(i, NULL, 0);
      } else if (identify_procline("CPU revision", &i)) {
        revision = strtol(i, NULL, 0);
      }
    }
    fclose(fp);
  }
  if (1 == ncores) {
    f = (ProcessorFeatures) (f | FT_SINGLE_CORE);
  }
  if (FLAG_IS_DEFAULT(UseCRC32Intrinsics)) {
    UseCRC32Intrinsics = true;
  }
  if ((f & FT_AdvSIMD) && FLAG_IS_DEFAULT(UseNeon) && (model & ~0x0f0) >= 0xc08) {
    UseNeon = true;
  }
  _features = f;
  sprintf(buf, "0x%02x:0x%x:0x%03x:%d", cpu, variant, model, revision);
  _cpu_features = os::strdup(buf);

#ifdef COMPILER2
  if (UseMultiplyToLenIntrinsic) {
    if (!FLAG_IS_DEFAULT(UseMultiplyToLenIntrinsic)) {
      warning("multiplyToLen intrinsic is not available in 32-bit VM");
    }
    FLAG_SET_DEFAULT(UseMultiplyToLenIntrinsic, false);
  }
#endif // COMPILER2

  if (FLAG_IS_DEFAULT(UseSIMDForMemoryOps) && (f & (FT_VFPV2 | FT_AdvSIMD))) {
    FLAG_SET_DEFAULT(UseSIMDForMemoryOps, true);
  }

/*  if (FLAG_IS_DEFAULT(UseBarriersForVolatile)) {
    UseBarriersForVolatile = (_cpuFeatures & CPU_DMB_ATOMICS) != 0;
  }*/

  /*if(!(f & FT_ARMV7) && FLAG_IS_DEFAULT(UseMembar)) {
  UseMembar = false;
  } else if(UseMembar) {
  fprintf(stderr, "Unable to use memory barriers as not on ARMv7, disabling.\n");
  UseMembar = false;
  }*/

  if (UseAES) {
    warning("AES instructions are not implemented on this CPU");
    FLAG_SET_DEFAULT(UseAES, false);
  }
  if (UseAESIntrinsics) {
    warning("AES intrinsics are not implemented on this CPU");
    FLAG_SET_DEFAULT(UseAESIntrinsics, false);
  }

  if (UseSHA) {
    warning("SHA instructions are not available on this CPU");
    FLAG_SET_DEFAULT(UseSHA, false);
  }
  if (UseSHA1Intrinsics || UseSHA256Intrinsics || UseSHA512Intrinsics) {
    warning("SHA intrinsics are not available on this CPU");
    FLAG_SET_DEFAULT(UseSHA1Intrinsics, false);
    FLAG_SET_DEFAULT(UseSHA256Intrinsics, false);
    FLAG_SET_DEFAULT(UseSHA512Intrinsics, false);
  }

}

void VM_Version::initialize() {
  ResourceMark rm;

  stub_blob = BufferBlob::create("getPsrInfo_stub", stub_size);
  if (stub_blob == NULL) {
    vm_exit_during_initialization("Unable to allocate getPsrInfo_stub");
  }

  get_processor_features();

#ifndef HARD_FLOAT_CC
  if( !(VM_Version::features() & (FT_VFPV2 | FT_VFPV3)) ) {
      if(FLAG_IS_CMDLINE(UseFPU)) {
          warning("FPU is not present on this core");
      }
      FLAG_SET_DEFAULT(UseFPU, false);
  }
#endif
}
