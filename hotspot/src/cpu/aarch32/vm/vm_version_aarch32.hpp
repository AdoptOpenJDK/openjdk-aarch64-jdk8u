/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, Red Hat Inc. All rights reserved.
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

#ifndef CPU_AARCH32_VM_VM_VERSION_AARCH32_HPP
#define CPU_AARCH32_VM_VM_VERSION_AARCH32_HPP

#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"

enum ProcessorFeatures {
  FT_NONE = 0,
  FT_HW_DIVIDE = 1,
  FT_VFPV2 = 2,
  FT_VFPV3 = 4,
  FT_ARMV7 = 8,
  FT_ARMV6T2 = 16,
  FT_ARMV6K = 32,
  FT_SINGLE_CORE = 64,
  FT_AdvSIMD = 128,
  FT_CRC32 = 256,
  FT_ALL = 0xffff
};

class VM_Version : public Abstract_VM_Version {
 public:
  // Processor feature lookup.

  enum {
    CPU_ARM       = 'A',
    CPU_BROADCOM  = 'B',
    CPU_CAVIUM    = 'C',
    CPU_DEC       = 'D',
    CPU_INFINEON  = 'I',
    CPU_MOTOROLA  = 'M',
    CPU_NVIDIA    = 'N',
    CPU_AMCC      = 'P',
    CPU_QUALCOM   = 'Q',
    CPU_MARVELL   = 'V',
    CPU_INTEL     = 'i',
  } cpuFamily;

  // Initialization
  static void initialize();

 private:
  static enum ProcessorFeatures _features;
  static const char* _cpu_features;
    static volatile bool _is_determine_features_test_running;

  static void get_processor_features();
  static bool identify_procline(const char *tag, char **line);

 public:
  static enum ProcessorFeatures features() {
    return _features;
  }
    static void features(ProcessorFeatures f) {
      _features = f;
    }
  static const char* cpu_features() { return _cpu_features; }

    static bool is_determine_features_test_running() { return _is_determine_features_test_running; }
};

#ifdef HARD_FLOAT_CC
inline const bool hasFPU(void) { return true; }
#else
inline bool hasFPU(void) { return (UseFPU); }
#endif


#endif // CPU_AARCH32_VM_VM_VERSION_AARCH32_HPP
