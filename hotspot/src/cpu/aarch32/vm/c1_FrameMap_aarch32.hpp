/*
 * Copyright (c) 1999, 2012, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, Red Hat Inc. All rights reserved.
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
// This file is a derivative work resulting from (and including) modifications
// made by Azul Systems, Inc.  The dates of such changes are 2013-2016.
// Copyright 2013-2016 Azul Systems, Inc.  All Rights Reserved.
//
// Please contact Azul Systems, 385 Moffett Park Drive, Suite 115, Sunnyvale,
// CA 94089 USA or visit www.azul.com if you need additional information or
// have any questions.

#ifndef CPU_AARCH32_VM_C1_FRAMEMAP_AARCH32_HPP
#define CPU_AARCH32_VM_C1_FRAMEMAP_AARCH32_HPP

// The following schema visualizes how a C1 frame looks like on AArch32.
// It corresponds to the case of an unextended frame. Each line of text
// represents one 4-byte slot. Every monitor takes two slots. Positions of
// incoming arguments are determined by the Java calling convention. Spill
// area and monitor area are not required to be 8-byte aligned. The slot
// for deoptimization support is used by frame::deoptimize() method to save
// the original pc before patching in the new one.
//
// When LIR_Opr's reference stack slots, they use virtual stack slot indices.
// They are mapped to the real stack slots by FrameMap::sp_offset_for_slot()
// and FrameMap::sp_offset_for_double_slot() methods. The first _argcount
// virtual stack slots correspond to the real stack slots occupied by the
// incoming arguments. Their mapping is defined by _argument_locations array
// (which is filled in by applying the Java calling convention). All other
// virtual stack slots correspond to spill slots.
//
// Higher addresses
//                  |              incoming              |      virtual stack slots
//                  |                                    |      [0 ... _arg_count - 1]
//                  |             arguments              |
//                  |====================================|----X- 8-byte aligned
//                  |            previous lr             |   /|\ address
//         rfp ===> |------------------------------------|    |
//                  |            previous rfp            |    |
//                  |====================================|    |
//                  |     alignment slot (if needed)     |    |
//                  |====================================|    |
//                  |  slot for deoptimization support   |    |
//                  |====================================|    |
//                  | monitor [_num_monitors - 1] object |    |
//                  |                                    |    |
//                  |  monitor [_num_monitors - 1] lock  |    |
//                  |------------------------------------|    |
//                  |                                    |    |
// Direction of     |                ...                 |    | _framesize
// stack growth     |                                    |    | slots
//      |           |------------------------------------|    |
//      V           |         monitor [0] object         |    |
//                  |                                    |    |
//                  |          monitor [0] lock          |    |
//                  |====================================|    |
//                  |    spill slot [_num_spills - 1]    |    | virtual stack slot
//                  |------------------------------------|    | [_arg_count + _num_spills - 1]
//                  |                ...                 |    | ...
//                  |------------------------------------|    |
//                  |           spill slot [0]           |    | virtual stack slot
//                  |====================================|    | [_arg_count]
//                  |     reserved argument area for     |    |
//                  |                ...                 |    |
//                  |  outgoing calls (8-byte aligned)   |   \|/
//          sp ===> |====================================|----X- 8-byte aligned
//                  |                                    |       address
// Lower addresses

 public:
  enum {
    first_available_sp_in_frame = 0,
    frame_pad_in_bytes = 8
  };

 public:
  static LIR_Opr r0_opr;
  static LIR_Opr r1_opr;
  static LIR_Opr r2_opr;
  static LIR_Opr r3_opr;
  static LIR_Opr r4_opr;
  static LIR_Opr r5_opr;
  static LIR_Opr r6_opr;
  static LIR_Opr r7_opr;
  static LIR_Opr r8_opr;
  static LIR_Opr r9_opr;
  static LIR_Opr r10_opr;
  static LIR_Opr r11_opr;
  static LIR_Opr r12_opr;
  static LIR_Opr r13_opr;
  static LIR_Opr r14_opr;
  static LIR_Opr r15_opr;

  static LIR_Opr r0_oop_opr;
  static LIR_Opr r1_oop_opr;
  static LIR_Opr r2_oop_opr;
  static LIR_Opr r3_oop_opr;
  static LIR_Opr r4_oop_opr;
  static LIR_Opr r5_oop_opr;
  static LIR_Opr r6_oop_opr;
  static LIR_Opr r7_oop_opr;
  static LIR_Opr r8_oop_opr;
  static LIR_Opr r9_oop_opr;
  static LIR_Opr r10_oop_opr;
  static LIR_Opr r11_oop_opr;
  static LIR_Opr r12_oop_opr;
  static LIR_Opr r13_oop_opr;
  static LIR_Opr r14_oop_opr;
  static LIR_Opr r15_oop_opr;

  static LIR_Opr r0_metadata_opr;
  static LIR_Opr r1_metadata_opr;
  static LIR_Opr r2_metadata_opr;
  static LIR_Opr r3_metadata_opr;
  static LIR_Opr r4_metadata_opr;
  static LIR_Opr r5_metadata_opr;

  static LIR_Opr sp_opr;
  static LIR_Opr receiver_opr;

  static LIR_Opr rscratch1_opr;
  static LIR_Opr rscratch2_opr;
  static LIR_Opr rscratch_long_opr;

  static LIR_Opr long0_opr;
  static LIR_Opr long1_opr;
  static LIR_Opr long2_opr;
  static LIR_Opr fpu0_float_opr;
  static LIR_Opr fpu0_double_opr;

  static LIR_Opr as_long_opr(Register r1, Register r2) {
    return LIR_OprFact::double_cpu(cpu_reg2rnr(r1), cpu_reg2rnr(r2));
  }
  static LIR_Opr as_pointer_opr(Register r) {
    return LIR_OprFact::single_cpu(cpu_reg2rnr(r));
  }

  static VMReg fpu_regname(int n);

  static bool is_caller_save_register(LIR_Opr opr) {
    // On AArch32, unlike on SPARC, we never explicitly request the C1 register
    // allocator to allocate a callee-saved register. Since the only place this
    // method is called is the assert in LinearScan::color_lir_opr(), we can
    // safely just always return true here.
    return true;
  }
  static int nof_caller_save_cpu_regs() {
    return pd_nof_caller_save_cpu_regs_frame_map;
  }
  static int last_cpu_reg() {
    return pd_last_cpu_reg;
  }

#endif // CPU_AARCH32_VM_C1_FRAMEMAP_AARCH32_HPP
