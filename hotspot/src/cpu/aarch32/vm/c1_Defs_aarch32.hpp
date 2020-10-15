/*
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH32_VM_C1_DEFS_AARCH32_HPP
#define CPU_AARCH32_VM_C1_DEFS_AARCH32_HPP

// Native word offsets from memory address (little endian format)
enum {
  pd_lo_word_offset_in_bytes = 0,
  pd_hi_word_offset_in_bytes = BytesPerWord
};

// TODO: We should understand what values are correct for the following 3 flags
// relevant to floating point operations:
// - UseSSE
//   Highest supported SSE instruction set on x86/x64. I believe we should
//   set it to 0 in VM_Version::initialize(), like other non-x86 ports do.
// - RoundFPResults
//   Indicates whether rounding is needed for floating point results
// - pd_strict_fp_requires_explicit_rounding
//   The same as above but for the strictfp mode

// Explicit rounding operations are not required to implement the strictfp mode
enum {
  pd_strict_fp_requires_explicit_rounding = false
};

// Registers
enum {
  // Number of registers used during code emission
  pd_nof_cpu_regs_frame_map = RegisterImpl::number_of_registers,
  pd_nof_fpu_regs_frame_map = FloatRegisterImpl::number_of_registers,

  // Number of registers killed by calls
  pd_nof_caller_save_cpu_regs_frame_map = 8,

  pd_nof_caller_save_fpu_regs_frame_map = pd_nof_fpu_regs_frame_map,
  // The following two constants need to be defined since they are referenced
  // from c1_FrameMap.hpp, but actually they are never used, so can be set to
  // arbitrary values.
  pd_nof_cpu_regs_reg_alloc = -1,
  pd_nof_fpu_regs_reg_alloc = -1,

  // All the constants below are used by linear scan register allocator only.
  // Number of registers visible to register allocator
  pd_nof_cpu_regs_linearscan = pd_nof_cpu_regs_frame_map,
  pd_nof_fpu_regs_linearscan = pd_nof_fpu_regs_frame_map,
  pd_nof_xmm_regs_linearscan = 0,

  // Register allocator specific register numbers corresponding to first/last
  // CPU/FPU registers available for allocation
  pd_first_cpu_reg = 0,
  pd_last_cpu_reg = 7,
  pd_first_fpu_reg = pd_nof_cpu_regs_frame_map,
  pd_last_fpu_reg = pd_first_fpu_reg + pd_nof_fpu_regs_frame_map - 1,
  // Register allocator specific register numbers corresponding to first/last
  // CPU/FPU callee-saved registers. These constants are used in
  // LinearScan::is_caller_save() only.
  pd_first_callee_saved_cpu_reg = 4,
  pd_last_callee_saved_cpu_reg = 11,
  pd_first_callee_saved_fpu_reg = pd_first_fpu_reg + pd_nof_fpu_regs_frame_map/2,
  pd_last_callee_saved_fpu_reg = pd_first_fpu_reg + pd_nof_fpu_regs_frame_map - 1
};

// This flag must be in sync with how the floating point registers are stored
// on the stack by RegisterSaver::save_live_registers() method
// (sharedRuntime_aarch32.cpp) and save_live_registers() function
// (c1_Runtime1_aarch32.cpp). On AArch32 the floating point registers keep
// floats and doubles in their native form. No float to double conversion
// happens when the registers are stored on the stack. This is opposite to
// what happens on x86, where the FPU stack registers are 80 bits wide,
// and storing them in either 4 byte or 8 byte stack slot is a conversion
// operation.
enum {
  pd_float_saved_as_double = false
};

#endif // CPU_AARCH32_VM_C1_DEFS_AARCH32_HPP
