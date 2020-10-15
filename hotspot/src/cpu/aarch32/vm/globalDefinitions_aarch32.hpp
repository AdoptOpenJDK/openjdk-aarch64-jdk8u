/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH32_VM_GLOBALDEFINITIONS_AARCH32_HPP
#define CPU_AARCH32_VM_GLOBALDEFINITIONS_AARCH32_HPP

// __ARM_PCS_VFP indicates that gcc runs with "-mfloat-abi=hard" option.
// This option allows generation of floating point instructions and enforces
// usage of FPU-specific calling conventions.
#ifdef __ARM_PCS_VFP
#define HARD_FLOAT_CC
#endif // __ARM_PCS_VFP

// If changing this please be sure to review all code which saves the registers
// and the corresponding register maps to ensure that the respective frame
// sizes are multiple of this new value
const int StackAlignmentInBytes = 8;

// Indicates whether the C calling conventions require that
// 32-bit integer argument values are properly extended to 64 bits.
// If set, SharedRuntime::c_calling_convention() must adapt
// signatures accordingly.
const bool CCallingConventionRequiresIntsAsLongs = false;

#define SUPPORTS_NATIVE_CX8

// The maximum B/BL offset range on AArch32 is 32MB.
#undef CODE_CACHE_DEFAULT_LIMIT
#define CODE_CACHE_DEFAULT_LIMIT (32*M)

// According to the ARMv8 ARM, "Concurrent modification and execution
// of instructions can lead to the resulting instruction performing
// any behavior that can be achieved by executing any sequence of
// instructions that can be executed from the same Exception level,
// except where the instruction before modification and the
// instruction after modification is a B, BL, NOP, BKPT, SVC, HVC, or
// SMC instruction."
//
// This makes the games we play when patching difficult, so when we
// come across an access that needs patching we deoptimize.  There are
// ways we can avoid this, but these would slow down C1-compiled code
// in the default case.  We could revisit this decision if we get any
// evidence that it's worth doing.
#define DEOPTIMIZE_WHEN_PATCHING

#endif // CPU_AARCH32_VM_GLOBALDEFINITIONS_AARCH32_HPP
