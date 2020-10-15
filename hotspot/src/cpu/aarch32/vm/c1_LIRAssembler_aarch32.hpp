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

#ifndef CPU_X86_VM_C1_LIRASSEMBLER_AARCH32_HPP
#define CPU_X86_VM_C1_LIRASSEMBLER_AARCH32_HPP

#include "assembler_aarch32.hpp"


 private:

  int array_element_size(BasicType type) const;

  // helper functions which checks for overflow and sets bailout if it
  // occurs.  Always returns a valid embeddable pointer but in the
  // bailout case the pointer won't be to unique storage.
  address float_constant(float f);
  address double_constant(double d);

  Address as_Address(LIR_Address* addr, Register tmp, Address::InsnDataType type);
  Address as_Address_hi(LIR_Address* addr, Address::InsnDataType type);
  Address as_Address_lo(LIR_Address* addr, Address::InsnDataType type);

  Address as_Address(LIR_Address* addr, Address::InsnDataType type) {
    return as_Address(addr, rscratch1, type);
  }


  // Record the type of the receiver in ReceiverTypeData
  void type_profile_helper(Register mdo,
                           ciMethodData *md, ciProfileData *data,
                           Register recv, Label* update_done);
  void add_debug_info_for_branch(address adr, CodeEmitInfo* info);

  void casw(Register addr, Register newval, Register cmpval, Register result);
  void casl(Register addr, Register newval_lo, Register newval_hi,
            Register cmpval_lo,  Register cmpval_hi,
            Register tmp_lo, Register tmp_hi, Register result);

  FloatRegister as_float_reg(LIR_Opr doubleReg);

  static const int max_tableswitches = 20;
  struct tableswitch switches[max_tableswitches];
  int tableswitch_count;

  void init() { tableswitch_count = 0; }

  void deoptimize_trap(CodeEmitInfo *info);

  // remap input register (*s1 or *s2) to a temp one if it is at the same time
  // used a result register (d) of a preceeding operation (so otherwise its
  // contents gets effectively corrupt)
  void check_register_collision(Register d, Register *s1, Register *s2 = NULL, Register tmp = rscratch1);

public:

  void store_parameter(Register r, int offset_from_sp_in_words);
  void store_parameter(jint c,     int offset_from_sp_in_words);
  void store_parameter(jobject c,  int offset_from_sp_in_words);

enum { call_stub_size = 12 * NativeInstruction::arm_insn_sz,
       exception_handler_size = DEBUG_ONLY(1*K) NOT_DEBUG(175),
       deopt_handler_size = 7 * NativeInstruction::arm_insn_sz };

#endif // CPU_X86_VM_C1_LIRASSEMBLER_AARCH32_HPP
