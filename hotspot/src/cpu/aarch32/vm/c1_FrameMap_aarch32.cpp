/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIR.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch32.inline.hpp"

LIR_Opr FrameMap::r0_opr;
LIR_Opr FrameMap::r1_opr;
LIR_Opr FrameMap::r2_opr;
LIR_Opr FrameMap::r3_opr;
LIR_Opr FrameMap::r4_opr;
LIR_Opr FrameMap::r5_opr;
LIR_Opr FrameMap::r6_opr;
LIR_Opr FrameMap::r7_opr;
LIR_Opr FrameMap::r8_opr;
LIR_Opr FrameMap::r9_opr;
LIR_Opr FrameMap::r10_opr;
LIR_Opr FrameMap::r11_opr;
LIR_Opr FrameMap::r12_opr;
LIR_Opr FrameMap::r13_opr;
LIR_Opr FrameMap::r14_opr;
LIR_Opr FrameMap::r15_opr;

LIR_Opr FrameMap::r0_oop_opr;
LIR_Opr FrameMap::r1_oop_opr;
LIR_Opr FrameMap::r2_oop_opr;
LIR_Opr FrameMap::r3_oop_opr;
LIR_Opr FrameMap::r4_oop_opr;
LIR_Opr FrameMap::r5_oop_opr;
LIR_Opr FrameMap::r6_oop_opr;
LIR_Opr FrameMap::r7_oop_opr;
LIR_Opr FrameMap::r8_oop_opr;
LIR_Opr FrameMap::r9_oop_opr;
LIR_Opr FrameMap::r10_oop_opr;
LIR_Opr FrameMap::r11_oop_opr;
LIR_Opr FrameMap::r12_oop_opr;
LIR_Opr FrameMap::r13_oop_opr;
LIR_Opr FrameMap::r14_oop_opr;
LIR_Opr FrameMap::r15_oop_opr;

LIR_Opr FrameMap::r0_metadata_opr;
LIR_Opr FrameMap::r1_metadata_opr;
LIR_Opr FrameMap::r2_metadata_opr;
LIR_Opr FrameMap::r3_metadata_opr;
LIR_Opr FrameMap::r4_metadata_opr;
LIR_Opr FrameMap::r5_metadata_opr;

LIR_Opr FrameMap::sp_opr;
LIR_Opr FrameMap::receiver_opr;

LIR_Opr FrameMap::rscratch1_opr;
LIR_Opr FrameMap::rscratch2_opr;
LIR_Opr FrameMap::rscratch_long_opr;

LIR_Opr FrameMap::long0_opr;
LIR_Opr FrameMap::long1_opr;
LIR_Opr FrameMap::long2_opr;
LIR_Opr FrameMap::fpu0_float_opr;
LIR_Opr FrameMap::fpu0_double_opr;

LIR_Opr FrameMap::_caller_save_cpu_regs[] = { 0, };
LIR_Opr FrameMap::_caller_save_fpu_regs[] = { 0, };

void FrameMap::initialize() {
  assert(!_init_done, "must be called once");

  int i = 0;
  map_register(i, r0); r0_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r1); r1_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r2); r2_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r3); r3_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r4); r4_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r5); r5_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r6); r6_opr = LIR_OprFact::single_cpu(i); i++;
  map_register(i, r7); r7_opr = LIR_OprFact::single_cpu(i); i++;
  // Mapping lines in this block may be arbitrarily mixed, but all allocatable
  // registers should go above this comment, and unallocatable registers -
  // below.
  map_register(i, r8); r8_opr = LIR_OprFact::single_cpu(i); i++;   // rthread
  map_register(i, r9); r9_opr = LIR_OprFact::single_cpu(i); i++;   // rscratch1
  map_register(i, r10); r10_opr = LIR_OprFact::single_cpu(i); i++; // rmethod
  map_register(i, r11); r11_opr = LIR_OprFact::single_cpu(i); i++; // rfp
  map_register(i, r12); r12_opr = LIR_OprFact::single_cpu(i); i++; // rscratch2
  map_register(i, r13); r13_opr = LIR_OprFact::single_cpu(i); i++; // sp
  map_register(i, r14); r14_opr = LIR_OprFact::single_cpu(i); i++; // lr
  map_register(i, r15); r15_opr = LIR_OprFact::single_cpu(i); i++; // r15_pc

  // This flag must be set after all integer registers are mapped but before
  // the first use of as_*_opr() methods.
  _init_done = true;

  r0_oop_opr = as_oop_opr(r0);
  r1_oop_opr = as_oop_opr(r1);
  r2_oop_opr = as_oop_opr(r2);
  r3_oop_opr = as_oop_opr(r3);
  r4_oop_opr = as_oop_opr(r4);
  r5_oop_opr = as_oop_opr(r5);
  r6_oop_opr = as_oop_opr(r6);
  r7_oop_opr = as_oop_opr(r7);
  r8_oop_opr = as_oop_opr(r8);
  r9_oop_opr = as_oop_opr(r9);
  r10_oop_opr = as_oop_opr(r10);
  r11_oop_opr = as_oop_opr(r11);
  r12_oop_opr = as_oop_opr(r12);
  r13_oop_opr = as_oop_opr(r13);
  r14_oop_opr = as_oop_opr(r14);
  r15_oop_opr = as_oop_opr(r15);

  r0_metadata_opr = as_metadata_opr(r0);
  r1_metadata_opr = as_metadata_opr(r1);
  r2_metadata_opr = as_metadata_opr(r2);
  r3_metadata_opr = as_metadata_opr(r3);
  r4_metadata_opr = as_metadata_opr(r4);
  r5_metadata_opr = as_metadata_opr(r5);

  sp_opr = as_pointer_opr(sp);

  VMRegPair regs;
  BasicType sig_bt = T_OBJECT;
  SharedRuntime::java_calling_convention(&sig_bt, &regs, 1, true);
  receiver_opr = as_oop_opr(regs.first()->as_Register());

  rscratch1_opr = as_opr(rscratch1);
  rscratch2_opr = as_opr(rscratch2);
  rscratch_long_opr = as_long_opr(rscratch1, rscratch2);

  long0_opr = as_long_opr(r0, r1);
  long1_opr = as_long_opr(r2, r3);
  long2_opr = as_long_opr(r4, r5);
  fpu0_float_opr = LIR_OprFact::single_fpu(0);
  fpu0_double_opr = LIR_OprFact::double_fpu(0, 1);

  _caller_save_cpu_regs[0] = r0_opr;
  _caller_save_cpu_regs[1] = r1_opr;
  _caller_save_cpu_regs[2] = r2_opr;
  _caller_save_cpu_regs[3] = r3_opr;
  _caller_save_cpu_regs[4] = r4_opr;
  _caller_save_cpu_regs[5] = r5_opr;
  _caller_save_cpu_regs[6] = r6_opr;
  _caller_save_cpu_regs[7] = r7_opr;

  for (i = 0; i < nof_caller_save_fpu_regs; i++) {
    _caller_save_fpu_regs[i] = LIR_OprFact::single_fpu(i);
  }
}

LIR_Opr FrameMap::stack_pointer() {
  return sp_opr;
}

// TODO: Make sure that neither method handle intrinsics nor compiled lambda
// forms modify sp register (i.e., vmIntrinsics::{_invokeBasic, _linkToVirtual,
// _linkToStatic, _linkToSpecial, _linkToInterface, _compiledLambdaForm})
LIR_Opr FrameMap::method_handle_invoke_SP_save_opr() {
  return LIR_OprFact::illegalOpr;
}

// Return LIR_Opr corresponding to the given VMRegPair and data type
LIR_Opr FrameMap::map_to_opr(BasicType type, VMRegPair* reg, bool) {
  LIR_Opr opr = LIR_OprFact::illegalOpr;
  VMReg r_1 = reg->first();
  VMReg r_2 = reg->second();
  if (r_1->is_stack()) {
    // Convert stack slot to sp-based address. The calling convention does not
    // count the SharedRuntime::out_preserve_stack_slots() value, so we must
    // add it in here.
    int st_off =
        (r_1->reg2stack() + SharedRuntime::out_preserve_stack_slots()) *
        VMRegImpl::stack_slot_size;
    opr = LIR_OprFact::address(new LIR_Address(sp_opr, st_off, type));
  } else if (r_1->is_Register()) {
    Register reg1 = r_1->as_Register();
#ifdef HARD_FLOAT_CC
    if (type == T_DOUBLE || type == T_FLOAT) {
        ShouldNotReachHere();
    } else
#endif
    if (type == T_LONG || type == T_DOUBLE) {
      assert(r_2->is_Register(), "wrong VMReg");
      Register reg2 = r_2->as_Register();
      opr = as_long_opr(reg1, reg2);
    } else if (type == T_OBJECT || type == T_ARRAY) {
      opr = as_oop_opr(reg1);
    } else if (type == T_METADATA) {
      opr = as_metadata_opr(reg1);
    } else {
      opr = as_opr(reg1);
    }
  } else if (r_1->is_FloatRegister()) {
    int num = r_1->as_FloatRegister()->encoding();
    if (type == T_FLOAT) {
      opr = LIR_OprFact::single_fpu(num);
    } else {
      assert(is_even(num) && r_2->as_FloatRegister()->encoding() == (num + 1),
             "wrong VMReg");
      opr = LIR_OprFact::double_fpu(num, num + 1);
    }
  } else {
    ShouldNotReachHere();
  }
  return opr;
}

// Return VMReg corresponding to the given FPU register number as it is
// encoded in LIR_Opr. The conversion is straightforward because in this
// implementation the encoding of FPU registers in LIR_Opr's is the same as
// in FloatRegister's.
VMReg FrameMap::fpu_regname(int n) {
  return as_FloatRegister(n)->as_VMReg();
}

// Check that the frame is properly addressable on the platform. The sp-based
// address of every frame slot must have the offset expressible as AArch32's
// imm12 with the separately stored sign.
bool FrameMap::validate_frame() {
  int max_offset = in_bytes(framesize_in_bytes());
  int java_index = 0;
  for (int i = 0; i < _incoming_arguments->length(); i++) {
    LIR_Opr opr = _incoming_arguments->at(i);
    if (opr->is_stack()) {
      max_offset = MAX2(_argument_locations->at(java_index), max_offset);
    }
    java_index += type2size[opr->type()];
  }
  return Assembler::is_valid_for_offset_imm(max_offset, 12);
}

Address FrameMap::make_new_address(ByteSize sp_offset) const {
  return Address(sp, in_bytes(sp_offset));
}
