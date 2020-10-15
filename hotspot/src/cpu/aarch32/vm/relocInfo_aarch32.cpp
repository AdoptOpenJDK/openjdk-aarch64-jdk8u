/*
 * Copyright (c) 1998, 2011, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "code/relocInfo.hpp"
#include "nativeInst_aarch32.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/safepoint.hpp"

void Relocation::pd_set_data_value(address x, intptr_t o, bool verify_only) {

  if (NativeFarLdr::is_at(addr())) {
    NativeFarLdr *nal = NativeFarLdr::from(addr());
    address const_addr = NULL;
    switch(type()) {
    case relocInfo::oop_type:
      const_addr = (address)code()->oop_addr_at(((oop_Relocation *)this)->oop_index());
      assert(*(address*)const_addr == x, "error in memory relocation");
      break;
    case relocInfo::section_word_type:
      const_addr = ((section_word_Relocation*)this)->target();
      assert(const_addr == x, "error in memory relocation");
      break;
    default:
      ShouldNotReachHere();
    }
    assert(const_addr, "should not be NULL");
    if (verify_only) {
      guarantee(nal->data_addr() == (intptr_t*) const_addr, "instructions must match");
      return;
    }
    nal->set_data_addr((intptr_t*) const_addr);
  } else {
    NativeMovConstReg *nm = NativeMovConstReg::from(addr());
    if (verify_only) {
      guarantee(nm->data() == (intptr_t) x, "instructions must match");
      return;
    }
    nm->set_data((intptr_t) x);
  }
}

address Relocation::pd_call_destination(address orig_addr) {
  intptr_t adj = 0;
  if (orig_addr != NULL) {
    // We just moved this call instruction from orig_addr to addr().
    // This means its target will appear to have grown by addr() - orig_addr.
    adj = -( addr() - orig_addr );
  }

  NativeInstruction *ni = NativeInstruction::from(addr());

  // Checking from shortest encoding size to longets,
  // to avoid access beyond CodeCache boundary
  if (NativeImmCall::is_at(addr())) {
    return NativeImmCall::from(addr())->destination() + adj;
  } else if (NativeImmJump::is_at(addr())) {
    return NativeImmJump::from(addr())->destination() + adj;
  } else if (NativeCall::is_at(addr())) {
    return NativeCall::from(addr())->destination();
  } else if (NativeJump::is_at(addr())) {
    return NativeJump::from(addr())->jump_destination();
  }

  ShouldNotReachHere();
}

void Relocation::pd_set_call_destination(address x) {
  assert(addr() != x, "call instruction in an infinite loop"); // FIXME what's wrong to _generate_ loop?
  NativeInstruction *ni = NativeInstruction::from(addr());

  // Checking from shortest encoding size to longets,
  // to avoid access beyond CodeCache boundary
  if (NativeImmCall::is_at(addr())) {
    NativeImmCall::from(addr())->set_destination(x);
  } else if (NativeImmJump::is_at(addr())) {
    NativeImmJump::from(addr())->set_destination(x);
  } else if (NativeCall::is_at(addr())) {
    NativeCall::from(addr())->set_destination(x);
  } else if (NativeJump::is_at(addr())) {
    NativeJump::from(addr())->set_jump_destination(x);
  } else {
    ShouldNotReachHere();
  }

  assert(pd_call_destination(addr()) == x, "fail in reloc");
}

address* Relocation::pd_address_in_code() {
  ShouldNotCallThis();
  return NULL;
}

address Relocation::pd_get_address_from_code() {
  ShouldNotCallThis();
  return NULL;
}

void poll_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest) {
  NativeInstruction *ni = NativeInstruction::from(addr());
  if (ni->is_mov_const_reg()) {
    address old_addr = old_addr_for(addr(), src, dest);
    NativeMovConstReg *nm2 = NativeMovConstReg::from(old_addr);
    NativeMovConstReg::from(addr())->set_data(nm2->data());
  }
}

void poll_return_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest)  {
  NativeInstruction *ni = NativeInstruction::from(addr());
  if (ni->is_mov_const_reg()) {
    address old_addr = old_addr_for(addr(), src, dest);
    NativeMovConstReg *nm2 = NativeMovConstReg::from(old_addr);
    NativeMovConstReg::from(addr())->set_data(nm2->data());
  }
}

void metadata_Relocation::pd_fix_value(address x) {
  if (NativeFarLdr::is_at(addr())) {
    NativeFarLdr *nal = NativeFarLdr::from(addr());
    address const_addr = (address)code()->metadata_addr_at(((metadata_Relocation *)this)->metadata_index());
    nal->set_data_addr((intptr_t*) const_addr);
  }
}
