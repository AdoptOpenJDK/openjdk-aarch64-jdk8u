/*
 * Copyright (c) 1997, 2015, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, 2015, Red Hat Inc. All rights reserved.
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

#include <sys/types.h>

#include "precompiled.hpp"
#include "asm/assembler.hpp"
#include "asm/assembler.inline.hpp"
#include "interpreter/interpreter.hpp"

#include "compiler/disassembler.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_aarch32.hpp"
//This ifdef was introduced so a core build can be built
#ifdef COMPILER2
#include "opto/compile.hpp"
#include "opto/node.hpp"
#endif

#include "runtime/biasedLocking.hpp"
#include "runtime/icache.hpp"
#include "runtime/interfaceSupport.hpp"
#include "runtime/sharedRuntime.hpp"

#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/g1CollectedHeap.inline.hpp"
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#include "gc_implementation/g1/heapRegion.hpp"
#include "vm_version_aarch32.hpp"
#endif

#ifdef PRODUCT
#define BLOCK_COMMENT(str) /* nothing */
#define STOP(error) stop(error)
#else
#define BLOCK_COMMENT(str) block_comment(str)
#define STOP(error) block_comment(error); stop(error)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

// FIXME This is not a nice fix, this constant was in a compiler2 header
#define MAX_stubs_size_div2 (128 / 2)
// FIXME END

// Note the corrections in the following three instructions for the PC.
// All literal modes that use the PC need to have the offset adjusted
// Patch any kind of instruction; there may be several instructions.
// Return the total length (in bytes) of the instructions.

int MacroAssembler::pd_patch_instruction_size(address branch, address target) {
  // Note the corrections
  int instructions = 1;
  long offset = target - (branch + 8); // correct for that PC = PC_this + 2 instructions
  bool add = offset >= 0;
  unsigned insn = *(unsigned*)branch;
  int opc = Instruction_aarch32::extract(insn, 27, 24);

  if(0b1010 == opc || 0b1011 == opc) {
    // Branch or branch with link
    assert(0 == (offset & 3), "not aligned correctly");
    Instruction_aarch32::spatch(branch, 23, 0, offset / 4);
  } else if (0b0011 == opc) {
    // Movw, Movt or mov, orr, orr, orr
    // patch up address load to registers (absolute address).
      instructions = patch_oop(branch, target) / NativeInstruction::arm_insn_sz;
  } else if (0b010 == (opc >> 1)) {
    // LDR, LDRB, STR, STRB
    Instruction_aarch32::patch(branch, 11, 0, uabs(offset));
    Instruction_aarch32::patch(branch, 23, 23, add);
  } else if (0b000 == (opc >> 1)) {
    // LDRH, LDRSH, LDRSB, LDRD, STRH, STRD
    offset = uabs(offset);
    Instruction_aarch32::patch(branch, 3, 0, offset & 0xf);
    Instruction_aarch32::patch(branch, 11, 8, offset >> 4);
    Instruction_aarch32::patch(branch, 23, 23, add);
  } else if (0b1101 == opc) {
    // VLDR, VSTR - NOTE VSTR(lit) is deprecated
    offset = uabs(offset);
    assert(0 == (offset & 3), "vldr, vstr can't do unaligned access");
    Instruction_aarch32::patch(branch, 7, 0, offset >> 2);
    Instruction_aarch32::patch(branch, 23, 23, add);
  } else if (0b0010 == opc) {
    // ADR
    Instruction_aarch32::patch(branch, 11, 0, encode_imm12(uabs(offset)));
    Instruction_aarch32::patch(branch, 23, 22, add ? 0b10 : 0b01 );
  } else {
    ShouldNotReachHere();
  }
  // aarch64 had something for polling page load?
  return instructions * NativeInstruction::arm_insn_sz;
}

int MacroAssembler::patch_oop(address insn_addr, address o) {
    unsigned insn = *(unsigned*)insn_addr;
    int opc = Instruction_aarch32::extract(insn, 27, 21);
    if(0b0011000 == opc) {
        //32-bit pointers, formed of a mov and a movt
        assert(nativeInstruction_at(insn_addr+4)->is_movt(), "wrong insns in patch");

        uint32_t btm = (uint32_t)o & 0xffff;
        Instruction_aarch32::patch(insn_addr, 19, 16, btm >> 12);
        Instruction_aarch32::patch(insn_addr, 11, 0, btm & 0xfff);
        uint32_t top = (uint32_t)o >> 16;
        Instruction_aarch32::patch(insn_addr + 4, 19, 16, top >> 12);
        Instruction_aarch32::patch(insn_addr + 4, 11, 0, top & 0xfff);
        return 2 * NativeInstruction::arm_insn_sz;
  } else if(0b0011101 == opc) {
    //Instead 32bit load sequence uses mov, orr, orr, orr
    assert(nativeInstruction_at(insn_addr+4 )->is_orr(), "wrong insns in patch");
    assert(nativeInstruction_at(insn_addr+8 )->is_orr(), "wrong insns in patch");
    assert(nativeInstruction_at(insn_addr+12)->is_orr(), "wrong insns in patch");
    // FIXME this could carry us outside valid memory

    uint32_t addr = (uint32_t)o;
    Instruction_aarch32::patch(insn_addr + 0,  11, 0, (0b0000 << 8) | ((addr >>  0) & 0xff));
    Instruction_aarch32::patch(insn_addr + 4,  11, 0, (0b1100 << 8) | ((addr >>  8) & 0xff));
    Instruction_aarch32::patch(insn_addr + 8,  11, 0, (0b1000 << 8) | ((addr >> 16) & 0xff));
    Instruction_aarch32::patch(insn_addr + 12, 11, 0, (0b0100 << 8) | ((addr >> 24) & 0xff));
    return 4 * NativeInstruction::arm_insn_sz;
  } else {
    ShouldNotReachHere();
  }
  return 0; //won't reach here
}

address MacroAssembler::target_addr_for_insn(address insn_addr, unsigned insn) {
  long offset = 0;
  int opc = Instruction_aarch32::extract(insn, 27, 24);

  if(0b1010 == opc || 0b1011 == opc) {
    // Branch or branch with link
    offset = Instruction_aarch32::sextract(insn, 23, 0) * 4;
  } else if (0b0011 == opc) {
    unsigned *insn_buf = (unsigned*)insn_addr;
    int opc2 = Instruction_aarch32::extract(insn, 23, 21);
    if(0b000 == opc2) {
      // movw, movt (only on newer ARMs)
      assert(nativeInstruction_at(&insn_buf[1])->is_movt(), "wrong insns in patch");
      uint32_t addr;
      addr  = Instruction_aarch32::extract(insn_buf[1], 19, 16) << 28;
      addr |= Instruction_aarch32::extract(insn_buf[1], 11, 0) << 16;
      addr |= Instruction_aarch32::extract(insn_buf[0], 19, 16) << 12;
      addr |= Instruction_aarch32::extract(insn_buf[0], 11, 0);
      return address(addr);
    } else if(0b101 == opc2) {
      // mov, orr, orr, orr
      assert(nativeInstruction_at(&insn_buf[1])->is_orr(), "wrong insns in patch");
      assert(nativeInstruction_at(&insn_buf[2])->is_orr(), "wrong insns in patch");
      assert(nativeInstruction_at(&insn_buf[3])->is_orr(), "wrong insns in patch");
      uint32_t addr;
      addr  = Assembler::decode_imm12(Instruction_aarch32::extract(insn_buf[0], 11, 0));
      addr |= Assembler::decode_imm12(Instruction_aarch32::extract(insn_buf[1], 11, 0));
      addr |= Assembler::decode_imm12(Instruction_aarch32::extract(insn_buf[2], 11, 0));
      addr |= Assembler::decode_imm12(Instruction_aarch32::extract(insn_buf[3], 11, 0));
      return address(addr);
    } else {
      ShouldNotReachHere();
    }
  } else if (0b010 == (opc >> 1)) {
    // LDR, LDRB, STR, STRB
    offset = Instruction_aarch32::extract(insn, 11, 0);
    bool add = Instruction_aarch32::extract(insn, 23, 23);
    offset = add ? offset : -offset;
  } else if (0b000 == (opc >> 1)) {
    // LDRH, LDRSH, LDRSB, LDRD, STRH, STRD
    offset = Instruction_aarch32::extract(insn, 3, 0);
    offset |= Instruction_aarch32::extract(insn, 11, 8) << 4;
    bool add = Instruction_aarch32::extract(insn, 23, 23);
    offset = add ? offset : -offset;
  } else if (0b1101 == opc) {
    // VLDR, VSTR - NOTE VSTR(lit) is deprecated
    offset = Instruction_aarch32::extract(insn, 7, 0) << 2;
    bool add = Instruction_aarch32::extract(insn, 23, 23);
    offset = add ? offset : -offset;
  } else if (0b0010 == opc) {
    // ADR
    offset = decode_imm12(Instruction_aarch32::extract(insn, 11, 0));
    int code = Instruction_aarch32::extract(insn, 23, 22);
    switch(code) {
      case 0b01: offset = -offset; break;
      case 0b10:                   break;
      default: ShouldNotReachHere();
    }
  } else {
    ShouldNotReachHere();
  }
  //Correct offset for PC
  offset += 8;
  return address(((uint32_t)insn_addr + offset));
}


void MacroAssembler::serialize_memory(Register thread, Register tmp) {
  dmb(Assembler::ISH);
}


void MacroAssembler::reset_last_Java_frame(bool clear_fp) {
  mov(rscratch1, 0);
  // we must set sp to zero to clear frame
  str(rscratch1, Address(rthread, JavaThread::last_Java_sp_offset()));

  // must clear fp, so that compiled frames are not confused; it is
  // possible that we need it only for debugging
  if (clear_fp) {
    str(rscratch1, Address(rthread, JavaThread::last_Java_fp_offset()));
  }

  // Always clear the pc because it could have been set by make_walkable()
  str(rscratch1, Address(rthread, JavaThread::last_Java_pc_offset()));
}

// Calls to C land
//
// When entering C land, the rfp & sp of the last Java frame have to be recorded
// in the (thread-local) JavaThread object. When leaving C land, the last Java fp
// has to be reset to 0. This is required to allow proper stack traversal.
void MacroAssembler::set_last_Java_frame(Register last_java_sp,
                                         Register last_java_fp,
                                         Register last_java_pc,
                                         Register scratch) {

  if (last_java_pc->is_valid()) {
      str(last_java_pc, Address(rthread,
                                JavaThread::frame_anchor_offset()
                                + JavaFrameAnchor::last_Java_pc_offset()));
    }

  // determine last_java_sp register
  if (last_java_sp == sp) {
    mov(scratch, sp);
    last_java_sp = scratch;
  } else if (!last_java_sp->is_valid()) {
    last_java_sp = sp;
  }

  str(last_java_sp, Address(rthread, JavaThread::last_Java_sp_offset()));

  // last_java_fp is optional
  if (last_java_fp->is_valid()) {
    str(last_java_fp, Address(rthread, JavaThread::last_Java_fp_offset()));
  }
}

void MacroAssembler::set_last_Java_frame(Register last_java_sp,
                                         Register last_java_fp,
                                         address  last_java_pc,
                                         Register scratch) {
  if (last_java_pc != NULL) {
    adr(scratch, last_java_pc);
  } else {
    // FIXME: This is almost never correct.  We should delete all
    // cases of set_last_Java_frame with last_java_pc=NULL and use the
    // correct return address instead.
    adr(scratch, pc());
  }

  str(scratch, Address(rthread,
                       JavaThread::frame_anchor_offset()
                       + JavaFrameAnchor::last_Java_pc_offset()));

  set_last_Java_frame(last_java_sp, last_java_fp, noreg, scratch);
}

void MacroAssembler::set_last_Java_frame(Register last_java_sp,
                                         Register last_java_fp,
                                         Label &L,
                                         Register scratch) {
  if (L.is_bound()) {
    set_last_Java_frame(last_java_sp, last_java_fp, target(L), scratch);
  } else {
    InstructionMark im(this);
    L.add_patch_at(code(), locator());
    set_last_Java_frame(last_java_sp, last_java_fp, (address)NULL, scratch);
  }
}

void MacroAssembler::far_call(Address entry, CodeBuffer *cbuf, Register tmp) {
  assert(CodeCache::find_blob(entry.target()) != NULL,
         "destination of far call not found in code cache");
  // TODO performance issue: if intented to patch later,
  // generate mov rX, imm; bl rX far call (to reserve space)
  if (far_branches()) {
    lea(tmp, entry);
    if (cbuf) cbuf->set_insts_mark();
    bl(tmp);
  } else {
    if (cbuf) cbuf->set_insts_mark();
    bl(entry);
  }
}

void MacroAssembler::far_jump(Address entry, CodeBuffer *cbuf, Register tmp) {
  assert(CodeCache::find_blob(entry.target()) != NULL,
         "destination of far call not found in code cache");
  assert(!external_word_Relocation::is_reloc_index((intptr_t)entry.target()), "can't far jump to reloc index)");
  if (far_branches()) {
    lea(tmp, entry);
    if (cbuf) cbuf->set_insts_mark();
    b(tmp);
  } else {
    if (cbuf) cbuf->set_insts_mark();
    b(entry);
  }
}

int MacroAssembler::biased_locking_enter(Register lock_reg,
                                         Register obj_reg,
                                         Register swap_reg,
                                         Register tmp_reg,
                                         bool swap_reg_contains_mark,
                                         Label& done,
                                         Label* slow_case,
                                         BiasedLockingCounters* counters) {
  assert(UseBiasedLocking, "why call this otherwise?");
  assert_different_registers(lock_reg, obj_reg, swap_reg);

  if (PrintBiasedLockingStatistics && counters == NULL)
    counters = BiasedLocking::counters();

  bool need_tmp_reg = false;
  if (tmp_reg == noreg) {
    tmp_reg = rscratch2;
  }
  assert_different_registers(lock_reg, obj_reg, swap_reg, tmp_reg, rscratch1);
  assert(markOopDesc::age_shift == markOopDesc::lock_bits + markOopDesc::biased_lock_bits, "biased locking makes assumptions about bit layout");
  Address mark_addr      (obj_reg, oopDesc::mark_offset_in_bytes());
  Address klass_addr     (obj_reg, oopDesc::klass_offset_in_bytes());
  Address saved_mark_addr(lock_reg, 0);

  // Biased locking
  // See whether the lock is currently biased toward our thread and
  // whether the epoch is still valid
  // Note that the runtime guarantees sufficient alignment of JavaThread
  // pointers to allow age to be placed into low bits
  // First check to see whether biasing is even enabled for this object
  Label cas_label;
  int null_check_offset = -1;
  if (!swap_reg_contains_mark) {
    null_check_offset = offset();
    ldr(swap_reg, mark_addr);
  }
  andr(tmp_reg, swap_reg, markOopDesc::biased_lock_mask_in_place);
  cmp(tmp_reg, markOopDesc::biased_lock_pattern);
  b(cas_label, Assembler::NE);
  // The bias pattern is present in the object's header. Need to check
  // whether the bias owner and the epoch are both still current.
  load_prototype_header(tmp_reg, obj_reg);
  orr(tmp_reg, tmp_reg, rthread);
  eor(tmp_reg, swap_reg, tmp_reg);
//  andr(tmp_reg, tmp_reg, ~((int) markOopDesc::age_mask_in_place));
  bic(tmp_reg, tmp_reg, markOopDesc::age_mask_in_place);
  if (counters != NULL) {
    Label around;
    cbnz(tmp_reg, around);
    atomic_inc(Address((address)counters->biased_lock_entry_count_addr()), tmp_reg, rscratch1);
    b(done);
    bind(around);
  } else {
    cbz(tmp_reg, done);
  }

  Label try_revoke_bias;
  Label try_rebias;

  // At this point we know that the header has the bias pattern and
  // that we are not the bias owner in the current epoch. We need to
  // figure out more details about the state of the header in order to
  // know what operations can be legally performed on the object's
  // header.

  // If the low three bits in the xor result aren't clear, that means
  // the prototype header is no longer biased and we have to revoke
  // the bias on this object.
  andr(rscratch1, tmp_reg, markOopDesc::biased_lock_mask_in_place);
  cbnz(rscratch1, try_revoke_bias);

  // Biasing is still enabled for this data type. See whether the
  // epoch of the current bias is still valid, meaning that the epoch
  // bits of the mark word are equal to the epoch bits of the
  // prototype header. (Note that the prototype header's epoch bits
  // only change at a safepoint.) If not, attempt to rebias the object
  // toward the current thread. Note that we must be absolutely sure
  // that the current epoch is invalid in order to do this because
  // otherwise the manipulations it performs on the mark word are
  // illegal.
  andr(rscratch1, tmp_reg, markOopDesc::epoch_mask_in_place);
  cbnz(rscratch1, try_rebias);

  // The epoch of the current bias is still valid but we know nothing
  // about the owner; it might be set or it might be clear. Try to
  // acquire the bias of the object using an atomic operation. If this
  // fails we will go in to the runtime to revoke the object's bias.
  // Note that we first construct the presumed unbiased header so we
  // don't accidentally blow away another thread's valid bias.
  {
    Label here;
    mov(rscratch1, markOopDesc::biased_lock_mask_in_place | markOopDesc::age_mask_in_place | markOopDesc::epoch_mask_in_place);
    andr(swap_reg, swap_reg, rscratch1);
    orr(tmp_reg, swap_reg, rthread);
    cmpxchgptr(swap_reg, tmp_reg, obj_reg, rscratch1, here, slow_case);
    // If the biasing toward our thread failed, this means that
    // another thread succeeded in biasing it toward itself and we
    // need to revoke that bias. The revocation will occur in the
    // interpreter runtime in the slow case.
    bind(here);
    if (counters != NULL) {
      atomic_inc(Address((address)counters->anonymously_biased_lock_entry_count_addr()),
                  tmp_reg, rscratch1);
    }
  }
  b(done);

  bind(try_rebias);
  // At this point we know the epoch has expired, meaning that the
  // current "bias owner", if any, is actually invalid. Under these
  // circumstances _only_, we are allowed to use the current header's
  // value as the comparison value when doing the cas to acquire the
  // bias in the current epoch. In other words, we allow transfer of
  // the bias from one thread to another directly in this situation.
  //
  // FIXME: due to a lack of registers we currently blow away the age
  // bits in this situation. Should attempt to preserve them.
  {
    Label here;
    load_prototype_header(tmp_reg, obj_reg);
    orr(tmp_reg, rthread, tmp_reg);
    cmpxchgptr(swap_reg, tmp_reg, obj_reg, rscratch1, here, slow_case);
    // If the biasing toward our thread failed, then another thread
    // succeeded in biasing it toward itself and we need to revoke that
    // bias. The revocation will occur in the runtime in the slow case.
    bind(here);
    if (counters != NULL) {
      atomic_inc(Address((address)counters->rebiased_lock_entry_count_addr()),
                  tmp_reg, rscratch1);
    }
  }
  b(done);

  bind(try_revoke_bias);
  // The prototype mark in the klass doesn't have the bias bit set any
  // more, indicating that objects of this data type are not supposed
  // to be biased any more. We are going to try to reset the mark of
  // this object to the prototype value and fall through to the
  // CAS-based locking scheme. Note that if our CAS fails, it means
  // that another thread raced us for the privilege of revoking the
  // bias of this particular object, so it's okay to continue in the
  // normal locking code.
  //
  // FIXME: due to a lack of registers we currently blow away the age
  // bits in this situation. Should attempt to preserve them.
  {
    Label here, nope;
    load_prototype_header(tmp_reg, obj_reg);
    cmpxchgptr(swap_reg, tmp_reg, obj_reg, rscratch1, here, &nope);
    bind(here);

    // Fall through to the normal CAS-based lock, because no matter what
    // the result of the above CAS, some thread must have succeeded in
    // removing the bias bit from the object's header.
    if (counters != NULL) {
      atomic_inc(Address((address)counters->revoked_lock_entry_count_addr()), tmp_reg,
                  rscratch1);
    }
    bind(nope);
  }

  bind(cas_label);

  return null_check_offset;
}

void MacroAssembler::biased_locking_exit(Register obj_reg, Register temp_reg, Label& done) {
  assert(UseBiasedLocking, "why call this otherwise?");

  // Check for biased locking unlock case, which is a no-op
  // Note: we do not have to check the thread ID for two reasons.
  // First, the interpreter checks for IllegalMonitorStateException at
  // a higher level. Second, if the bias was revoked while we held the
  // lock, the object could not be rebiased toward another thread, so
  // the bias bit would be clear.
  ldr(temp_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));
  andr(temp_reg, temp_reg, markOopDesc::biased_lock_mask_in_place);
  cmp(temp_reg, markOopDesc::biased_lock_pattern);
  b(done, Assembler::EQ);
}


static void pass_arg0(MacroAssembler* masm, Register arg) {
  if (c_rarg0 != arg ) {
    masm->mov(c_rarg0, arg);
  }
}

static void pass_arg1(MacroAssembler* masm, Register arg) {
  if (c_rarg1 != arg ) {
    masm->mov(c_rarg1, arg);
  }
}

static void pass_arg2(MacroAssembler* masm, Register arg) {
  if (c_rarg2 != arg ) {
    masm->mov(c_rarg2, arg);
  }
}

static void pass_arg3(MacroAssembler* masm, Register arg) {
  if (c_rarg3 != arg ) {
    masm->mov(c_rarg3, arg);
  }
}

void MacroAssembler::call_VM_base(Register oop_result,
                                  Register java_thread,
                                  Register last_java_sp,
                                  address  entry_point,
                                  int      number_of_arguments,
                                  bool     check_exceptions) {
   // determine java_thread register
  if (!java_thread->is_valid()) {
    java_thread = rthread;
  }

  // determine last_java_sp register
  if (!last_java_sp->is_valid()) {
    last_java_sp = sp;
  }

  // debugging support
  assert(number_of_arguments >= 0   , "cannot have negative number of arguments");
  assert(java_thread == rthread, "unexpected register");

  assert(java_thread != oop_result  , "cannot use the same register for java_thread & oop_result");
  assert(java_thread != last_java_sp, "cannot use the same register for java_thread & last_java_sp");

  // push java thread (becomes first argument of C function)

  mov(c_rarg0, java_thread);

  // set last Java frame before call
  assert(last_java_sp != rfp, "can't use rfp");

  Label l;
  set_last_Java_frame(last_java_sp, rfp, l, rscratch2);


  // FIXME - Can save lr in more elegant way ?
  //str(lr, pre(sp, -wordSize));

  // do the call, remove parameters
  MacroAssembler::call_VM_leaf_base(entry_point, number_of_arguments, &l);

  //ldr(lr, post(sp, wordSize));

  // reset last Java frame
  // Only interpreter should have to clear fp
  reset_last_Java_frame(true);

   // C++ interp handles this in the interpreter
  check_and_handle_popframe(java_thread);
  check_and_handle_earlyret(java_thread);

  if (check_exceptions) {
    // check for pending exceptions (java_thread is set upon return)
    ldr(rscratch2, Address(java_thread, in_bytes(Thread::pending_exception_offset())));
    Label ok;
    cbz(rscratch2, ok);

    lea(rscratch2, RuntimeAddress(StubRoutines::forward_exception_entry()));
    // forward_exception uses LR to choose exception handler but LR is trashed by previous code
    // since we used to get here from interpreted code BL is acceptable way to acquire correct LR (see StubGenerator::generate_forward_exception)
    bl(rscratch2);
    bind(ok);
  }

  // get oop result if there is one and reset the value in the thread
  if (oop_result->is_valid()) {
    get_vm_result(oop_result, java_thread);
  }
}

void MacroAssembler::call_VM_helper(Register oop_result, address entry_point, int number_of_arguments, bool check_exceptions) {
  call_VM_base(oop_result, noreg, noreg, entry_point, number_of_arguments, check_exceptions);
}

// Maybe emit a call via a trampoline.  If the code cache is small
// trampolines won't be emitted.

void MacroAssembler::trampoline_call(Address entry, CodeBuffer *cbuf) {
  assert(entry.rspec().type() == relocInfo::runtime_call_type
         || entry.rspec().type() == relocInfo::opt_virtual_call_type
         || entry.rspec().type() == relocInfo::static_call_type
         || entry.rspec().type() == relocInfo::virtual_call_type, "wrong reloc type");

  if (cbuf) {
    cbuf->set_insts_mark();
  }

  if (far_branches()) {
    // Have make trampline such way: destination address should be raw 4 byte value,
    // so it's patching could be done atomically.
    relocate(entry.rspec());
    address start = pc();
    add(lr, r15_pc, NativeCall::instruction_size - 2 * NativeInstruction::arm_insn_sz);
    ldr(r15_pc, Address(r15_pc, 4));
    emit_int32((uintptr_t) entry.target());
    // possibly pad the call to the NativeCall size to make patching happy
    while (pc() - start < NativeCall::instruction_size) {
      nop();
    }
    assert(pc() - start == NativeCall::instruction_size, "fix NativeTrampolineCall::instruction_size!");
  } else {
    bl(entry);
  }
}

void MacroAssembler::ic_call(address entry) {
  RelocationHolder rh = virtual_call_Relocation::spec(pc());
  // address const_ptr = long_constant((jlong)Universe::non_oop_word());
  // unsigned long offset;
  // ldr_constant(rscratch2, const_ptr);
  movptr(rscratch2, (uintptr_t)Universe::non_oop_word());
  trampoline_call(Address(entry, rh));
}

// Implementation of call_VM versions

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             bool check_exceptions) {
  call_VM_helper(oop_result, entry_point, 0, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             bool check_exceptions) {
  pass_arg1(this, arg_1);
  call_VM_helper(oop_result, entry_point, 1, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             bool check_exceptions) {
  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);
  pass_arg1(this, arg_1);
  call_VM_helper(oop_result, entry_point, 2, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             Register arg_3,
                             bool check_exceptions) {
  assert(arg_1 != c_rarg3, "smashed arg");
  assert(arg_2 != c_rarg3, "smashed arg");
  pass_arg3(this, arg_3);

  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);

  pass_arg1(this, arg_1);
  call_VM_helper(oop_result, entry_point, 3, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             int number_of_arguments,
                             bool check_exceptions) {
  call_VM_base(oop_result, rthread, last_java_sp, entry_point, number_of_arguments, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             bool check_exceptions) {
  pass_arg1(this, arg_1);
  call_VM(oop_result, last_java_sp, entry_point, 1, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             bool check_exceptions) {

  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);
  pass_arg1(this, arg_1);
  call_VM(oop_result, last_java_sp, entry_point, 2, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             Register arg_3,
                             bool check_exceptions) {
  assert(arg_1 != c_rarg3, "smashed arg");
  assert(arg_2 != c_rarg3, "smashed arg");
  pass_arg3(this, arg_3);
  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);
  pass_arg1(this, arg_1);
  call_VM(oop_result, last_java_sp, entry_point, 3, check_exceptions);
}


void MacroAssembler::get_vm_result(Register oop_result, Register java_thread) {
  ldr(oop_result, Address(java_thread, JavaThread::vm_result_offset()));
  assert(oop_result != rscratch2, "can't be");
  mov(rscratch2, 0);
  str(rscratch2, Address(java_thread, JavaThread::vm_result_offset()));
  verify_oop(oop_result, "broken oop in call_VM_base");
}

void MacroAssembler::get_vm_result_2(Register metadata_result, Register java_thread) {
  ldr(metadata_result, Address(java_thread, JavaThread::vm_result_2_offset()));
  assert(metadata_result != rscratch2 &&
         java_thread != rscratch2, "can't be");
  mov(rscratch2, 0);
  str(rscratch2, Address(java_thread, JavaThread::vm_result_2_offset()));
}

void MacroAssembler::align(int modulus) {
  while (offset() % modulus != 0) nop();
}

// these are no-ops overridden by InterpreterMacroAssembler

void MacroAssembler::check_and_handle_earlyret(Register java_thread) { }

void MacroAssembler::check_and_handle_popframe(Register java_thread) { }


RegisterOrConstant MacroAssembler::delayed_value_impl(intptr_t* delayed_value_addr,
                                                      Register tmp,
                                                      int offset) {
  intptr_t value = *delayed_value_addr;
  if (value != 0)
    return RegisterOrConstant(value + offset);

  // load indirectly to solve generation ordering problem
  ldr(tmp, ExternalAddress((address) delayed_value_addr));

  if (offset != 0)
    add(tmp, tmp, offset);

  return RegisterOrConstant(tmp);
}


// Look up the method for a megamorphic invokeinterface call.
// The target method is determined by <intf_klass, itable_index>.
// The receiver klass is in recv_klass.
// On success, the result will be in method_result, and execution falls through.
// On failure, execution transfers to the given label.
void MacroAssembler::lookup_interface_method(Register recv_klass,
                                             Register intf_klass,
                                             RegisterOrConstant itable_index,
                                             Register method_result,
                                             Register scan_temp,
                                             Label& L_no_such_interface,
                                             bool return_method) {
  assert_different_registers(recv_klass, intf_klass, scan_temp);
  assert_different_registers(method_result, intf_klass, scan_temp);
  assert(recv_klass != method_result || !return_method,
         "recv_klass can be destroyed when method isn't needed");

  // Compute start of first itableOffsetEntry (which is at the end of the vtable)
  int vtable_base = InstanceKlass::vtable_start_offset() * wordSize;
  int itentry_off = itableMethodEntry::method_offset_in_bytes();
  int scan_step   = itableOffsetEntry::size() * wordSize;
  int vte_size    = vtableEntry::size() * wordSize;
  assert(vte_size == wordSize, "else adjust times_vte_scale");

  ldr(scan_temp, Address(recv_klass, InstanceKlass::vtable_length_offset() * wordSize));

  // %%% Could store the aligned, prescaled offset in the klassoop.
  // lea(scan_temp, Address(recv_klass, scan_temp, times_vte_scale, vtable_base));
  lea(scan_temp, Address(recv_klass, scan_temp, lsl(2)));
  add(scan_temp, scan_temp, vtable_base);
  if (HeapWordsPerLong > 1) {
    // Round up to align_object_offset boundary
    // see code for instanceKlass::start_of_itable!
    round_to(scan_temp, BytesPerLong);
  }

  if (return_method) {
    // Adjust recv_klass by scaled itable_index, so we can free itable_index.
    assert(itableMethodEntry::size() * wordSize == wordSize, "adjust the scaling in the code below");
    // lea(recv_klass, Address(recv_klass, itable_index, Address::times_ptr, itentry_off));
    lea(recv_klass, itable_index.is_register() ?
            Address(recv_klass, itable_index, lsl(2)) :
            Address(recv_klass, itable_index.as_constant() << 2));
    if (itentry_off)
      add(recv_klass, recv_klass, itentry_off);
  }

  // for (scan = klass->itable(); scan->interface() != NULL; scan += scan_step) {
  //   if (scan->interface() == intf) {
  //     result = (klass + scan->offset() + itable_index);
  //   }
  // }
  Label search, found_method;

  for (int peel = 1; peel >= 0; peel--) {
    ldr(method_result, Address(scan_temp, itableOffsetEntry::interface_offset_in_bytes()));
    cmp(intf_klass, method_result);

    if (peel) {
      b(found_method, Assembler::EQ);
    } else {
      b(search, Assembler::NE);
      // (invert the test to fall through to found_method...)
    }

    if (!peel)  break;

    bind(search);

    // Check that the previous entry is non-null.  A null entry means that
    // the receiver class doesn't implement the interface, and wasn't the
    // same as when the caller was compiled.
    cbz(method_result, L_no_such_interface);
    add(scan_temp, scan_temp, scan_step);
  }

  bind(found_method);

  if (return_method) {
    // Got a hit.
    ldr(scan_temp, Address(scan_temp, itableOffsetEntry::offset_offset_in_bytes()));
    ldr(method_result, Address(recv_klass, scan_temp));
  }
}

// virtual method calling
void MacroAssembler::lookup_virtual_method(Register recv_klass,
                                           RegisterOrConstant vtable_index,
                                           Register method_result) {
  const int base = InstanceKlass::vtable_start_offset() * wordSize;
  int vtable_offset_in_bytes = base + vtableEntry::method_offset_in_bytes();
  if (vtable_index.is_register()) {
    lea(method_result, Address(recv_klass,
                               vtable_index.as_register(),
                               lsl(LogBytesPerWord)));
    ldr(method_result, Address(method_result, vtable_offset_in_bytes));
  } else {
    vtable_offset_in_bytes += vtable_index.as_constant() * wordSize;
    if(is_valid_for_offset_imm(vtable_offset_in_bytes, 12)) {
      ldr(method_result, Address(recv_klass, vtable_offset_in_bytes));
    } else {
      mov(method_result, vtable_offset_in_bytes);
      ldr(method_result, Address(recv_klass, method_result));
    }
  }
}

void MacroAssembler::check_klass_subtype(Register sub_klass,
                           Register super_klass,
                           Register temp_reg,
                           Label& L_success) {
  Label L_failure;
  check_klass_subtype_fast_path(sub_klass, super_klass, temp_reg,        &L_success, &L_failure, NULL);
  check_klass_subtype_slow_path(sub_klass, super_klass, temp_reg, noreg, &L_success, NULL);
  bind(L_failure);
}


void MacroAssembler::check_klass_subtype_fast_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp_reg,
                                                   Label* L_success,
                                                   Label* L_failure,
                                                   Label* L_slow_path,
                                        RegisterOrConstant super_check_offset) {
  assert_different_registers(sub_klass, super_klass, temp_reg);
  bool must_load_sco = (super_check_offset.constant_or_zero() == -1);
  if (super_check_offset.is_register()) {
    assert_different_registers(sub_klass, super_klass,
                               super_check_offset.as_register());
  } else if (must_load_sco) {
    assert(temp_reg != noreg, "supply either a temp or a register offset");
  }

  Label L_fallthrough;
  int label_nulls = 0;
  if (L_success == NULL)   { L_success   = &L_fallthrough; label_nulls++; }
  if (L_failure == NULL)   { L_failure   = &L_fallthrough; label_nulls++; }
  if (L_slow_path == NULL) { L_slow_path = &L_fallthrough; label_nulls++; }
  assert(label_nulls <= 1, "at most one NULL in the batch");

  int sc_offset = in_bytes(Klass::secondary_super_cache_offset());
  int sco_offset = in_bytes(Klass::super_check_offset_offset());
  Address super_check_offset_addr(super_klass, sco_offset);

  // Hacked jmp, which may only be used just before L_fallthrough.
#define final_jmp(label)                                                \
  if (&(label) == &L_fallthrough) { /*do nothing*/ }                    \
  else                            b(label)                /*omit semi*/

  // If the pointers are equal, we are done (e.g., String[] elements).
  // This self-check enables sharing of secondary supertype arrays among
  // non-primary types such as array-of-interface.  Otherwise, each such
  // type would need its own customized SSA.
  // We move this check to the front of the fast path because many
  // type checks are in fact trivially successful in this manner,
  // so we get a nicely predicted branch right at the start of the check.
  cmp(sub_klass, super_klass);
  b(*L_success, Assembler::EQ);

  // Check the supertype display:
  if (must_load_sco) {
    ldr(temp_reg, super_check_offset_addr);
    super_check_offset = RegisterOrConstant(temp_reg);
  }
  Address super_check_addr(sub_klass, super_check_offset);
  ldr(rscratch1, super_check_addr);
  cmp(super_klass, rscratch1); // load displayed supertype

  // This check has worked decisively for primary supers.
  // Secondary supers are sought in the super_cache ('super_cache_addr').
  // (Secondary supers are interfaces and very deeply nested subtypes.)
  // This works in the same check above because of a tricky aliasing
  // between the super_cache and the primary super display elements.
  // (The 'super_check_addr' can address either, as the case requires.)
  // Note that the cache is updated below if it does not help us find
  // what we need immediately.
  // So if it was a primary super, we can just fail immediately.
  // Otherwise, it's the slow path for us (no success at this point).

  if (super_check_offset.is_register()) {
    b(*L_success, Assembler::EQ);
    cmp(super_check_offset.as_register(), sc_offset);
    if (L_failure == &L_fallthrough) {
      b(*L_slow_path, Assembler::EQ);
    } else {
      b(*L_failure, Assembler::NE);
      final_jmp(*L_slow_path);
    }
  } else if (super_check_offset.as_constant() == sc_offset) {
    // Need a slow path; fast failure is impossible.
    if (L_slow_path == &L_fallthrough) {
      b(*L_success, Assembler::EQ);
    } else {
      b(*L_slow_path, Assembler::NE);
      final_jmp(*L_success);
    }
  } else {
    // No slow path; it's a fast decision.
    if (L_failure == &L_fallthrough) {
      b(*L_success, Assembler::EQ);
    } else {
      b(*L_failure, Assembler::NE);
      final_jmp(*L_success);
    }
  }

  bind(L_fallthrough);

#undef final_jmp
}

// These two are taken from x86, but they look generally useful

// scans count pointer sized words at [addr] for occurence of value,
// generic
void MacroAssembler::repne_scan(Register addr, Register value, Register count,
                                Register scratch) {
  Label loop, fail, found;
  cmp(count, 0);
  b(fail, EQ);

  bind(loop);
  ldr(scratch, post(addr, wordSize));
  cmp(value, scratch);
  b(found, EQ);
  subs(count, count, 1);
  b(loop, NE);

  bind(fail);
  cmp(sp, 0); // sp never zero
  bind(found);
}

// Form an address from base + offset in Rd.  Rd may or may
// not actually be used: you must use the Address that is returned.
// It is up to you to ensure that the shift provided matches the size
// of your data.
Address MacroAssembler::form_address(Register Rd, Register base, long byte_offset, int shift) {
  // form_address result should only be used together with ldr/str instructions
  // otherwise please provide exact type instead of IDT_INT or apply safe_for()
  if (Address::offset_ok_for_immed(byte_offset, Address::IDT_INT))
    // It fits; no need for any heroics
    return Address(base, byte_offset);

  // See if we can do this with two 12-bit offsets
  {
    unsigned long masked_offset = byte_offset & ~0xfff;
    if (Address::offset_ok_for_immed(byte_offset - masked_offset, Address::IDT_INT)
        && Assembler::operand_valid_for_add_sub_immediate(masked_offset)) {
      add(Rd, base, masked_offset);
      byte_offset -= masked_offset;
      return Address(Rd, byte_offset);
    }
  }

  // Do it the hard way
  mov(Rd, byte_offset);
  add(Rd, base, Rd);
  return Address(Rd);
}

// scans count 4 byte words at [addr] for occurence of value,
// generic
/*void MacroAssembler::repne_scanw(Register addr, Register value, Register count,
                                Register scratch) {
  Label Lloop, Lexit;
  cbz(count, Lexit);
  bind(Lloop);
  ldr(scratch, post(addr, wordSize));
  cmp(value, scratch);
  b(Lexit, EQ);
  sub(count, count, 1);
  cbnz(count, Lloop);
  bind(Lexit);
}*/

void MacroAssembler::check_klass_subtype_slow_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp_reg,
                                                   Register temp2_reg,
                                                   Label* L_success,
                                                   Label* L_failure,
                                                   bool set_cond_codes) {
  assert_different_registers(sub_klass, super_klass, temp_reg);
  if (temp2_reg != noreg)
    assert_different_registers(sub_klass, super_klass, temp_reg, temp2_reg, rscratch1);
#define IS_A_TEMP(reg) ((reg) == temp_reg || (reg) == temp2_reg)

  Label L_fallthrough;
  int label_nulls = 0;
  if (L_success == NULL)   { L_success   = &L_fallthrough; label_nulls++; }
  if (L_failure == NULL)   { L_failure   = &L_fallthrough; label_nulls++; }
  assert(label_nulls <= 1, "at most one NULL in the batch");

  // a couple of useful fields in sub_klass:
  int ss_offset = in_bytes(Klass::secondary_supers_offset());
  int sc_offset = in_bytes(Klass::secondary_super_cache_offset());
  Address secondary_supers_addr(sub_klass, ss_offset);
  Address super_cache_addr(     sub_klass, sc_offset);

  BLOCK_COMMENT("check_klass_subtype_slow_path");

  // Do a linear scan of the secondary super-klass chain.
  // This code is rarely used, so simplicity is a virtue here.
  // The repne_scan instruction uses fixed registers, which we must spill.
  // Don't worry too much about pre-existing connections with the input regs.

  assert(sub_klass != r0, "killed reg"); // killed by mov(r0, super)
  assert(sub_klass != r2, "killed reg"); // killed by lea(r2, &pst_counter)

  // Get super_klass value into r0 (even if it was in r14 or r2).
  RegSet pushed_registers;
  if (!IS_A_TEMP(r2))    pushed_registers += r2;
  if (!IS_A_TEMP(r14))    pushed_registers += r14;

  if (super_klass != r0) {
    if (!IS_A_TEMP(r0))   pushed_registers += r0;
  }

  push(pushed_registers, sp);

#ifndef PRODUCT
  mov(rscratch2, (address)&SharedRuntime::_partial_subtype_ctr);
  Address pst_counter_addr(rscratch2);
  ldr(rscratch1, pst_counter_addr);
  add(rscratch1, rscratch1, 1);
  str(rscratch1, pst_counter_addr);
#endif //PRODUCT

  // We will consult the secondary-super array.
  ldr(r14, secondary_supers_addr);
  // Load the array length.
  ldr(r2, Address(r14, Array<Klass*>::length_offset_in_bytes()));
  // Skip to start of data.
  add(r14, r14, Array<Klass*>::base_offset_in_bytes());

  cmp(sp, 0); // Clear Z flag; SP is never zero
  // Scan R2 words at [R14] for an occurrence of R0.
  // Set NZ/Z based on last compare.
  repne_scan(r14, r0, r2, rscratch1);

  // Unspill the temp. registers:
  pop(pushed_registers, sp);

  b(*L_failure, Assembler::NE);

  // Success.  Cache the super we found and proceed in triumph.
  str(super_klass, super_cache_addr);

  if (L_success != &L_fallthrough) {
    b(*L_success);
  }

#undef IS_A_TEMP

  bind(L_fallthrough);
}


void MacroAssembler::verify_oop(Register reg, const char* s) {
  if (!VerifyOops) return;

  // Pass register number to verify_oop_subroutine
  const char* b = NULL;
  {
    ResourceMark rm;
    stringStream ss;
    ss.print("verify_oop: %s: %s", reg->name(), s);
    b = code_string(ss.as_string());
  }
  BLOCK_COMMENT("verify_oop {");

  stmdb(sp, RegSet::of(r0, r1, rscratch1, rscratch2, lr).bits());

  mov(r0, reg);
  mov(rscratch1, (address)b);
  mrs(r1);

  // call indirectly to solve generation ordering problem
  reg_printf("Verify oop entry, sp = %p, rfp = %p\n", sp, rfp);
  lea(rscratch2, ExternalAddress(StubRoutines::verify_oop_subroutine_entry_address()));
  ldr(rscratch2, Address(rscratch2));
  bl(rscratch2);
  reg_printf("Verify oop exit,  sp = %p, rfp = %p\n", sp, rfp);

  msr(r1);
  ldmia(sp, RegSet::of(r0, r1, rscratch1, rscratch2, lr).bits());

  BLOCK_COMMENT("} verify_oop");
}

void MacroAssembler::verify_oop_addr(Address addr, const char* s) {
  if (!VerifyOops) return;

  const char* b = NULL;
  {
    ResourceMark rm;
    stringStream ss;
    ss.print("verify_oop_addr: %s", s);
    b = code_string(ss.as_string());
  }
  BLOCK_COMMENT("verify_oop_addr {");

  stmdb(sp, RegSet::of(r0, r1, rscratch1, rscratch2, lr).bits());
  mrs(r1);

  // addr may contain sp so we will have to adjust it based on the
  // pushes that we just did.
  if (addr.uses(sp)) {
    lea(r0, addr);
    ldr(r0, Address(r0, 5 * wordSize));
  } else {
    ldr(r0, addr);
  }
  mov(rscratch1, (address)b);

  // call indirectly to solve generation ordering problem
  lea(rscratch2, ExternalAddress(StubRoutines::verify_oop_subroutine_entry_address()));
  ldr(rscratch2, Address(rscratch2));
  bl(rscratch2);

  msr(r1);
  ldmia(sp, RegSet::of(r0, r1, rscratch1, rscratch2, lr).bits());

  BLOCK_COMMENT("} verify_oop_addr");
}

Address MacroAssembler::argument_address(RegisterOrConstant arg_slot,
                                         int extra_slot_offset) {
  // cf. TemplateTable::prepare_invoke(), if (load_receiver).
  int stackElementSize = Interpreter::stackElementSize;
  int offset = Interpreter::expr_offset_in_bytes(extra_slot_offset+0);
#ifdef ASSERT
  int offset1 = Interpreter::expr_offset_in_bytes(extra_slot_offset+1);
  assert(offset1 - offset == stackElementSize, "correct arithmetic");
#endif
  if (arg_slot.is_constant()) {
    return Address(sp, arg_slot.as_constant() * stackElementSize
                   + offset);
  } else {
    add(rscratch1, sp, arg_slot.as_register(),
        lsl(exact_log2(stackElementSize)));
    return Address(rscratch1, offset);
  }
}

void MacroAssembler::call_VM_leaf_base(address entry_point,
                                       int number_of_arguments,
                                       Label *retaddr) {
  Label E, L;

  //FIXME Do this alignment in a more elegant way
  mov(rscratch2, sp);
  sub(sp, sp, wordSize);
  bic(sp, sp, 2 * wordSize - 1); // Align to eight bytes
  str(rscratch2, Address(sp));

  // FIXME Do we need to preserve rscratch2?
  //str(rscratch2, Address(pre(sp, -wordSize)));

  mov(rscratch2, entry_point);
  reg_printf("\tJust about to call into the VM, rfp = %p\n", rfp);
  bl(rscratch2);
  if (retaddr)
    bind(*retaddr);
  reg_printf("\tReturned from call into the VM, rfp = %p\n", rfp);

  //ldr(rscratch2, Address(post(sp, wordSize)));

  //Undo alignment
  ldr(sp, Address(sp));

  maybe_isb();
}

void MacroAssembler::call_VM_leaf(address entry_point, int number_of_arguments) {
  call_VM_leaf_base(entry_point, number_of_arguments);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0) {
  pass_arg0(this, arg_0);
  call_VM_leaf_base(entry_point, 1);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0, Register arg_1) {
  pass_arg0(this, arg_0);
  pass_arg1(this, arg_1);
  call_VM_leaf_base(entry_point, 2);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0,
                                  Register arg_1, Register arg_2) {
  pass_arg0(this, arg_0);
  pass_arg1(this, arg_1);
  pass_arg2(this, arg_2);
  call_VM_leaf_base(entry_point, 3);
}

void MacroAssembler::super_call_VM_leaf(address entry_point, Register arg_0) {
  pass_arg0(this, arg_0);
  MacroAssembler::call_VM_leaf_base(entry_point, 1);
}

void MacroAssembler::super_call_VM_leaf(address entry_point, Register arg_0, Register arg_1) {

  assert(arg_0 != c_rarg1, "smashed arg");
  pass_arg1(this, arg_1);
  pass_arg0(this, arg_0);
  MacroAssembler::call_VM_leaf_base(entry_point, 2);
}

void MacroAssembler::super_call_VM_leaf(address entry_point, Register arg_0, Register arg_1, Register arg_2) {
  assert(arg_0 != c_rarg2, "smashed arg");
  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);
  assert(arg_0 != c_rarg1, "smashed arg");
  pass_arg1(this, arg_1);
  pass_arg0(this, arg_0);
  MacroAssembler::call_VM_leaf_base(entry_point, 3);
}

void MacroAssembler::super_call_VM_leaf(address entry_point, Register arg_0, Register arg_1, Register arg_2, Register arg_3) {
  assert(arg_0 != c_rarg3, "smashed arg");
  assert(arg_1 != c_rarg3, "smashed arg");
  assert(arg_2 != c_rarg3, "smashed arg");
  pass_arg3(this, arg_3);
  assert(arg_0 != c_rarg2, "smashed arg");
  assert(arg_1 != c_rarg2, "smashed arg");
  pass_arg2(this, arg_2);
  assert(arg_0 != c_rarg1, "smashed arg");
  pass_arg1(this, arg_1);
  pass_arg0(this, arg_0);
  MacroAssembler::call_VM_leaf_base(entry_point, 4);
}

// Clobbers rscratch1
void MacroAssembler::null_check(Register reg, int offset) {
  if (needs_explicit_null_check(offset)) {
    // provoke OS NULL exception if reg = NULL by
    // accessing M[reg] w/o changing any registers
    // NOTE: this is plenty to provoke a segv
    reg_printf("Generating OS check null with ptr = %p\n", reg);
    assert(reg != rscratch1, "can't be");
    ldr(rscratch1, Address(reg));
  } else {
    // nothing to do, (later) access of M[reg + offset]
    // will provoke OS NULL exception if reg = NULL
  }
}

// MacroAssembler protected routines needed to implement
// public methods

void MacroAssembler::mov(Register r, Address dest, Condition cond) {
  code_section()->relocate(pc(), dest.rspec());
  uint32_t imm32 = (uint32_t)dest.target();
  movptr(r, imm32, cond);
}

// Move a constant pointer into r.  In aarch32 address space
// is 32 bits in size and so a pointer can be encoded in two mov
// instructions.
void MacroAssembler::movptr(Register r, uintptr_t imm32, Condition cond) {
#ifndef PRODUCT
  {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "0x%"PRIX32, imm32);
    block_comment(buffer);
  }
#endif
  Assembler::mov_immediate32(r, imm32, cond, false);
}

void MacroAssembler::ret(Register reg) {
  assert(reg == lr, "Can do return only to LR");
  mov(r15_pc, lr);
}

void MacroAssembler::atomic_inc(Register counter_addr, Register tmp) {
  Label retry_load;
  bind(retry_load);
  // flush and load exclusive from the memory location
  ldrex(tmp, counter_addr);
  add(tmp, tmp, 1);
  // if we store+flush with no intervening write tmp wil be zero
  strex(tmp, tmp, counter_addr);
  cmp(tmp, 0);
  b(retry_load, Assembler::NE);
}


// MacroAssembler routines found actually to be needed

void MacroAssembler::push(Register src)
{
  str(src, Address(pre(sp, -1 * wordSize)));
}

void MacroAssembler::pop(Register dst)
{
  ldr(dst, Address(post(sp, 1 * wordSize)));
}

// Note: load_unsigned_short used to be called load_unsigned_word.
int MacroAssembler::load_unsigned_short(Register dst, Address src) {
  int off = offset();
  ldrh(dst, src);
  return off;
}

int MacroAssembler::load_unsigned_byte(Register dst, Address src) {
  int off = offset();
  ldrb(dst, src);
  return off;
}

int MacroAssembler::load_signed_short(Register dst, Address src) {
  int off = offset();
  ldrsh(dst, src);
  return off;
}

int MacroAssembler::load_signed_byte(Register dst, Address src) {
  int off = offset();
  ldrsb(dst, src);
  return off;
}

void MacroAssembler::load_sized_value(Register dst, Address src, size_t size_in_bytes, bool is_signed, Register dst2) {
  switch (size_in_bytes) {
  //case  8:  ldr(dst, src); break;
  case  4:  ldr(dst, src); break;
  case  2:  is_signed ? load_signed_short(dst, src) : load_unsigned_short(dst, src); break;
  case  1:  is_signed ? load_signed_byte( dst, src) : load_unsigned_byte( dst, src); break;
  default:  ShouldNotReachHere();
  }
}

void MacroAssembler::store_sized_value(Address dst, Register src, size_t size_in_bytes, Register src2) {
  switch (size_in_bytes) {
  //case  8:  str(src, dst); break;
  case  4:  str(src, dst); break;
  case  2:  strh(src, dst); break;
  case  1:  strb(src, dst); break;
  default:  ShouldNotReachHere();
  }
}

void MacroAssembler::decrement(Register reg, int value) {
  if (value < 0) {
    increment(reg, -value);
    return;
  }
  if (value == 0) {
    return;
  }
  if (operand_valid_for_add_sub_immediate(value)) {
    sub(reg, reg, value);
    return;
  }
  assert(reg != rscratch2, "invalid register for decrement");
  mov(rscratch2, (unsigned int) value);
  sub(reg, reg, rscratch2);
}

void MacroAssembler::decrement(Address dst, int value) {
  assert(!dst.uses(rscratch1), "invalid address for decrement");
  ldr(rscratch1, dst);
  decrement(rscratch1, value);
  str(rscratch1, dst);
}

void MacroAssembler::increment(Register reg, int value) {
  if (value < 0) {
    decrement(reg, -value);
    return;
  }
  if (value == 0) {
    return;
  }
  if (operand_valid_for_add_sub_immediate(value)) {
    add(reg, reg, value);
    return;
  }
  assert(reg != rscratch2, "invalid register for increment");
  mov(rscratch2, (unsigned int) value);
  add(reg, reg, rscratch2);
}

void MacroAssembler::increment(Address dst, int value) {
  assert(!dst.uses(rscratch1), "invalid address for increment");
  ldr(rscratch1, dst);
  increment(rscratch1, value);
  str(rscratch1, dst);
}

// Loads and stores everything except the pc and sp
void MacroAssembler::pusha() {
  unsigned regset = 0b0101111111111111;
  stmdb(sp, regset);
}
void MacroAssembler::popa() {
  unsigned regset = 0b0101111111111111;
  ldmia(sp, regset);
}

static void multiple_reg_check(unsigned int bitset, Register stack) {
  const unsigned int pcbit = 1 << r15_pc->encoding();
  const unsigned int lrbit = 1 << lr->encoding();
  const unsigned int spbit = 1 << sp->encoding();
  const unsigned int stackbit = 1 << stack->encoding();
  assert(!(bitset & spbit), "The SP can be in the list. However, "
      "ARM deprecates using these instructions with SP in the list.");
  assert(!(bitset & pcbit) || !(bitset & lrbit),
      "ARM deprecates using these instructions with both "
      "the LR and the PC in the list.");
  assert(!(bitset & stackbit), "Instructions with the base register "
      "in the list and ! specified are only available before ARMv7, "
      "and ARM deprecates the use of such instructions. "
      "The value of the base register after such an instruction is UNKNOWN");
}

// Push lots of registers in the bit set supplied.  Don't push sp.
// Return the number of words pushed
int MacroAssembler::push(unsigned int bitset, Register stack) {
  multiple_reg_check(bitset, stack);
  unsigned bc = bitset, count = 0, i;
  for(i = 0; i <= 15; i++) {
    if (1 & bc) count++;
    bc >>= 1;
  }
  // TODO Also why did it only do even quantities before?
  stmdb(stack, bitset);
  return count;
}

int MacroAssembler::pop(unsigned int bitset, Register stack) {
  multiple_reg_check(bitset, stack);
  unsigned bc = bitset, count = 0, i;
  for(i = 0; i <= 15; i++) {
    if (1 & bc) count++;
    bc >>= 1;
  }
  // TODO Also why did it only do even quantities before?
  ldmia(stack, bitset);
  return count;
}

void MacroAssembler::stop(const char* msg) {
  pusha();
  // Save old sp value
  add(rscratch2, sp, 14 * wordSize);
  str(rscratch2, Address(pre(sp, -4)));
  mov(c_rarg0, (address)msg);
  mov(c_rarg1, r15_pc);
  sub(c_rarg1, c_rarg1, 8); // Restore to actual value
  mov(c_rarg2, sp);
  mov(c_rarg3, CAST_FROM_FN_PTR(address, MacroAssembler::debug32));
  bl(c_rarg3);
  hlt(0);
}

// this simulates the behaviour of the x86 cmpxchg instruction using a
// load linked/store conditional pair. we use the acquire/release
// versions of these instructions so that we flush pending writes as
// per Java semantics.

// n.b the x86 version assumes the old value to be compared against is
// in rax and updates rax with the value located in memory if the
// cmpxchg fails. we supply a register for the old value explicitly

// the aarch32 load linked/store conditional instructions do not
// accept an offset. so, unlike x86, we must provide a plain register
// to identify the memory word to be compared/exchanged rather than a
// register+offset Address.

void MacroAssembler::cmpxchgptr(Register oldv, Register newv, Register addr, Register tmp,
                                Label &succeed, Label *fail) {
  // oldv holds comparison value
  // newv holds value to write in exchange
  // addr identifies memory word to compare against/update
  // tmp returns 0/1 for success/failure
  Label retry_load, nope;

  bind(retry_load);
  // flush and load exclusive from the memory location
  // and fail if it is not what we expect
  ldrex(tmp, addr);
  cmp(tmp, oldv);
  b(nope, Assembler::NE);
  // if we store+flush with no intervening write tmp wil be zero
  strex(tmp, newv, addr);
  cmp(tmp, 0);
  b(succeed, Assembler::EQ);
  // retry so we only ever return after a load fails to compare
  // ensures we don't return a stale value after a failed write.
  b(retry_load);
  // if the memory word differs we return it in oldv and signal a fail
  bind(nope);
  membar(AnyAny);
  mov(oldv, tmp);
  if (fail)
    b(*fail);
}

void MacroAssembler::cmpxchgw(Register oldv, Register newv, Register addr, Register tmp,
                                Label &succeed, Label *fail) {
  // oldv holds comparison value
  // newv holds value to write in exchange
  // addr identifies memory word to compare against/update
  // tmp returns 0/1 for success/failure
  Label retry_load, nope;

  bind(retry_load);
  // flush and load exclusive from the memory location
  // and fail if it is not what we expect
  ldrex(tmp, addr);
  cmp(tmp, oldv);
  b(nope, Assembler::NE);
  // if we store+flush with no intervening write tmp wil be zero
  strex(tmp, newv, addr);
  cmp(tmp, 0);
  b(succeed, Assembler::EQ);
  // retry so we only ever return after a load fails to compare
  // ensures we don't return a stale value after a failed write.
  b(retry_load);
  // if the memory word differs we return it in oldv and signal a fail
  bind(nope);
  membar(AnyAny);
  mov(oldv, tmp);
  if (fail)
    b(*fail);
}

void MacroAssembler::incr_allocated_bytes(Register thread,
                                          Register var_size_in_bytes,
                                          int con_size_in_bytes,
                                          Register t1) {
  if (!thread->is_valid()) {
    thread = rthread;
  }
  assert(t1->is_valid(), "need temp reg");

  ldr(t1, Address(thread, in_bytes(JavaThread::allocated_bytes_offset())));
  if (var_size_in_bytes->is_valid()) {
    add(t1, t1, var_size_in_bytes);
  } else {
    add(t1, t1, con_size_in_bytes);
  }
  str(t1, Address(thread, in_bytes(JavaThread::allocated_bytes_offset())));
}

#ifndef PRODUCT
extern "C" void findpc(intptr_t x);
#endif

void MacroAssembler::debug32(char* msg, int32_t pc, int32_t regs[])
{
  print_unseen_bytecodes();
  // In order to get locks to work, we need to fake a in_VM state
  if (ShowMessageBoxOnError) {
    JavaThread* thread = JavaThread::current();
    JavaThreadState saved_state = thread->thread_state();
    thread->set_thread_state(_thread_in_vm);
#ifndef PRODUCT
    if (CountBytecodes || TraceBytecodes || StopInterpreterAt) {
      ttyLocker ttyl;
      BytecodeCounter::print();
    }
#endif
    if (os::message_box(msg, "Execution stopped, print registers?")) {
      ttyLocker ttyl;
      tty->print_cr(" pc = 0x%016x", pc);
#ifndef PRODUCT
      tty->cr();
      findpc(pc);
      tty->cr();
#endif
      tty->print_cr("THIS IS WRONG!");
      tty->print_cr(" r0 = 0x%016x", regs[0]);
      tty->print_cr(" r1 = 0x%016x", regs[1]);
      tty->print_cr(" r2 = 0x%016x", regs[2]);
      tty->print_cr(" r3 = 0x%016x", regs[3]);
      tty->print_cr(" r4 = 0x%016x", regs[4]);
      tty->print_cr(" r5 = 0x%016x", regs[5]);
      tty->print_cr(" r6 = 0x%016x", regs[6]);
      tty->print_cr(" r7 = 0x%016x", regs[7]);
      tty->print_cr(" r8 = 0x%016x", regs[8]);
      tty->print_cr(" r9 = 0x%016x", regs[9]);
      tty->print_cr("r10 = 0x%016x", regs[10]);
      tty->print_cr("r11 = 0x%016x", regs[11]);
      tty->print_cr("r12 = 0x%016x", regs[12]);
      tty->print_cr("r13 = 0x%016x", regs[13]);
      tty->print_cr("r14 = 0x%016x", regs[14]);
      tty->print_cr("r15 = 0x%016x", regs[15]);
      BREAKPOINT;
    }
    ThreadStateTransition::transition(thread, _thread_in_vm, saved_state);
  } else {
    {
    ttyLocker ttyl;
    ::tty->print_cr("=============== DEBUG MESSAGE: %s ================", msg);
    ::tty->print_cr(" r0 [   arg0    ] = 0x%08x", regs[1]);
    ::tty->print_cr(" r1 [   arg1    ] = 0x%08x", regs[2]);
    ::tty->print_cr(" r2 [   arg2    ] = 0x%08x", regs[3]);
    ::tty->print_cr(" r3 [   arg3    ] = 0x%08x", regs[4]);
    ::tty->print_cr(" r4 [ rdispatch ] = 0x%08x", regs[5]);
    ::tty->print_cr(" r5 [   rbcp    ] = 0x%08x", regs[6]);
    ::tty->print_cr(" r6 [  rlocals  ] = 0x%08x", regs[7]);
    ::tty->print_cr(" r7 [  rcpool   ] = 0x%08x", regs[8]);
    ::tty->print_cr(" r8 [  rthread  ] = 0x%08x", regs[9]);
    ::tty->print_cr(" r9 [ rscratch1 ] = 0x%08x", regs[10]);
    ::tty->print_cr("r10 [  rmethod  ] = 0x%08x", regs[11]);
    ::tty->print_cr("r11 [    rfp    ] = 0x%08x", regs[12]);
    ::tty->print_cr("r12 [ rscratch2 ] = 0x%08x", regs[13]);
    ::tty->print_cr("r13 [    sp     ] = 0x%08x", regs[0]);
    ::tty->print_cr("r14 [    lr     ] = 0x%08x", regs[14]);
    ::tty->print_cr("r15 [    pc     ] = 0x%08x", pc);
    }
    assert(false, err_msg("DEBUG MESSAGE: %s", msg));
  }
}

void MacroAssembler::push_CPU_state() {
  // ensure the sp is decremented by the multiple of StackAlignmentInBytes
  sub(sp, sp, 4);
  // if fix this, update also RegisterSaved::save_live_registers and it's map
  push(0x1fff, sp); // integer registers except lr & sp & (aarch32 pc)

  if(hasFPU()) {
    const int nfloat = FPUStateSizeInWords / 2; // saved by pairs
    vstmdb_f64(sp, (1 << nfloat) - 1);
  } else {
    sub(sp, sp, FPUStateSizeInWords * wordSize);
  }
}

void MacroAssembler::pop_CPU_state() {
  if(hasFPU()) {
    const int nfloat = FloatRegisterImpl::number_of_registers / 2;
    vldmia_f64(sp, (1 << nfloat) - 1);
  } else {
    add(sp, sp, FPUStateSizeInWords * wordSize);
  }

  pop(0x1fff, sp); // integer registers except lr & sp & (aarch32 pc)
  add(sp, sp, 4);
}

// appears this needs to round up!
void MacroAssembler::round_to(Register reg, int modulus) {
  // from x86
  add(reg, reg, modulus - 1);
  bic(reg, reg, modulus - 1); // and( reg, -modulus)
}

SkipIfEqual::SkipIfEqual(
    MacroAssembler* masm, const bool* flag_addr, bool value) {
  _masm = masm;
  _masm->mov(rscratch1, ExternalAddress((address)flag_addr));
  _masm->ldrb(rscratch1, rscratch1);
  _masm->cmp(rscratch1, 0);
  _masm->b(_label, value ? Assembler::NE : Assembler::EQ);
}

SkipIfEqual::~SkipIfEqual() {
  _masm->bind(_label);
}

void MacroAssembler::cmpptr(Register src1, Address src2) {
  mov(rscratch1, src2);
  ldr(rscratch1, Address(rscratch1));
  cmp(src1, rscratch1);
}

void MacroAssembler::store_check(Register obj) {
  // Does a store check for the oop in register obj. The content of
  // register obj is destroyed afterwards.

  BarrierSet* bs = Universe::heap()->barrier_set();
  assert(bs->kind() == BarrierSet::CardTableModRef, "Wrong barrier set kind");
  CardTableModRefBS* ct = (CardTableModRefBS*)bs;
  assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");

  // The calculation for byte_map_base is as follows:
  // byte_map_base = _byte_map - (uintptr_t(low_bound) >> card_shift);
  // So this essentially converts an address to a displacement and
  // it will never need to be relocated.

  // FIXME: It's not likely that disp will fit into an offset so we
  // don't bother to check, but it could save an instruction.
  intptr_t disp = (intptr_t) ct->byte_map_base;
  mov(rscratch1, disp);
  assert((disp & 0xff) == 0, "fix store char 0 below");
  strb(rscratch1, Address(rscratch1, obj, lsr((int) CardTableModRefBS::card_shift)));
}

void MacroAssembler::store_check(Register obj, Address dst) {
  store_check(obj);
}

// split the store check operation so that other instructions can be scheduled inbetween
void MacroAssembler::store_check_part_1(Register obj) {
  ShouldNotCallThis();
}

void MacroAssembler::store_check_part_2(Register obj) {
  ShouldNotCallThis();
}

void MacroAssembler::load_klass(Register dst, Register src) {
  ldr(dst, Address(src, oopDesc::klass_offset_in_bytes()));
}

void MacroAssembler::cmp_klass(Register oop, Register trial_klass, Register tmp) {
  ldr(tmp, Address(oop, oopDesc::klass_offset_in_bytes()));
  cmp(trial_klass, tmp);
}

void MacroAssembler::load_prototype_header(Register dst, Register src) {
  load_klass(dst, src);
  ldr(dst, Address(dst, Klass::prototype_header_offset()));
}

void MacroAssembler::store_klass(Register dst, Register src) {
  str(src, Address(dst, oopDesc::klass_offset_in_bytes()));
}

void MacroAssembler::store_klass_gap(Register dst, Register src) { }

void MacroAssembler::load_heap_oop(Register dst, Address src)
{
  ldr(dst, src);
}

void MacroAssembler::load_heap_oop_not_null(Register dst, Address src)
{
  ldr(dst, src);
}

void MacroAssembler::store_heap_oop(Address dst, Register src) {
  str(src, dst);
}

// Used for storing NULLs.
void MacroAssembler::store_heap_oop_null(Address dst) {
  mov(rscratch1, 0);
  str(rscratch1, dst);
}

void MacroAssembler::resolve_jobject(Register value,
                                     Register thread,
                                     Register tmp) {
     Label done, not_weak;
    cbz(value, done);           // Use NULL as-is.
    STATIC_ASSERT(JNIHandles::weak_tag_mask == 1u);
    tbz(value, 0, not_weak);    // Test for jweak tag.
    // Resolve jweak.
    ldr(value, Address(value, -JNIHandles::weak_tag_value));
    verify_oop(value);
#if INCLUDE_ALL_GCS
    if (UseG1GC) {
      g1_write_barrier_pre(noreg /* obj */,
                              value /* pre_val */,
                              thread /* thread */,
                              tmp   /* tmp */,
                              true /* tosca_live */,
                              true /* expand_call */);
    }
#endif // INCLUDE_ALL_GCS
    b(done);
    bind(not_weak);
    // Resolve (untagged) jobject.
    ldr(value, Address(value, 0));
    verify_oop(value);
    bind(done);
}

void MacroAssembler::clear_jweak_tag(Register possibly_jweak) {
  // If mask changes we need to ensure that the inverse is still encodable as an immediate
  STATIC_ASSERT(JNIHandles::weak_tag_mask == 1);
  bfc(possibly_jweak, 0, 1);
}


#if INCLUDE_ALL_GCS
void MacroAssembler::g1_write_barrier_pre(Register obj,
                                          Register pre_val,
                                          Register thread,
                                          Register tmp,
                                          bool tosca_live,
                                          bool expand_call) {
  // If expand_call is true then we expand the call_VM_leaf macro
  // directly to skip generating the check by
  // InterpreterMacroAssembler::call_VM_leaf_base that checks _last_sp.

  assert(thread == rthread, "must be");

  Label done;
  Label runtime;

  assert(pre_val != noreg, "check this code");

  if (obj != noreg)
    assert_different_registers(obj, pre_val, tmp);

  Address in_progress(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_active()));
  Address index(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_index()));
  Address buffer(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_buf()));


  // Is marking active?
  if (in_bytes(PtrQueue::byte_width_of_active()) == 4) {
    ldr(tmp, in_progress);
  } else {
    assert(in_bytes(PtrQueue::byte_width_of_active()) == 1, "Assumption");
    ldrb(tmp, in_progress);
  }
  cmp(tmp, 0);
  b(done, Assembler::EQ);

  // Do we need to load the previous value?
  if (obj != noreg) {
    load_heap_oop(pre_val, Address(obj, 0));
  }

  // Is the previous value null?
  cbz(pre_val, done);

  // Can we store original value in the thread's buffer?
  // Is index == 0?
  // (The index field is typed as size_t.)

  ldr(tmp, index);                      // tmp := *index_adr
  cbz(tmp, runtime);                    // tmp == 0?
                                        // If yes, goto runtime

  sub(tmp, tmp, wordSize);              // tmp := tmp - wordSize
  str(tmp, index);                      // *index_adr := tmp
  ldr(rscratch1, buffer);
  add(tmp, tmp, rscratch1);             // tmp := tmp + *buffer_adr

  // Record the previous value
  str(pre_val, Address(tmp, 0));
  b(done);

  bind(runtime);
  // save the live input values
  push(r0->bit(tosca_live) | obj->bit(obj != noreg) | pre_val->bit(true), sp);

  // Calling the runtime using the regular call_VM_leaf mechanism generates
  // code (generated by InterpreterMacroAssember::call_VM_leaf_base)
  // that checks that the *(rfp+frame::interpreter_frame_last_sp) == NULL.
  //
  // If we care generating the pre-barrier without a frame (e.g. in the
  // intrinsified Reference.get() routine) then ebp might be pointing to
  // the caller frame and so this check will most likely fail at runtime.
  //
  // Expanding the call directly bypasses the generation of the check.
  // So when we do not have have a full interpreter frame on the stack
  // expand_call should be passed true.

  if (expand_call) {
    assert(pre_val != c_rarg1, "smashed arg");
    pass_arg1(this, thread);
    pass_arg0(this, pre_val);
    MacroAssembler::call_VM_leaf_base(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_pre), 2);
  } else {
    call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_pre), pre_val, thread);
  }

  pop(r0->bit(tosca_live) | obj->bit(obj != noreg) | pre_val->bit(true), sp);

  bind(done);
}

void MacroAssembler::g1_write_barrier_post(Register store_addr,
                                           Register new_val,
                                           Register thread,
                                           Register tmp,
                                           Register tmp2) {
  assert(thread == rthread, "must be");

  Address queue_index(thread, in_bytes(JavaThread::dirty_card_queue_offset() +
                                       PtrQueue::byte_offset_of_index()));
  Address buffer(thread, in_bytes(JavaThread::dirty_card_queue_offset() +
                                       PtrQueue::byte_offset_of_buf()));

  BarrierSet* bs = Universe::heap()->barrier_set();
  CardTableModRefBS* ct = (CardTableModRefBS*)bs;
  assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");

  Label done;
  Label runtime;

  // Does store cross heap regions?

  eor(tmp, store_addr, new_val);
  lsr(tmp, tmp, HeapRegion::LogOfHRGrainBytes);
  cbz(tmp, done);

  // crosses regions, storing NULL?

  cbz(new_val, done);

  // storing region crossing non-NULL, is card already dirty?


  assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");
  const Register card_addr = tmp;

  lsr(card_addr, store_addr, CardTableModRefBS::card_shift);

  //ExternalAddress cardtable((address) ct->byte_map_base);
  mov(tmp2, (unsigned)ct->byte_map_base);

  // get the address of the card
  add(card_addr, card_addr, tmp2);
  ldrb(tmp2, Address(card_addr));
  cmp(tmp2, (int)G1SATBCardTableModRefBS::g1_young_card_val());
  b(done, Assembler::EQ);

  assert((int)CardTableModRefBS::dirty_card_val() == 0, "must be 0");

  membar(Assembler::StoreLoad);

  ldrb(tmp2, Address(card_addr));
  cmp(tmp2, 0);
  b(done, Assembler::EQ);

  // storing a region crossing, non-NULL oop, card is clean.
  // dirty card and log.
  mov(rscratch1, 0);
  strb(rscratch1, Address(card_addr));

  ldr(rscratch1, queue_index);
  cbz(rscratch1, runtime);
  sub(rscratch1, rscratch1, wordSize);
  str(rscratch1, queue_index);

  ldr(tmp2, buffer);
  str(card_addr, Address(tmp2, rscratch1));
  b(done);

  bind(runtime);
  // save the live input values
  push(store_addr->bit(true) | new_val->bit(true), sp);
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_post), card_addr, thread);
  pop(store_addr->bit(true) | new_val->bit(true), sp);

  bind(done);
}

#endif // INCLUDE_ALL_GCS

Address MacroAssembler::allocate_metadata_address(Metadata* obj) {
  assert(oop_recorder() != NULL, "this assembler needs a Recorder");
  int index = oop_recorder()->allocate_metadata_index(obj);
  RelocationHolder rspec = metadata_Relocation::spec(index);
  return Address((address)obj, rspec);
}

// Move an oop into a register.  immediate is true if we want
// immediate instrcutions, i.e. we are not going to patch this
// instruction while the code is being executed by another thread.  In
// that case we can use move immediates rather than the constant pool.
void MacroAssembler::movoop(Register dst, jobject obj, bool immediate) {
  int oop_index;
  if (obj == NULL) {
    oop_index = oop_recorder()->allocate_oop_index(obj);
  } else {
    oop_index = oop_recorder()->find_index(obj);
    assert(Universe::heap()->is_in_reserved(JNIHandles::resolve(obj)), "should be real oop");
  }
  if (! immediate) {
    far_load_oop(dst, oop_index);
  } else {
    RelocationHolder rspec = oop_Relocation::spec(oop_index);
    mov(dst, Address((address)obj, rspec));
  }
}

// Move a metadata address into a register.
void MacroAssembler::mov_metadata(Register dst, Metadata* obj) {
  int oop_index;
  if (obj == NULL) {
    oop_index = oop_recorder()->allocate_metadata_index(obj);
  } else {
    oop_index = oop_recorder()->find_index(obj);
  }
  RelocationHolder rspec = metadata_Relocation::spec(oop_index);
  mov(dst, Address((address)obj, rspec));
}

void MacroAssembler::far_load(Register dst, address addr) {
  address far_load_addr = pc();
  add(dst, r15_pc, 0);
  ldr(dst, Address(dst));

  NativeFarLdr* far_load = (NativeFarLdr*) far_load_addr;
  far_load->set_data_addr((intptr_t*) addr);
}

void MacroAssembler::far_load_oop(Register dst, int oop_index) {
    relocate(oop_Relocation::spec(oop_index));
    // can't provide meaningful addr, give far_load addr itself
    far_load(dst, pc());
}

void MacroAssembler::far_load_metadata(Register dst, int metadata_index) {
    relocate(metadata_Relocation::spec(metadata_index));
    // can't provide meaningful addr, give far_load addr itself
    far_load(dst, pc());
}

void MacroAssembler::far_load_const(Register dst, address const_addr) {
    relocate(section_word_Relocation::spec(const_addr, CodeBuffer::SECT_CONSTS));
    far_load(dst, const_addr);
}

Address MacroAssembler::constant_oop_address(jobject obj) {
  assert(oop_recorder() != NULL, "this assembler needs an OopRecorder");
  assert(Universe::heap()->is_in_reserved(JNIHandles::resolve(obj)), "not an oop");
  int oop_index = oop_recorder()->find_index(obj);
  return Address((address)obj, oop_Relocation::spec(oop_index));
}

// Defines obj, preserves var_size_in_bytes, okay for t2 == var_size_in_bytes.
void MacroAssembler::tlab_allocate(Register obj,
                                   Register var_size_in_bytes,
                                   int con_size_in_bytes,
                                   Register t1,
                                   Register t2,
                                   Label& slow_case) {
  assert_different_registers(obj, t2);
  assert_different_registers(obj, var_size_in_bytes);
  Register end = t2;

  // verify_tlab();

  ldr(obj, Address(rthread, JavaThread::tlab_top_offset()));
  if (var_size_in_bytes == noreg) {
    lea(end, Address(obj, con_size_in_bytes));
  } else {
    lea(end, Address(obj, var_size_in_bytes));
  }
  ldr(rscratch1, Address(rthread, JavaThread::tlab_end_offset()));
  cmp(end, rscratch1);
  b(slow_case, Assembler::HI);

  // update the tlab top pointer
  str(end, Address(rthread, JavaThread::tlab_top_offset()));

  // recover var_size_in_bytes if necessary
  if (var_size_in_bytes == end) {
    sub(var_size_in_bytes, var_size_in_bytes, obj);
  }
  // verify_tlab();
}

// Preserves r6, and r3.
Register MacroAssembler::tlab_refill(Label& retry,
                                     Label& try_eden,
                                     Label& slow_case) {
  Register top = r0;
  Register t1  = r2;
  Register t2  = r4;
  assert_different_registers(top, rthread, t1, t2, /* preserve: */ r6, r3);
  Label do_refill, discard_tlab;

  if (CMSIncrementalMode || !Universe::heap()->supports_inline_contig_alloc()) {
    // No allocation in the shared eden.
    b(slow_case);
  }

  ldr(top, Address(rthread, in_bytes(JavaThread::tlab_top_offset())));
  ldr(t1,  Address(rthread, in_bytes(JavaThread::tlab_end_offset())));

  // calculate amount of free space
  sub(t1, t1, top);
  lsr(t1, t1, LogHeapWordSize);

  // Retain tlab and allocate object in shared space if
  // the amount free in the tlab is too large to discard.

  ldr(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_refill_waste_limit_offset())));
  cmp(t1, rscratch1);
  b(discard_tlab, Assembler::LE);

  // Retain
  // ldr(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_refill_waste_limit_offset())));
  mov(t2, (int32_t) ThreadLocalAllocBuffer::refill_waste_limit_increment());
  add(rscratch1, rscratch1, t2);
  str(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_refill_waste_limit_offset())));

  if (TLABStats) {
    // increment number of slow_allocations
    addmw(Address(rthread, in_bytes(JavaThread::tlab_slow_allocations_offset())),
         1, rscratch1);
  }
  b(try_eden);

  bind(discard_tlab);
  if (TLABStats) {
    // increment number of refills
    addmw(Address(rthread, in_bytes(JavaThread::tlab_number_of_refills_offset())), 1,
         rscratch1);
    // accumulate wastage -- t1 is amount free in tlab
    addmw(Address(rthread, in_bytes(JavaThread::tlab_fast_refill_waste_offset())), t1,
         rscratch1);
  }

  // if tlab is currently allocated (top or end != null) then
  // fill [top, end + alignment_reserve) with array object
  cbz(top, do_refill);

  // set up the mark word
  mov(rscratch1, (intptr_t)markOopDesc::prototype()->copy_set_hash(0x2));
  str(rscratch1, Address(top, oopDesc::mark_offset_in_bytes()));
  // set the length to the remaining space
  sub(t1, t1, typeArrayOopDesc::header_size(T_INT));
  add(t1, t1, (int32_t)ThreadLocalAllocBuffer::alignment_reserve());
  lsl(t1, t1, log2_intptr(HeapWordSize/sizeof(jint)));
  str(t1, Address(top, arrayOopDesc::length_offset_in_bytes()));
  // set klass to intArrayKlass
  // dubious reloc why not an oop reloc?
  mov(rscratch1, ExternalAddress((address)Universe::intArrayKlassObj_addr()));
  ldr(t1, Address(rscratch1));
  // store klass last.  concurrent gcs assumes klass length is valid if
  // klass field is not null.
  store_klass(top, t1);

  mov(t1, top);
  ldr(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_start_offset())));
  sub(t1, t1, rscratch1);
  incr_allocated_bytes(rthread, t1, 0, rscratch1);

  // refill the tlab with an eden allocation
  bind(do_refill);
  ldr(t1, Address(rthread, in_bytes(JavaThread::tlab_size_offset())));
  lsl(t1, t1, LogHeapWordSize);
  // allocate new tlab, address returned in top
  eden_allocate(top, t1, 0, t2, slow_case);

  // Check that t1 was preserved in eden_allocate.
#ifdef ASSERT
  if (UseTLAB) {
    Label ok;
    Register tsize = r4;
    assert_different_registers(tsize, rthread, t1);
    str(tsize, Address(pre(sp, -16)));
    ldr(tsize, Address(rthread, in_bytes(JavaThread::tlab_size_offset())));
    lsl(tsize, tsize, LogHeapWordSize);
    cmp(t1, tsize);
    b(ok, Assembler::EQ);
    STOP("assert(t1 != tlab size)");
    should_not_reach_here();

    bind(ok);
    ldr(tsize, Address(post(sp, 16)));
  }
#endif
  str(top, Address(rthread, in_bytes(JavaThread::tlab_start_offset())));
  str(top, Address(rthread, in_bytes(JavaThread::tlab_top_offset())));
  add(top, top, t1);
  sub(top, top, (int32_t)ThreadLocalAllocBuffer::alignment_reserve_in_bytes());
  str(top, Address(rthread, in_bytes(JavaThread::tlab_end_offset())));
  verify_tlab();
  b(retry);

  return rthread; // for use by caller
}

// Defines obj, preserves var_size_in_bytes
void MacroAssembler::eden_allocate(Register obj,
                                   Register var_size_in_bytes,
                                   int con_size_in_bytes,
                                   Register t1,
                                   Label& slow_case) {
  assert_different_registers(obj, var_size_in_bytes, t1);
  if (CMSIncrementalMode || !Universe::heap()->supports_inline_contig_alloc()) {
    b(slow_case);
  } else {
    Register end = t1;
    Register heap_end = rscratch2;
    Label retry;
    bind(retry);

    mov(rscratch1, ExternalAddress((address) Universe::heap()->end_addr()));
    ldr(heap_end, Address(rscratch1));

    ExternalAddress heap_top((address) Universe::heap()->top_addr());
    mov(rscratch1, heap_top);
    ldrex(obj, rscratch1);

    // Adjust it my the size of our new object
    if (var_size_in_bytes == noreg) {
      lea(end, Address(obj, con_size_in_bytes));
    } else {
      lea(end, Address(obj, var_size_in_bytes));
    }

    // if end < obj then we wrapped around high memory
    cmp(end, obj);
    b(slow_case, Assembler::LO);

    cmp(end, heap_end);
    b(slow_case, Assembler::HI);

    // If heap_top hasn't been changed by some other thread, update it.
    mov(rscratch2, rscratch1);
    strex(rscratch1, end, rscratch2);
    cmp(rscratch1, 0);
    b(retry, Assembler::NE);
  }
}

void MacroAssembler::verify_tlab() {
#ifdef ASSERT
  if (UseTLAB && VerifyOops) {
    Label next, ok;

    strd(rscratch2, rscratch1, Address(pre(sp, -16)));

    ldr(rscratch2, Address(rthread, in_bytes(JavaThread::tlab_top_offset())));
    ldr(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_start_offset())));
    cmp(rscratch2, rscratch1);
    b(next, Assembler::HS);
    STOP("assert(top >= start)");
    should_not_reach_here();

    bind(next);
    ldr(rscratch2, Address(rthread, in_bytes(JavaThread::tlab_end_offset())));
    ldr(rscratch1, Address(rthread, in_bytes(JavaThread::tlab_top_offset())));
    cmp(rscratch2, rscratch1);
    b(ok, Assembler::HS);
    STOP("assert(top <= end)");
    should_not_reach_here();

    bind(ok);
    ldrd(rscratch2, rscratch1, Address(post(sp, 16)));
  }
#endif
}

// Writes to stack successive pages until offset reached to check for
// stack overflow + shadow pages.  This clobbers tmp.
void MacroAssembler::bang_stack_size(Register size, Register tmp) {
  assert_different_registers(tmp, size, rscratch1);
  mov(tmp, sp);
  // Bang stack for total size given plus shadow page size.
  // Bang one page at a time because large size can bang beyond yellow and
  // red zones.
  Label loop;
  mov(rscratch1, os::vm_page_size());
  bind(loop);
  lea(tmp, Address(tmp, -os::vm_page_size()));
  subs(size, size, rscratch1);
  str(size, Address(tmp));
  b(loop, Assembler::GT);

  // Bang down shadow pages too.
  // At this point, (tmp-0) is the last address touched, so don't
  // touch it again.  (It was touched as (tmp-pagesize) but then tmp
  // was post-decremented.)  Skip this address by starting at i=1, and
  // touch a few more pages below.  N.B.  It is important to touch all
  // the way down to and including i=StackShadowPages.
  for (int i = 0; i< StackShadowPages-1; i++) {
    // this could be any sized move but this is can be a debugging crumb
    // so the bigger the better.
    lea(tmp, Address(tmp, -os::vm_page_size()));
    str(size, Address(tmp));
  }
}


address MacroAssembler::read_polling_page(Register r, address page, relocInfo::relocType rtype) {
  mov(r, Address(page, rtype));
  InstructionMark im(this);
  code_section()->relocate(inst_mark(), rtype);
  ldr(r, Address(r));
  return inst_mark();
}

address MacroAssembler::read_polling_page(Register r, relocInfo::relocType rtype) {
  InstructionMark im(this);
  code_section()->relocate(inst_mark(), rtype);
  // It's ok to load to reg from reg + off (without write-back)
  ldr(r, Address(r, 0));
  return inst_mark();
}

// Helper functions for 64-bit multipliction, division and remainder
// does <Rd+1:Rd> = <Rn+1:Rn> * <Rm+1:Rm>
void MacroAssembler::mult_long(Register Rd, Register Rn, Register Rm) {
  Register Rdh = (Register)(Rd->encoding_nocheck() + 1);
  Register Rnh = (Register)(Rn->encoding_nocheck() + 1);
  Register Rmh = (Register)(Rm->encoding_nocheck() + 1);

  mult_long(Rd, Rdh, Rn, Rnh, Rm, Rmh);
}

// does <Rdh:Rd> = <Rnh:Rn> * <Rmh:Rm>
void MacroAssembler::mult_long(Register Rd, Register Rdh, Register Rn, Register Rnh, Register Rm, Register Rmh) {
  assert_different_registers(Rn, Rnh);
  assert_different_registers(Rm, Rmh);
  assert_different_registers(Rd, Rdh); // umull restriction
  const Register t = rscratch1;

  mul(t, Rm, Rnh);
  mla(t, Rn, Rmh, t);
  umull(Rd, Rdh, Rm, Rn);
  add(Rdh, t, Rdh);
}


int64_t internal_ldiv(int64_t a, int64_t b) {
  return a / b;
}

int64_t internal_lmod(int64_t a, int64_t b) {
  return a % b;
}

void MacroAssembler::divide32(Register res, Register num, Register den, bool want_mod) {
    Register cnt = rscratch1;
    Register mod = rscratch2;
    Register sign = r14;
    assert_different_registers(num, den, rscratch1, rscratch2, r14);

    // FIXME This works by first converting any negative values to positive ones, however
    // it is not possible to express |INT_MIN|. Need to fix this

    //Convert to positive values
    mov(sign, 0);

    cmp(num, 0);
    mov(sign, 1, MI);
    rsb(num, num, 0, MI);

    cmp(den, 0);
    if(!want_mod) eor(sign, sign, 1, MI);
    rsb(den, den, 0, MI);

    // Algorithm from
    // http://www.chiark.greenend.org.uk/~theom/riscos/docs/ultimate/a252div.txt
    // Graeme Williams
    mov(cnt, 28);
    mov(mod, num, lsr(4));
    cmp(den, mod, lsr(12));
    sub(cnt, cnt, 16, Assembler::LE);
    mov(mod, mod, lsr(16), Assembler::LE);
    cmp(den, mod, lsr(4));
    sub(cnt, cnt, 8, Assembler::LE);
    mov(mod, mod, lsr(8), Assembler::LE);
    cmp(den, mod);
    sub(cnt, cnt, 4, Assembler::LE);
    mov(mod, mod, lsr(4), Assembler::LE);
    mov(num, num, lsl(cnt));
    rsb(den, den, 0);

    adds(num, num, num);
    //Now skip over cnt copies of the 3 instr. loop.
    add(cnt, cnt, cnt, lsl(1));
    add(r15_pc, r15_pc, cnt, lsl(2));
    mov(r0, r0);

    for(int i = 0; i < 32; i++) {
        adcs(mod, den, mod, lsl(1));
        sub(mod, mod, den, Assembler::LO);
        adcs(num, num, num);
    }

    cmp(sign, 0);
    rsb(res, want_mod? mod : num, 0, NE);
    mov(res, want_mod? mod : num, EQ);
}


// <Rd+1:Rd> = <Rn+1:Rn> / <Rm+1:Rm>
// <Rd+1:Rd> = <Rn+1:Rn> % <Rm+1:Rm>
// <Rd> = <Rn> / <Rm>
// <Rd> = <Rn> % <Rm>
void MacroAssembler::divide(Register Rd, Register Rn, Register Rm, int width, bool want_remainder) {
  //Dispatch to best possible
  Register Rdh = (Register)(Rd->encoding_nocheck() + 1);
  Register Rnh = (Register)(Rn->encoding_nocheck() + 1);
  Register Rmh = (Register)(Rm->encoding_nocheck() + 1);

  assert(32 == width || 64 == width, "Invalid width");
  bool is64b = 64 == width;

  if(is64b) {
    assert_different_registers(Rn, Rnh, Rm, Rmh, rscratch1, rscratch2);
  }

  if(!is64b && VM_Version::features() & FT_HW_DIVIDE) {
    // Emit a hw instruction sequnce.
    if(want_remainder) {
      sdiv(rscratch1, Rn, Rm);
      mls(Rd, rscratch1, Rm, Rn);
    } else {
      sdiv(Rd, Rn, Rm);
    }
  } else if(!is64b) {
    // Fall back to assembly software routine
    divide32(Rd, Rn, Rm, want_remainder);
  } else {
    // Fall back to C software routine for
    // 64 bit divide/mod
    if(Rn != r0) {
      mov(rscratch1, Rm);
      mov(rscratch2, Rmh);

      mov(r0, Rn);
      mov(r1, Rnh);

      mov(r2, rscratch1);
      mov(r3, rscratch2);
    } else if(Rm != r2) {
      mov(r2, Rm);
      mov(r3, Rmh);
    }
    address function;
    if(want_remainder) function = (address)internal_lmod;
    else               function = (address)internal_ldiv;

    mov(rscratch1, function);
    bl(rscratch1);
    if(Rd != r0) {
      mov(Rd, r0);
      if(is64b) mov(Rdh, r1);
    }
  }
}

void MacroAssembler::extract_bits(Register dest, Register source, int lsb, int width) {
  assert(lsb >= 0 && lsb + width <= 32 && width != 0, "Invalid lsb/width");
  // Dispatch to the best sequence
  if(0 == (lsb & 7) && (width == 8 || width == 16 || width == 32)) {
    // Can use extend X
    switch(width){
      case 8:  uxtb(dest, source, ror(lsb)); break;
      case 16: uxth(dest, source, ror(lsb)); break;
      default:                               break;
   }
  } else if(VM_Version::features() & (FT_ARMV7 | FT_ARMV6T2)) {
    ubfx(dest, source, lsb, width);
  } else {
    // Do two shifts
    lsl(dest, source, 32 - (width + lsb));
    lsr(dest, dest, 32 - width);
  }
}


void MacroAssembler::atomic_ldrd(Register Rt, Register Rt2, Register Rbase) {
  assert(Rt->encoding_nocheck() % 2 == 0, "Must be an even register");
  assert((Register) (Rt + 1) == Rt2, "Must be contiguous");
  if(VM_Version::features() & FT_SINGLE_CORE) {
    ldrd(Rt, Rbase);
  } else if (VM_Version::features() & (FT_ARMV7 | FT_ARMV6K)) {
#ifdef ASSERT
    Label lbl;
    tst(Rbase, 7);
    b(lbl, EQ);
    stop("atomic_ldrd is not doubleword aligned!");
    bind(lbl);
#endif // ASSERT

    ldrexd(Rt, Rbase);
  } else {
    // TODO: Find Java way of logging
    static bool warning_printed = false;
    if(!warning_printed) {
      fprintf(stderr, "Unable to provide atomic doubleword load.\n");
      warning_printed = true;
    }
    ldrd(Rt, Rbase);
  }
}

void MacroAssembler::atomic_strd(Register Rt, Register Rt2, Register Rbase,
                                 Register temp, Register temp2) {
  assert(Rt->encoding_nocheck() % 2 == 0, "Must be an even register");
  assert((Register) (Rt + 1) == Rt2, "Must be contiguous");
  assert((Register) (temp + 1) == temp2, "Must be contiguous");
  assert_different_registers(temp, Rt, Rbase, temp2);
  if(VM_Version::features() & FT_SINGLE_CORE) {
    strd(Rt, Rbase);
  } else if (VM_Version::features() & (FT_ARMV7 | FT_ARMV6K)) {
    // First need to gain exclusive access
    Label retry;

#ifdef ASSERT
    tst(Rbase, 7);
    b(retry, EQ);
    stop("atomic_strd is not doubleword aligned!");
#endif // ASSERT

    bind(retry);
    ldrexd(temp, Rbase);
    strexd(temp, Rt, Rbase);
    cmp(temp, 0);
    b(retry, NE);
  } else {
    // TODO: Find Java way of logging
    static bool warning_printed = false;
    if(!warning_printed) {
      fprintf(stderr, "Unable to provide atomic doubleword store.\n");
      warning_printed = true;
    }
    strd(Rt, Rbase);
  }
}


#define ENABLE_DEBUGGING 0
// Helloworld is 2,482,397
uint32_t MacroAssembler::bytecodes_until_print = 2400000; //13795328; //6888000L; //6881772L;

uint32_t MacroAssembler::bytecodes_executed = 0;

int MacroAssembler::enable_debug = 0;
int MacroAssembler::enable_method_debug = 0;
int MacroAssembler::enable_debugging_static = ENABLE_DEBUGGING;

#define N_J_BYTECODES 234
const char* j_bytecodes[N_J_BYTECODES] = {"nop", "aconstnull", "iconstm1", "iconst0", "iconst1", "iconst2", "iconst3", "iconst4", "iconst5", "lconst0",
"lconst1", "fconst0", "fconst1", "fconst2", "dconst0", "dconst1", "bipush", "sipush", "ldc", "ldcw", "ldc2w",
"iload", "lload", "fload", "dload", "aload", "iload0", "iload1", "iload2", "iload3", "lload0", "lload1", "lload2",
"lload3", "fload0", "fload1", "fload2", "fload3", "dload0", "dload1", "dload2", "dload3", "aload0", "aload1", "aload2",
"aload3", "iaload", "laload", "faload", "daload", "aaload", "baload", "caload", "saload", "istore", "lstore", "fstore",
"dstore", "astore", "istore0", "istore1", "istore2", "istore3", "lstore0", "lstore1", "lstore2", "lstore3", "fstore0",
"fstore1", "fstore2", "fstore3", "dstore0", "dstore1", "dstore2", "dstore3", "astore0", "astore1", "astore2", "astore3",
"iastore", "lastore", "fastore", "dastore", "aastore", "bastore", "castore", "sastore", "pop", "pop2", "dup", "dupx1",
"dupx2", "dup2", "dup2x1", "dup2x2", "swap", "iadd", "ladd", "fadd", "dadd", "isub", "lsub", "fsub", "dsub", "imul",
"lmul", "fmul", "dmul", "idiv", "ldiv", "fdiv", "ddiv", "irem", "lrem", "frem", "drem", "ineg", "lneg", "fneg", "dneg",
"ishl", "lshl", "ishr", "lshr", "iushr", "lushr", "iand", "land", "ior", "lor", "ixor", "lxor", "iinc", "i2l", "i2f",
"i2d", "l2i", "l2f", "l2d", "f2i", "f2l", "f2d", "d2i", "d2l", "d2f", "i2b", "i2c", "i2s", "lcmp", "fcmpl", "fcmpg",
"dcmpl", "dcmpg", "ifeq", "ifne", "iflt", "ifge", "ifgt", "ifle", "ificmpeq", "ificmpne", "ificmplt", "ificmpge",
"ificmpgt", "ificmple", "ifacmpeq", "ifacmpne", "goto", "jsr", "ret", "tableswitch", "lookupswitch", "ireturn",
"lreturn", "freturn", "dreturn", "areturn", "return", "getstatic", "putstatic", "getfield", "putfield",
"invokevirtual", "invokespecial", "invokestatic", "invokeinterface", "invokedynamic", "new", "newarray",
"anewarray", "arraylength", "athrow", "checkcast", "instanceof", "monitorenter", "monitorexit", "wide",
"multianewarray", "ifnull", "ifnonnull", "gotow", "jsrw", "breakpoint", "fast_agetfield", "fast_bgetfield",
"fast_cgetfield", "fast_dgetfield", "fast_fgetfield", "fast_igetfield", "fast_lgetfield", "fast_sgetfield",
"fast_aputfield", "fast_bputfield", "fast_cputfield", "fast_dputfield", "fast_fputfield", "fast_iputfield",
"fast_lputfield", "fast_sputfield", "fast_aload_0", "fast_iaccess_0", "fast_aaccess_0", "fast_faccess_0",
"fast_iload", "fast_iload2", "fast_icaload", "fast_invokevfinal", "fast_linearswitch", "fast_binaryswitch",
"fast_aldc", "fast_aldc_w", "return_register_finalizer", "invokehandle", "INVALID"};

int bytecodes_seen[256];

void MacroAssembler::init_unseen_bytecodes() {
  for(int i = 0; i < 256; i++ ) {
    bytecodes_seen[i] = 0;
  }
}

void MacroAssembler::bytecode_seen(Register bc_reg, Register scratch) {
  if(ENABLE_DEBUGGING) {
    mov(scratch, (address)bytecodes_seen);
    add(scratch, scratch, bc_reg, lsl(2));
    add(bc_reg, bc_reg, 1);
    str(bc_reg, Address(scratch));
    sub(bc_reg, bc_reg, 1);
  }
}

void MacroAssembler::print_unseen_bytecodes() {
  if(ENABLE_DEBUGGING) {
    printf("=== Unseen bytecodes ===\n");
    for(int i = 0; i < N_J_BYTECODES; i++) {
      if(0 == bytecodes_seen[i]) {
        printf("\t%s\n", j_bytecodes[i]);
      }
    }
    printf("=== End unseen ===\n");
  } else {
    printf("Not kept track, enable debugging to view info\n");
  }
  fflush(stdout);
}

int machine_state_regset = 0b0101111111111111;
int machine_state_float_regset = 0b11;

void MacroAssembler::save_machine_state() {
    stmdb(sp, machine_state_regset);
    if(hasFPU()) {
        vstmdb_f64(sp, machine_state_float_regset);
    }
    enter();
}

void MacroAssembler::restore_machine_state() {
    leave();
    if(hasFPU()) {
        vldmia_f64(sp, machine_state_float_regset);
    }
    ldmia(sp, machine_state_regset);
}

void internal_internal_printf(const char *fmt, ...) {
  va_list args;
  va_start (args, fmt);
  vprintf (fmt, args);
  fflush(stdout);
  va_end(args);
}

void internal_printf(const char *format, uint32_t a, uint32_t b, uint32_t c) {
  char buf[2048];
  char fmt[2048];
  buf[0] = '\0';
  const char *thread_str = "THREAD 0x%08x : ";
  int id = pthread_self();
  strcpy(fmt, format);

  char *str = strtok(fmt, "\n");
  int nreplace = 0;
  while(str) {
    strcpy(buf, thread_str);
    strcat(buf, str);
    strcat(buf, "\n");
    internal_internal_printf((const char*)buf, id, a, b, c);
    str = strtok(NULL, "\n");
  }
}

void MacroAssembler::get_bytecode(Register dst, Register bc) {
  if(ENABLE_DEBUGGING) {
    int nbytecodes = N_J_BYTECODES;
    mov(dst, (address)j_bytecodes);
    cmp(bc, nbytecodes);

    ldr(dst, Address(dst, bc, lsl(2)), Assembler::LT);
    ldr(dst, Address(dst, wordSize * nbytecodes), Assembler::GE);
  }
}

int invocation_depth_count = -1; //TODO remove this with debugging info

#define MAX_FCALL_DEPTH 4096
struct thread_method_record{
  int thread_id;
  char names[MAX_FCALL_DEPTH][512];
  int invocation_depth_count;
};
int ntmrs = 0;
#define MAX_TMRS 10
thread_method_record tmr_list[MAX_TMRS];

void push_tmr(Method *meth, int *thread_id, int *invocation_depth_count, char **name) {
  int id = pthread_self();
  *thread_id = id;
  for(int i = 0; i < ntmrs; i++) {
    thread_method_record *tmr = &tmr_list[i];
    if(id == tmr->thread_id) {
      // Add a new frame
      if(tmr->invocation_depth_count >= -1 &&
        tmr->invocation_depth_count < (MAX_FCALL_DEPTH - 1)) {
        *invocation_depth_count = ++(tmr->invocation_depth_count);
        *name = tmr->names[tmr->invocation_depth_count];
        meth->name_and_sig_as_C_string(tmr->names[tmr->invocation_depth_count], 512);
        return;
      } else {
        fprintf(stderr, "%s : Invalid fcall depth index, %d\n", __FUNCTION__, tmr->invocation_depth_count);
        exit(1);
      }
    }
  }
  // Add a new thread
  if(ntmrs >= MAX_TMRS) {
    fprintf(stderr, "Too many tmrs\n");
    exit(1);
  }
  //Create a new tmr
  tmr_list[ntmrs].thread_id = id;
  tmr_list[ntmrs].invocation_depth_count = 0;
  meth->name_and_sig_as_C_string(tmr_list[ntmrs].names[0], 512);
  *invocation_depth_count = 0;
  *name = tmr_list[ntmrs].names[0];
  ntmrs++;
}

void pop_tmr(int *thread_id, int *invocation_depth_count, char **name) {
  int id = pthread_self();
  *thread_id = id;
  for(int i = 0; i < ntmrs; i++) {
    thread_method_record *tmr = &tmr_list[i];
    if(id == tmr->thread_id) {
      if(tmr->invocation_depth_count >= 0 &&
        tmr->invocation_depth_count < MAX_FCALL_DEPTH) {
        // Pop frame
        *name = tmr->names[tmr->invocation_depth_count];
        *invocation_depth_count = (tmr->invocation_depth_count)--;
        return;
      } else if ( -1 == tmr->invocation_depth_count) {
        *name = (char*)"JVM-EXCEPTION-EXIT:(NOT-REALLY-A-FRAME)";
        *invocation_depth_count = 0;
        return;
      } else {
        fprintf(stderr, "%s : Invalid fcall depth index, %d\n", __FUNCTION__, tmr->invocation_depth_count);
        exit(1);
      }
    }
  }
  fprintf(stderr, "Unable to find suitable tmr\n");
  exit(1);
}

void prepare_entry_exit_prefix(char *buf, int id, int invocation_depth_count) {
  sprintf(buf, "THREAD 0x%08x : ", id);
  for(int i = 0; i < invocation_depth_count; i++) {
    strcat(buf, "  ");
  }
}


void print_entry(Method *meth, int native) {
  char *name;
  int invocation_depth_count, id;
  push_tmr(meth, &id, &invocation_depth_count, &name);

  if(MacroAssembler::enable_method_debug) {
    char buf[4096], buf_b[2048];
    prepare_entry_exit_prefix(buf, id, invocation_depth_count);
    if(native) {
      sprintf(buf_b, "CALL NATIVE : %s\n", name);
    } else {
      sprintf(buf_b, "CALL JAVA   : %s\n", name);
    }
    strcat(buf, buf_b);
    printf("%s", buf);
    fflush(stdout);
  }
}

void print_exit(bool normal) {
  char *name;
  int invocation_depth_count, id;
  pop_tmr(&id, &invocation_depth_count, &name);

  if(MacroAssembler::enable_method_debug) {
    char buf[4096], buf_b[2048];
    prepare_entry_exit_prefix(buf, id, invocation_depth_count);
    sprintf(buf_b, normal ? "EXIT        : %s\n" : "EXCPN EXIT  : %s\n", name);
    strcat(buf, buf_b);
    printf("%s", buf);
    fflush(stdout);
  }
}

void MacroAssembler::print_method_entry(Register rmethod, bool native) {
  if(ENABLE_DEBUGGING) {
    save_machine_state();

    bic(sp, sp, 7); // 8-byte align stack
    mov(rscratch2, (address)print_entry);
    mov(r0, rmethod);
    mov(r1, native);
    bl(rscratch2);

    restore_machine_state();
  }
}

void MacroAssembler::print_method_exit(bool normal) {
  if(ENABLE_DEBUGGING) {
    save_machine_state();

    bic(sp, sp, 7); // 8-byte align stack
    mov(rscratch2, (address)print_exit);
    mov(r0, normal);
    bl(rscratch2);

    restore_machine_state();
  }
}

void MacroAssembler::reg_printf_internal(bool important, const char *fmt, Register ra, Register rb, Register rc) {
  if(ENABLE_DEBUGGING) {
    Label skip;
    save_machine_state();

        mov(rscratch1, ra);
        str(rscratch1, Address(pre(sp, -wordSize)));
        mov(rscratch1, rb);
        str(rscratch1, Address(pre(sp, -wordSize)));
        mov(rscratch1, rc);
        str(rscratch1, Address(pre(sp, -wordSize)));

        if(!important) {
            mov(r0, (address)&enable_debug);
            ldr(r0, Address(r0));
            cmp(r0, 0);
            b(skip, Assembler::EQ);
        }

        int sp_difference = wordSize * (count_bits(machine_state_regset) +
                                        2 * count_bits(machine_state_float_regset) +
                                        2 + 3); //Frame entry and saved

        mov(r0, (address)fmt);
        if(ra != sp) ldr(r1, Address(sp, 2 * wordSize));
        else         add(r1, sp, sp_difference);

        if(rb != sp) ldr(r2, Address(sp, wordSize));
        else         add(r2, sp, sp_difference);

        if(rc != sp) ldr(r3, Address(sp));
        else         add(r3, sp, sp_difference);

        bic(sp, sp, 7); // 8-byte align stack

        mov(rscratch2, (address)internal_printf);
        bl(rscratch2);

        bind(skip);
        restore_machine_state();
    }
}

void MacroAssembler::reg_printf(const char *fmt, Register ra, Register rb, Register rc) {
  reg_printf_internal(false, fmt, ra, rb, rc);
}

void MacroAssembler::reg_printf_important(const char *fmt, Register ra, Register rb, Register rc) {
  reg_printf_internal(true, fmt, ra, rb, rc);
}

// When debugging, set the break on bkpnt
void bkpnt() { return; }
void MacroAssembler::create_breakpoint() {
    if(ENABLE_DEBUGGING) {
        save_machine_state();
        bic(sp, sp, 7); // 8-byte align stack

        mov(rscratch2, (address) bkpnt);
        bl(rscratch2);

        restore_machine_state();
    }
}


void MacroAssembler::print_cpool(InstanceKlass *klass) {
    ttyLocker ttyl;
    klass->constants()->print_on(tty);
}

int MacroAssembler::ldrd(Register Rt, Register Rt2, const Address& adr, Register Rtmp, Condition cond) {
    if((0 == Rt->encoding_nocheck() % 2 &&
         (Rt->encoding_nocheck() + 1 == Rt2->encoding_nocheck())) &&
      (uabs(adr.offset()) < (1 << 8))) {
      /* Good to go with a ldrd */
      ldrd(Rt, adr, cond);
      return 0x0;
    } else {
      return double_ld_failed_dispatch(Rt, Rt2, adr, &Assembler::ldm,
                                &Assembler::ldr, Rtmp, cond);
    }
}

int MacroAssembler::strd(Register Rt, Register Rt2, const Address& adr, Condition cond) {
    if((0 == Rt->encoding_nocheck() % 2 &&
         (Rt->encoding_nocheck() + 1 == Rt2->encoding_nocheck())) &&
      (uabs(adr.offset()) < (1 << 8))) {
      /* Good to go with a strd */
      strd(Rt, adr, cond);
    } else {
      double_ldst_failed_dispatch(Rt, Rt2, adr, &Assembler::stm, &Assembler::str, cond);
    }
    return 0x0;
}

int MacroAssembler::double_ld_failed_dispatch(Register Rt, Register Rt2, const Address& adr,
        void (Assembler::* mul)(unsigned, const Address&, Condition),
        void (Assembler::* sgl)(Register, const Address&, Condition),
        Register Rtmp, Condition cond) {
  if (can_ldst_multiple(RegSet::of(Rt, Rt2).bits(), adr) &&
          (Rt->encoding_nocheck() < Rt2->encoding_nocheck())) {
    /* Do a load or store multiple instruction */
    (this->*mul)(RegSet::of(Rt, Rt2).bits(), adr, cond);
  } else if (!adr.uses(Rt)) {
    double_ldst_failed_dispatch(Rt, Rt2, adr, mul, sgl, cond);
  } else {
    // need to reshuffle operation, otherwise write to Rt destroys adr
    if (adr.get_mode() != Address::reg) {
      // offset-based addressing. hence Rt2 could not be by adr
      if (adr.get_wb_mode() == Address::pre) {
        (this->*sgl)(Rt2, Address(pre(adr.base(), adr.offset() + wordSize)), cond);
        (this->*sgl)(Rt, Address(pre(adr.base(), -wordSize)), cond);
      } else if (adr.get_wb_mode() == Address::post) {
        (this->*sgl)(Rt2, Address(adr.base(), adr.offset() + wordSize), cond);
        (this->*sgl)(Rt, adr, cond);
      } else if (adr.get_wb_mode() == Address::off) {
        (this->*sgl)(Rt2, Address(adr.base(), adr.offset() + wordSize), cond);
        (this->*sgl)(Rt, adr, cond);
      } else {
        ShouldNotReachHere();
      }
    } else {
      // index-based addressing. both Rt and Rt2 could be used by adr
      // hence temp register is necessary
      adr.lea(this, Rtmp);
      double_ldst_failed_dispatch(Rt, Rt2, Address(Rtmp), mul, sgl, cond);
      // adr.lea have only address manipulation and cannot cause trap.
      // first instruction when NPE can occur is in double_ldst_failed_dispatch
      // so shift offset appropriately
      return 0x4;
    }
  }
  return 0x0;
}

void MacroAssembler::double_ldst_failed_dispatch(Register Rt, Register Rt2, const Address& adr,
        void (Assembler::* mul)(unsigned, const Address&, Condition),
        void (Assembler::* sgl)(Register, const Address&, Condition),
        Condition cond) {
  if (can_ldst_multiple(RegSet::of(Rt, Rt2).bits(), adr) &&
          (Rt->encoding_nocheck() < Rt2->encoding_nocheck())) {
    /* Do a store multiple instruction */
    (this->*mul)(RegSet::of(Rt, Rt2).bits(), adr, cond);
  } else {
    if (adr.get_mode() != Address::reg) {
      // offset-based addressing
      if (adr.get_wb_mode() == Address::pre) {
        (this->*sgl)(Rt, adr, cond);
        (this->*sgl)(Rt2, Address(adr.base(), wordSize), cond);
      } else if (adr.get_wb_mode() == Address::post) {
        (this->*sgl)(Rt, adr, cond);
        (this->*sgl)(Rt2, Address(adr.base(), wordSize - adr.offset()), cond);
      } else if (adr.get_wb_mode() == Address::off) {
        (this->*sgl)(Rt, adr, cond);
        (this->*sgl)(Rt2, Address(adr.base(), adr.offset() + wordSize), cond);
      } else {
        ShouldNotReachHere();
      }
    } else {
      // index-based addressing
      if (adr.get_wb_mode() == Address::pre) {
        // current implementation does not use Address::pre for indexed access
        ShouldNotReachHere();
      } else if (adr.get_wb_mode() == Address::post) {
        // current implementation does not use Address:post for indexed access
        // enable the code below and implement proper post() method if it is required
        ShouldNotReachHere();
      } else if (adr.get_wb_mode() == Address::off) {
        (this->*sgl)(Rt, Address(pre(adr.base(), adr.index(), adr.shift(), adr.op())), cond);
        (this->*sgl)(Rt2, Address(adr.base(), wordSize), cond);
        compensate_addr_offset(adr, cond);
      } else {
        ShouldNotReachHere();
      }
    }
  }
}

#ifdef ASSERT
void MacroAssembler::verify_stack_alignment() {
  if (StackAlignmentInBytes > 4) {
    Label x;
    tst(sp, StackAlignmentInBytes-1);
    b(x, EQ);
    stop("stack unaligned");
    bind(x);
  }
}
#endif

/**
 * Emits code to update CRC-32 with a byte value according to constants in table
 *
 * @param [in,out]crc   Register containing the crc.
 * @param [in]val       Register containing the byte to fold into the CRC.
 * @param [in]table     Register containing the table of crc constants.
 *
 * uint32_t crc;
 * val = crc_table[(val ^ crc) & 0xFF];
 * crc = val ^ (crc >> 8);
 *
 */
void MacroAssembler::update_byte_crc32(Register crc, Register val, Register table) {
  eor(val, val, crc);
  andr(val, val, 0xff);
  ldr(val, Address(table, val, lsl(2)));
  eor(crc, val, crc, Assembler::lsr(8));
}

/**
 * Emits code to update CRC-32 with a 32-bit value according to tables 0 to 3
 *
 * @param [in,out]crc   Register containing the crc.
 * @param [in]v         Register containing the 32-bit to fold into the CRC.
 * @param [in]table0    Register containing table 0 of crc constants.
 * @param [in]table1    Register containing table 1 of crc constants.
 * @param [in]table2    Register containing table 2 of crc constants.
 * @param [in]table3    Register containing table 3 of crc constants.
 *
 * uint32_t crc;
 *   v = crc ^ v
 *   crc = table3[v&0xff]^table2[(v>>8)&0xff]^table1[(v>>16)&0xff]^table0[v>>24]
 *
 */
void MacroAssembler::update_word_crc32(Register crc, Register v, Register tmp,
        Register tmp2, Register table0, Register table1, Register table2, Register table3) {
  eor(v, crc, v);
  uxtb(tmp, v);
  uxtb(tmp2, v, ror(8));
  ldr(crc, Address(table3, tmp, lsl(2)));
  ldr(tmp2, Address(table2, tmp2, lsl(2)));
  uxtb(tmp, v, ror(16));
  eor(crc, crc, tmp2);
  uxtb(tmp2, v, ror(24));
  ldr(tmp, Address(table1, tmp, lsl(2)));
  ldr(tmp2, Address(table0, tmp2, lsl(2)));
  eor(crc, crc, tmp);
  eor(crc, crc, tmp2);
}

/**
 * @param crc   register containing existing CRC (32-bit)
 * @param buf   register pointing to input byte buffer (byte*)
 * @param len   register containing number of bytes
 * @param table register that will contain address of CRC table
 * @param tmp   scratch register
 */
void MacroAssembler::kernel_crc32(Register crc, Register buf, Register len,
        Register table0, Register table1, Register table2, Register table3,
        Register tmp, Register tmp2, Register tmp3) {
  Label L_cpu, L_by8_loop, L_by1, L_by1_loop, L_align_by1_loop, L_align_exit, L_exit;

  inv(crc, crc);
  if (UseCRC32) {
    Label CRC_by4_loop, CRC_by1_loop;

      subs(len, len, 4);
      b(CRC_by4_loop, Assembler::GE);
      adds(len, len, 4);
      b(CRC_by1_loop, Assembler::GT);
      b(L_exit);

    BIND(CRC_by4_loop);
      ldr(tmp, Address(post(buf, 4)));
      subs(len, len, 4);
      crc32w(crc, crc, tmp);
      b(CRC_by4_loop, Assembler::GE);
      adds(len, len, 4);
      b(L_exit, Assembler::LE);
    BIND(CRC_by1_loop);
      ldrb(tmp, Address(post(buf, 1)));
      subs(len, len, 1);
      crc32b(crc, crc, tmp);
      b(CRC_by1_loop, Assembler::GT);
    BIND(L_exit);
      inv(crc, crc);
      return;
  }
    lea(table0, ExternalAddress(StubRoutines::crc_table_addr()));
    add(table1, table0, 1*256*sizeof(juint));
    add(table2, table0, 2*256*sizeof(juint));
    add(table3, table0, 3*256*sizeof(juint));

  BIND(L_align_by1_loop);
    tst(buf, 3);
    b(L_align_exit, Assembler::EQ);
    cmp(len, 0);
    b(L_exit, Assembler::EQ);
    sub(len, len, 1);
    ldrb(tmp, Address(post(buf, 1)));
    update_byte_crc32(crc, tmp, table0);
    b(L_align_by1_loop);

  BIND(L_align_exit);

  if(VM_Version::features() & FT_AdvSIMD) {
  if (UseNeon) {
      cmp(len, 32+12); // account for possible need for alignment
      b(L_cpu, Assembler::LT);

    Label L_fold, L_align_by4_loop, L_align_by4_exit;

    BIND(L_align_by4_loop);
      tst(buf, 0xf);
      b(L_align_by4_exit, Assembler::EQ);
      ldr(tmp, Address(post(buf, 4)));
      update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
      sub(len, len, 4);
      b(L_align_by4_loop);

    BIND(L_align_by4_exit);

      add(tmp, table0, 4*256*sizeof(juint)); // Point at the Neon constants

      vld1_64(d0, d1, post(buf, 16), Assembler::ALIGN_128);
      vld1_64(d4, post(tmp, 8), Assembler::ALIGN_64);
      vld1_64(d5, post(tmp, 8), Assembler::ALIGN_64);
      vld1_64(d6, post(tmp, 8), Assembler::ALIGN_64);
      vld1_64(d7, post(tmp, 8), Assembler::ALIGN_64);
      veor_64(d16, d16, d16);
      vmov_32(d16, 0, crc);

      veor_64(d0, d0, d16);
      sub(len, len, 32);

    BIND(L_fold);
      vmullp_8(q8, d0, d5);
      vmullp_8(q9, d0, d7);
      vmullp_8(q10, d0, d4);
      vmullp_8(q11, d0, d6);

      vmullp_8(q12, d1, d5);
      vmullp_8(q13, d1, d7);
      vmullp_8(q14, d1, d4);
      vmullp_8(q15, d1, d6);

      vuzp_128_16(q9, q8);
      veor_128(q8, q8, q9);

      vuzp_128_16(q13, q12);
      veor_128(q12, q12, q13);

      vshll_16u(q9, d16, 8);
      vshll_16u(q8, d17, 8);

      vshll_16u(q13, d24, 8);
      vshll_16u(q12, d25, 8);

      veor_128(q8, q8, q10);
      veor_128(q12, q12, q14);
      veor_128(q9, q9, q11);
      veor_128(q13, q13, q15);

      veor_64(d19, d19, d18);
      veor_64(d18, d27, d26);

      vshll_32u(q13, d18, 16);
      vshll_32u(q9, d19, 16);

      veor_128(q9, q8, q9);
      veor_128(q13, q12, q13);

      veor_64(d31, d26, d27);
      veor_64(d30, d18, d19);

      vshl_128_64(q15, q15, 1);
      vld1_64(d0, d1, post(buf, 16), Assembler::ALIGN_128);
      veor_128(q0, q0, q15);

      subs(len, len, 16);
      b(L_fold, Assembler::GE);

      vmov_32(tmp, d0, 0);
      mov(crc, 0);
      update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
      vmov_32(tmp, d0, 1);
      update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
      vmov_32(tmp, d1, 0);
      update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
      vmov_32(tmp, d1, 1);
      update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);

      add(len, len, 16);
  }
  } // if FT_AdvSIMD

  BIND(L_cpu);
    subs(len, len, 8);
    b(L_by8_loop, Assembler::GE);
    adds(len, len, 8);
    b(L_by1_loop, Assembler::GT);
    b(L_exit);

  BIND(L_by8_loop);
    ldr(tmp, Address(post(buf, 4)));
    update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
    ldr(tmp, Address(post(buf, 4)));
    update_word_crc32(crc, tmp, tmp2, tmp3, table0, table1, table2, table3);
    subs(len, len, 8);
    b(L_by8_loop, Assembler::GE);
    adds(len, len, 8);
    b(L_exit, Assembler::LE);
  BIND(L_by1_loop);
    subs(len, len, 1);
    ldrb(tmp, Address(post(buf, 1)));
    update_byte_crc32(crc, tmp, table0);
    b(L_by1_loop, Assembler::GT);

  BIND(L_exit);
    inv(crc, crc);
}

void MacroAssembler::bfc_impl(Register Rd, int lsb, int width, Condition cond) {
  if (width > 15 && lsb == 0) {
    lsr(Rd, Rd, width);
    lsl(Rd, Rd, width);
  } else if (width > 15 && lsb + width == 32) {
    lsl(Rd, Rd, 32 - lsb);
    lsr(Rd, Rd, 32 - lsb);
  } else {
    const int lsb1 = (lsb & 1);
    int w1 = width <= 8 - lsb1 ? width : 8 - lsb1;
    while (width) {
      bic(Rd, Rd, ((1 << w1) - 1) << lsb);
      width -= w1;
      lsb += w1;
      w1 = width > 8 ? 8 : width;
    }
  }
}
