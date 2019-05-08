/*
 * Copyright (c) 2018, Red Hat, Inc. All rights reserved.
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
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "macroAssembler_aarch64.hpp"
#include "shenandoahBarrierSetAssembler_aarch64.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSetC1.hpp"
#include "gc_implementation/shenandoah/shenandoahForwarding.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.hpp"
#include "gc_implementation/shenandoah/shenandoahRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/thread.hpp"

ShenandoahBarrierSetAssembler* ShenandoahBarrierSetAssembler::bsasm() {
  return ShenandoahBarrierSet::barrier_set()->bsasm();
}

#define __ masm->

void ShenandoahBarrierSetAssembler::resolve_forward_pointer(MacroAssembler* masm, Register dst) {
  assert(ShenandoahCASBarrier, "should be enabled");
  Label is_null;
  __ cbz(dst, is_null);
  resolve_forward_pointer_not_null(masm, dst);
  __ bind(is_null);
}

void ShenandoahBarrierSetAssembler::resolve_forward_pointer_not_null(MacroAssembler* masm, Register dst) {
  assert(ShenandoahCASBarrier || ShenandoahLoadRefBarrier, "should be enabled");
  __ ldr(dst, Address(dst, ShenandoahForwarding::byte_offset()));
}

void ShenandoahBarrierSetAssembler::load_reference_barrier_not_null(MacroAssembler* masm, Register dst) {
  assert(ShenandoahLoadRefBarrier, "Should be enabled");
  assert(dst != rscratch2, "need rscratch2");

  Label done;
  __ enter();
  Address gc_state(rthread, in_bytes(JavaThread::gc_state_offset()));
  __ ldrb(rscratch2, gc_state);

  // Check for heap stability
  __ tbz(rscratch2, ShenandoahHeap::HAS_FORWARDED_BITPOS, done);

  RegSet to_save = RegSet::of(r0);
  if (dst != r0) {
    __ push(to_save, sp);
    __ mov(r0, dst);
  }

  __ push_call_clobbered_registers();
  __ super_call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::load_reference_barrier_interpreter), r0);
  __ mov(rscratch1, r0);
  __ pop_call_clobbered_registers();
  __ mov(r0, rscratch1);

  if (dst != r0) {
    __ mov(dst, r0);
    __ pop(to_save, sp);
  }

  __ bind(done);
  __ leave();
}

void ShenandoahBarrierSetAssembler::load_reference_barrier(MacroAssembler* masm, Register dst) {
  if (ShenandoahLoadRefBarrier) {
    Label is_null;
    __ cbz(dst, is_null);
    load_reference_barrier_not_null(masm, dst);
    __ bind(is_null);
  }
}

void ShenandoahBarrierSetAssembler::cmpxchg_oop(MacroAssembler* masm, Register addr, Register expected, Register new_val,
                                                bool acquire, bool release, bool weak, bool is_cae,
                                                Register result) {

  Register tmp1 = rscratch1;
  Register tmp2 = rscratch2;
  bool is_narrow = UseCompressedOops;
  Assembler::operand_size size = is_narrow ? Assembler::word : Assembler::xword;

  assert_different_registers(addr, expected, new_val, tmp1, tmp2);

  Label retry, done, fail;

  // CAS, using LL/SC pair.
  __ bind(retry);
  __ load_exclusive(tmp1, addr, size, acquire);
  if (is_narrow) {
    __ cmpw(tmp1, expected);
  } else {
    __ cmp(tmp1, expected);
  }
  __ br(Assembler::NE, fail);
  __ store_exclusive(tmp2, new_val, addr, size, release);
  if (weak) {
    __ cmpw(tmp2, 0u); // If the store fails, return NE to our caller
  } else {
    __ cbnzw(tmp2, retry);
  }
  __ b(done);

  __ bind(fail);
  // Check if rb(expected)==rb(tmp1)
  // Shuffle registers so that we have memory value ready for next expected.
  __ mov(tmp2, expected);
  __ mov(expected, tmp1);
  if (is_narrow) {
    __ decode_heap_oop(tmp1, tmp1);
    __ decode_heap_oop(tmp2, tmp2);
  }
  resolve_forward_pointer(masm, tmp1);
  resolve_forward_pointer(masm, tmp2);
  __ cmp(tmp1, tmp2);
  // Retry with expected now being the value we just loaded from addr.
  __ br(Assembler::EQ, retry);
  if (is_cae && is_narrow) {
    // For cmp-and-exchange and narrow oops, we need to restore
    // the compressed old-value. We moved it to 'expected' a few lines up.
    __ mov(result, expected);
  }
  __ bind(done);

  if (is_cae) {
    __ mov(result, tmp1);
  } else {
    __ cset(result, Assembler::EQ);
  }
}

#undef __

#ifdef COMPILER1

#define __ ce->masm()->

void ShenandoahBarrierSetAssembler::gen_load_reference_barrier_stub(LIR_Assembler* ce, ShenandoahLoadReferenceBarrierStub* stub) {

  Register obj = stub->obj()->as_register();
  Register res = stub->result()->as_register();

  Label done;

  __ bind(*stub->entry());

  if (res != obj) {
    __ mov(res, obj);
  }
  // Check for null.
  if (stub->needs_null_check()) {
    __ cbz(res, done);
  }

  load_reference_barrier_not_null(ce->masm(), res);

  __ bind(done);
  __ b(*stub->continuation());
}

#undef __

#endif // COMPILER1

#define __ masm->

void ShenandoahHeap::compile_prepare_oop(MacroAssembler* masm, Register obj) {
  __ add(obj, obj, ShenandoahForwarding::byte_size());
  __ str(obj, Address(obj, -1 * HeapWordSize));
}

#undef __
