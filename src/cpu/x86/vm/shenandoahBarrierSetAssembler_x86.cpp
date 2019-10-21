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
#include "macroAssembler_x86.hpp"
#include "shenandoahBarrierSetAssembler_x86.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSetC1.hpp"
#include "gc_implementation/shenandoah/shenandoahBrooksPointer.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.hpp"
#include "gc_implementation/shenandoah/shenandoahHeapRegion.hpp"
#include "gc_implementation/shenandoah/shenandoahRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"

ShenandoahBarrierSetAssembler* ShenandoahBarrierSetAssembler::bsasm() {
  return ShenandoahBarrierSet::barrier_set()->bsasm();
}

#define __ masm->

void ShenandoahBarrierSetAssembler::resolve_forward_pointer(MacroAssembler* masm, Register dst) {
  assert(ShenandoahCASBarrier, "should be enabled");
  Label is_null;
  __ testptr(dst, dst);
  __ jcc(Assembler::zero, is_null);
  resolve_forward_pointer_not_null(masm, dst);
  __ bind(is_null);
}

void ShenandoahBarrierSetAssembler::resolve_forward_pointer_not_null(MacroAssembler* masm, Register dst) {
  assert(ShenandoahCASBarrier || ShenandoahLoadRefBarrier, "should be enabled");
  __ movptr(dst, Address(dst, ShenandoahBrooksPointer::byte_offset()));
}

void ShenandoahBarrierSetAssembler::load_reference_barrier_not_null(MacroAssembler* masm, Register dst) {
  assert(ShenandoahLoadRefBarrier, "Should be enabled");
#ifdef _LP64
  Label done;

  Address gc_state(r15_thread, in_bytes(JavaThread::gc_state_offset()));
  __ testb(gc_state, ShenandoahHeap::HAS_FORWARDED | ShenandoahHeap::EVACUATION);
  __ jcc(Assembler::zero, done);

  {
    //__ movq(Address(rsp, -5 * wordSize), rsp);

    __ subq(rsp, 16 * wordSize);

    __ movq(Address(rsp, 15 * wordSize), rax);
    __ movq(Address(rsp, 14 * wordSize), rcx);
    __ movq(Address(rsp, 13 * wordSize), rdx);
    __ movq(Address(rsp, 12 * wordSize), rbx);
    // skip rsp
    __ movq(Address(rsp, 10 * wordSize), rbp);
    __ movq(Address(rsp, 9 * wordSize), rsi);
    __ movq(Address(rsp, 8 * wordSize), rdi);
    __ movq(Address(rsp, 7 * wordSize), r8);
    __ movq(Address(rsp, 6 * wordSize), r9);
    __ movq(Address(rsp, 5 * wordSize), r10);
    __ movq(Address(rsp, 4 * wordSize), r11);
    __ movq(Address(rsp, 3 * wordSize), r12);
    __ movq(Address(rsp, 2 * wordSize), r13);
    __ movq(Address(rsp, wordSize), r14);
    __ movq(Address(rsp, 0), r15);
  }
  __ super_call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::load_reference_barrier_interpreter), dst);
  {
    __ movq(r15, Address(rsp, 0));
    __ movq(r14, Address(rsp, wordSize));
    __ movq(r13, Address(rsp, 2 * wordSize));
    __ movq(r12, Address(rsp, 3 * wordSize));
    __ movq(r11, Address(rsp, 4 * wordSize));
    __ movq(r10, Address(rsp, 5 * wordSize));
    __ movq(r9,  Address(rsp, 6 * wordSize));
    __ movq(r8,  Address(rsp, 7 * wordSize));
    __ movq(rdi, Address(rsp, 8 * wordSize));
    __ movq(rsi, Address(rsp, 9 * wordSize));
    __ movq(rbp, Address(rsp, 10 * wordSize));
    // skip rsp
    __ movq(rbx, Address(rsp, 12 * wordSize));
    __ movq(rdx, Address(rsp, 13 * wordSize));
    __ movq(rcx, Address(rsp, 14 * wordSize));
    if (dst != rax) {
      __ movq(dst, rax);
      __ movq(rax, Address(rsp, 15 * wordSize));
    }
    __ addq(rsp, 16 * wordSize);
  }
  __ bind(done);
#else
  Unimplemented();
#endif
}

void ShenandoahBarrierSetAssembler::load_reference_barrier(MacroAssembler* masm, Register dst) {
  if (ShenandoahLoadRefBarrier) {
    Label done;
    __ testptr(dst, dst);
    __ jcc(Assembler::zero, done);
    load_reference_barrier_not_null(masm, dst);
    __ bind(done);
  }
}

// Special Shenandoah CAS implementation that handles false negatives
// due to concurrent evacuation.
#ifndef _LP64
void ShenandoahBarrierSetAssembler::cmpxchg_oop(MacroAssembler* masm,
                                                Register res, Address addr, Register oldval, Register newval,
                                                bool exchange, Register tmp1, Register tmp2) {
  // Shenandoah has no 32-bit version for this.
  Unimplemented();
}
#else
void ShenandoahBarrierSetAssembler::cmpxchg_oop(MacroAssembler* masm,
                                                Register res, Address addr, Register oldval, Register newval,
                                                bool exchange, Register tmp1, Register tmp2) {
  assert(ShenandoahCASBarrier, "Should only be used when CAS barrier is enabled");
  assert(oldval == rax, "must be in rax for implicit use in cmpxchg");

  Label retry, done;

  // Remember oldval for retry logic below
  if (UseCompressedOops) {
    __ movl(tmp1, oldval);
  } else {
    __ movptr(tmp1, oldval);
  }

  // Step 1. Try to CAS with given arguments. If successful, then we are done,
  // and can safely return.
  if (os::is_MP()) __ lock();
  if (UseCompressedOops) {
    __ cmpxchgl(newval, addr);
  } else {
    __ cmpxchgptr(newval, addr);
  }
  __ jcc(Assembler::equal, done, true);

  // Step 2. CAS had failed. This may be a false negative.
  //
  // The trouble comes when we compare the to-space pointer with the from-space
  // pointer to the same object. To resolve this, it will suffice to resolve both
  // oldval and the value from memory -- this will give both to-space pointers.
  // If they mismatch, then it was a legitimate failure.
  //
  if (UseCompressedOops) {
    __ decode_heap_oop(tmp1);
  }
  resolve_forward_pointer(masm, tmp1);

  if (UseCompressedOops) {
    __ movl(tmp2, oldval);
    __ decode_heap_oop(tmp2);
  } else {
    __ movptr(tmp2, oldval);
  }
  resolve_forward_pointer(masm, tmp2);

  __ cmpptr(tmp1, tmp2);
  __ jcc(Assembler::notEqual, done, true);

  // Step 3. Try to CAS again with resolved to-space pointers.
  //
  // Corner case: it may happen that somebody stored the from-space pointer
  // to memory while we were preparing for retry. Therefore, we can fail again
  // on retry, and so need to do this in loop, always resolving the failure
  // witness.
  __ bind(retry);
  if (os::is_MP()) __ lock();
  if (UseCompressedOops) {
    __ cmpxchgl(newval, addr);
  } else {
    __ cmpxchgptr(newval, addr);
  }
  __ jcc(Assembler::equal, done, true);

  if (UseCompressedOops) {
    __ movl(tmp2, oldval);
    __ decode_heap_oop(tmp2);
  } else {
    __ movptr(tmp2, oldval);
  }
  resolve_forward_pointer(masm, tmp2);

  __ cmpptr(tmp1, tmp2);
  __ jcc(Assembler::equal, retry, true);

  // Step 4. If we need a boolean result out of CAS, check the flag again,
  // and promote the result. Note that we handle the flag from both the CAS
  // itself and from the retry loop.
  __ bind(done);
  if (!exchange) {
    assert(res != NULL, "need result register");
    __ setb(Assembler::equal, res);
    __ movzbl(res, res);
  }
}
#endif // LP64

void ShenandoahBarrierSetAssembler::save_vector_registers(MacroAssembler* masm) {
  //__ push_FPU_state();
}

void ShenandoahBarrierSetAssembler::restore_vector_registers(MacroAssembler* masm) {
  //__ pop_FPU_state();
}

#undef __

#ifdef COMPILER1

#define __ ce->masm()->

void ShenandoahBarrierSetAssembler::gen_load_reference_barrier_stub(LIR_Assembler* ce, ShenandoahLoadReferenceBarrierStub* stub) {
  __ bind(*stub->entry());

  Label done;
  Register obj = stub->obj()->as_register();
  Register res = stub->result()->as_register();

  if (res != obj) {
    __ mov(res, obj);
  }

  // Check for null.
  if (stub->needs_null_check()) {
    __ testptr(res, res);
    __ jcc(Assembler::zero, done);
  }

  load_reference_barrier_not_null(ce->masm(), res);

  __ bind(done);
  __ jmp(*stub->continuation());
}

#undef __

#endif // COMPILER1

#define __ masm->

void ShenandoahHeap::compile_prepare_oop(MacroAssembler* masm, Register obj) {
#ifdef _LP64
  __ incrementq(obj, ShenandoahBrooksPointer::byte_size());
#else
  __ incrementl(obj, ShenandoahBrooksPointer::byte_size());
#endif
  __ movptr(Address(obj, ShenandoahBrooksPointer::byte_offset()), obj);
}

#undef __
