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
#include "gc_implementation/shenandoah/shenandoahForwarding.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.hpp"
#include "gc_implementation/shenandoah/shenandoahHeapRegion.hpp"
#include "gc_implementation/shenandoah/shenandoahRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"

ShenandoahBarrierSetAssembler* ShenandoahBarrierSetAssembler::bsasm() {
  return ShenandoahBarrierSet::barrier_set()->bsasm();
}

#define __ masm->

void ShenandoahBarrierSetAssembler::resolve_forward_pointer(MacroAssembler* masm, Register dst, Register tmp) {
  assert(ShenandoahCASBarrier, "should be enabled");
  Label is_null;
  __ testptr(dst, dst);
  __ jcc(Assembler::zero, is_null);
  resolve_forward_pointer_not_null(masm, dst, tmp);
  __ bind(is_null);
}

void ShenandoahBarrierSetAssembler::resolve_forward_pointer_not_null(MacroAssembler* masm, Register dst, Register tmp) {
  assert(ShenandoahCASBarrier || ShenandoahLoadRefBarrier, "should be enabled");
  // The below loads the mark word, checks if the lowest two bits are
  // set, and if so, clear the lowest two bits and copy the result
  // to dst. Otherwise it leaves dst alone.
  // Implementing this is surprisingly awkward. I do it here by:
  // - Inverting the mark word
  // - Test lowest two bits == 0
  // - If so, set the lowest two bits
  // - Invert the result back, and copy to dst

  bool borrow_reg = (tmp == noreg);
  if (borrow_reg) {
    // No free registers available. Make one useful.
    tmp = LP64_ONLY(rscratch1) NOT_LP64(rdx);
    if (tmp == dst) {
      tmp = LP64_ONLY(rscratch2) NOT_LP64(rcx);
    }
    __ push(tmp);
  }

  assert_different_registers(dst, tmp);

  Label done;
  __ movptr(tmp, Address(dst, oopDesc::mark_offset_in_bytes()));
  __ notptr(tmp);
  __ testb(tmp, markOopDesc::marked_value);
  __ jccb(Assembler::notZero, done);
  __ orptr(tmp, markOopDesc::marked_value);
  __ notptr(tmp);
  __ mov(dst, tmp);
  __ bind(done);

  if (borrow_reg) {
    __ pop(tmp);
  }
}

void ShenandoahBarrierSetAssembler::load_reference_barrier_not_null(MacroAssembler* masm, Register dst) {
  assert(ShenandoahLoadRefBarrier, "Should be enabled");

  Label done;

#ifdef _LP64
  Register thread = r15_thread;
#else
  Register thread = rcx;
  if (thread == dst) {
    thread = rbx;
  }
  __ push(thread);
  __ get_thread(thread);
#endif
  assert_different_registers(dst, thread);

  Address gc_state(thread, in_bytes(JavaThread::gc_state_offset()));
  __ testb(gc_state, ShenandoahHeap::HAS_FORWARDED);
  __ jcc(Assembler::zero, done);

  {
    __ save_vector_registers();

    __ subptr(rsp, LP64_ONLY(16) NOT_LP64(8) * wordSize);

    __ movptr(Address(rsp,  0 * wordSize), rax);
    __ movptr(Address(rsp,  1 * wordSize), rcx);
    __ movptr(Address(rsp,  2 * wordSize), rdx);
    __ movptr(Address(rsp,  3 * wordSize), rbx);
    // skip rsp
    __ movptr(Address(rsp,  5 * wordSize), rbp);
    __ movptr(Address(rsp,  6 * wordSize), rsi);
    __ movptr(Address(rsp,  7 * wordSize), rdi);
#ifdef _LP64
    __ movptr(Address(rsp,  8 * wordSize),  r8);
    __ movptr(Address(rsp,  9 * wordSize),  r9);
    __ movptr(Address(rsp, 10 * wordSize), r10);
    __ movptr(Address(rsp, 11 * wordSize), r11);
    __ movptr(Address(rsp, 12 * wordSize), r12);
    __ movptr(Address(rsp, 13 * wordSize), r13);
    __ movptr(Address(rsp, 14 * wordSize), r14);
    __ movptr(Address(rsp, 15 * wordSize), r15);
#endif
  }
  __ super_call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::load_reference_barrier_interpreter), dst);
  {
#ifdef _LP64
    __ movptr(r15, Address(rsp, 15 * wordSize));
    __ movptr(r14, Address(rsp, 14 * wordSize));
    __ movptr(r13, Address(rsp, 13 * wordSize));
    __ movptr(r12, Address(rsp, 12 * wordSize));
    __ movptr(r11, Address(rsp, 11 * wordSize));
    __ movptr(r10, Address(rsp, 10 * wordSize));
    __ movptr(r9,  Address(rsp,  9 * wordSize));
    __ movptr(r8,  Address(rsp,  8 * wordSize));
#endif
    __ movptr(rdi, Address(rsp,  7 * wordSize));
    __ movptr(rsi, Address(rsp,  6 * wordSize));
    __ movptr(rbp, Address(rsp,  5 * wordSize));
    // skip rsp
    __ movptr(rbx, Address(rsp,  3 * wordSize));
    __ movptr(rdx, Address(rsp,  2 * wordSize));
    __ movptr(rcx, Address(rsp,  1 * wordSize));
    if (dst != rax) {
      __ movptr(dst, rax);
      __ movptr(rax, Address(rsp, 0 * wordSize));
    }
    __ addptr(rsp, LP64_ONLY(16) NOT_LP64(8) * wordSize);

    __ restore_vector_registers();
  }
  __ bind(done);

#ifndef _LP64
  __ pop(thread);
#endif
}

void ShenandoahBarrierSetAssembler::storeval_barrier(MacroAssembler* masm, Register dst, Register tmp) {
  if (ShenandoahStoreValEnqueueBarrier) {
    storeval_barrier_impl(masm, dst, tmp);
  }
}

void ShenandoahBarrierSetAssembler::storeval_barrier_impl(MacroAssembler* masm, Register dst, Register tmp) {
  assert(ShenandoahStoreValEnqueueBarrier, "should be enabled");

  if (dst == noreg) return;

  if (ShenandoahStoreValEnqueueBarrier) {
    // The set of registers to be saved+restored is the same as in the write-barrier above.
    // Those are the commonly used registers in the interpreter.
    __ pusha();
    // __ push_callee_saved_registers();
    __ subptr(rsp, 2 * Interpreter::stackElementSize);
    __ movdbl(Address(rsp, 0), xmm0);

#ifdef _LP64
    Register thread = r15_thread;
#else
    Register thread = rcx;
    if (thread == dst || thread == tmp) {
      thread = rdi;
    }
    if (thread == dst || thread == tmp) {
      thread = rbx;
    }
    __ get_thread(thread);
#endif
    assert_different_registers(dst, tmp, thread);

    __ g1_write_barrier_pre(noreg, dst, thread, tmp, true, false);
    __ movdbl(xmm0, Address(rsp, 0));
    __ addptr(rsp, 2 * Interpreter::stackElementSize);
    //__ pop_callee_saved_registers();
    __ popa();
  }
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
void ShenandoahBarrierSetAssembler::cmpxchg_oop(MacroAssembler* masm,
                                                Register res, Address addr, Register oldval, Register newval,
                                                bool exchange, Register tmp1, Register tmp2) {
  assert(ShenandoahCASBarrier, "Should only be used when CAS barrier is enabled");
  assert(oldval == rax, "must be in rax for implicit use in cmpxchg");

  Label retry, done;

  // Remember oldval for retry logic below
#ifdef _LP64
  if (UseCompressedOops) {
    __ movl(tmp1, oldval);
  } else
#endif
  {
    __ movptr(tmp1, oldval);
  }

  // Step 1. Try to CAS with given arguments. If successful, then we are done,
  // and can safely return.
  if (os::is_MP()) __ lock();
#ifdef _LP64
  if (UseCompressedOops) {
    __ cmpxchgl(newval, addr);
  } else
#endif
  {
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
#ifdef _LP64
  if (UseCompressedOops) {
    __ decode_heap_oop(tmp1);
  }
#endif
  resolve_forward_pointer(masm, tmp1);

#ifdef _LP64
  if (UseCompressedOops) {
    __ movl(tmp2, oldval);
    __ decode_heap_oop(tmp2);
  } else
#endif
  {
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
#ifdef _LP64
  if (UseCompressedOops) {
    __ cmpxchgl(newval, addr);
  } else
#endif
  {
    __ cmpxchgptr(newval, addr);
  }
  __ jcc(Assembler::equal, done, true);

#ifdef _LP64
  if (UseCompressedOops) {
    __ movl(tmp2, oldval);
    __ decode_heap_oop(tmp2);
  } else
#endif
  {
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
#ifdef _LP64
    __ setb(Assembler::equal, res);
    __ movzbl(res, res);
#else
    // Need something else to clean the result, because some registers
    // do not have byte encoding that movzbl wants. Cannot do the xor first,
    // because it modifies the flags.
    Label res_non_zero;
    __ movptr(res, 1);
    __ jcc(Assembler::equal, res_non_zero, true);
    __ xorptr(res, res);
    __ bind(res_non_zero);
#endif
  }
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
