/*
 * Copyright (c) 2003, 2011, Oracle and/or its affiliates. All rights reserved.
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
#include "interp_masm_aarch32.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/bytecodeTracer.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/method.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"

#include <sys/types.h>

#ifndef PRODUCT
#include "oops/method.hpp"
#include "vm_version_aarch32.hpp"
#endif // !PRODUCT

#define __ _masm->

#ifndef CC_INTERP

//-----------------------------------------------------------------------------

extern "C" void entry(CodeBuffer*);

//-----------------------------------------------------------------------------

address TemplateInterpreterGenerator::generate_StackOverflowError_handler() {
  address entry = __ pc();

#ifdef ASSERT
  {
    Label L;
    __ ldr(rscratch1, Address(rfp,
                       frame::interpreter_frame_monitor_block_top_offset *
                       wordSize));
    __ mov(rscratch2, sp);
    __ cmp(rscratch1, rscratch2); // maximal rsp for current rfp (stack
                           // grows negative)
    __ b(L, Assembler::HS); // check if frame is complete
    __ stop ("interpreter frame not set up");
    __ bind(L);
  }
#endif // ASSERT
  // Restore bcp under the assumption that the current frame is still
  // interpreted
  __ restore_bcp();

  // expression stack must be empty before entering the VM if an
  // exception happened
  __ empty_expression_stack();
  // throw exception
  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address,
                              InterpreterRuntime::throw_StackOverflowError));
  return entry;
}

address TemplateInterpreterGenerator::generate_ArrayIndexOutOfBounds_handler(
        const char* name) {
  address entry = __ pc();
  // expression stack must be empty before entering the VM if an
  // exception happened
  __ empty_expression_stack();
  // setup parameters
  // ??? convention: expect aberrant index in register r2
  __ mov(c_rarg1, (address)name);
  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address,
                              InterpreterRuntime::
                              throw_ArrayIndexOutOfBoundsException),
             c_rarg1, c_rarg2);
  return entry;
}

address TemplateInterpreterGenerator::generate_ClassCastException_handler() {
  address entry = __ pc();

  // object is at TOS
  __ pop(c_rarg1);

  // expression stack must be empty before entering the VM if an
  // exception happened
  __ empty_expression_stack();

  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address,
                              InterpreterRuntime::
                              throw_ClassCastException),
             c_rarg1);
  return entry;
}

address TemplateInterpreterGenerator::generate_exception_handler_common(
        const char* name, const char* message, bool pass_oop) {
  assert(!pass_oop || message == NULL, "either oop or message but not both");
  address entry = __ pc();
  if (pass_oop) {
    // object is at TOS
    __ pop(c_rarg2);
  }
  // expression stack must be empty before entering the VM if an
  // exception happened
  __ empty_expression_stack();
  // FIXME shouldn't it be in rest of generate_* ?
  // rdispatch assumed to cache dispatch table. This code can be called from
  // signal handler, so it can't assume execption caller preserved the register,
  // so restore it here
  __ get_dispatch();
  // FIXME shouldn't get_method be here ?
  // setup parameters
  __ lea(c_rarg1, Address((address)name));
  if (pass_oop) {
    __ call_VM(r0, CAST_FROM_FN_PTR(address,
                                    InterpreterRuntime::
                                    create_klass_exception),
               c_rarg1, c_rarg2);
  } else {
    // kind of lame ExternalAddress can't take NULL because
    // external_word_Relocation will assert.
    if (message != NULL) {
      __ lea(c_rarg2, Address((address)message));
    } else {
      __ mov(c_rarg2, NULL_WORD);
    }
    __ call_VM(r0,
               CAST_FROM_FN_PTR(address, InterpreterRuntime::create_exception),
               c_rarg1, c_rarg2);
  }
  // throw exception
  __ b(address(Interpreter::throw_exception_entry()));
  return entry;
}

address TemplateInterpreterGenerator::generate_continuation_for(TosState state) {
  address entry = __ pc();
  // NULL last_sp until next java call
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  __ dispatch_next(state);
  return entry;
}

address TemplateInterpreterGenerator::generate_return_entry_for(TosState state, int step, size_t index_size) {
  address entry = __ pc();

  __ print_method_exit();
  __ reg_printf("A. return_entry <r1:r0> : 0x%08x%08x\n", r1, r0);

  // Restore stack bottom in case i2c adjusted stack
  __ ldr(sp, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  // and NULL it as marker that sp is now tos until next java call
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  __ reg_printf("B. return_entry <r1:r0> : 0x%08x%08x\n", r1, r0);
  __ restore_bcp();
  __ restore_locals();
  __ restore_constant_pool_cache();
  __ get_method(rmethod);
  __ reg_printf("C. return_entry <r1:r0> : 0x%08x%08x\n", r1, r0);

  // Pop N words from the stack
  __ get_cache_and_index_at_bcp(r3, r2, 1, index_size);
  __ reg_printf("D. return_entry <r1:r0> : 0x%08x%08x\n", r1, r0);
  __ ldr(r3, Address(r3, ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::flags_offset()));
  __ andr(r3, r3, ConstantPoolCacheEntry::parameter_size_mask);

  __ add(sp, sp, r3, lsl(2));

  // Restore machine SP
  /*__ ldr(rscratch1, Address(rmethod, Method::const_offset()));
  __ ldrh(rscratch1, Address(rscratch1, ConstMethod::max_stack_offset()));
  __ add(rscratch1, rscratch1, frame::interpreter_frame_monitor_size() + 2);
  __ ldr(rscratch2,
         Address(rfp, frame::interpreter_frame_initial_sp_offset * wordSize));
  __ sub(rscratch1, rscratch2, rscratch1, lsl(2));
  __ bic(sp, rscratch1, 0xf);*/

  __ get_dispatch();
  __ reg_printf("E. return_entry <r1:r0> : 0x%08x%08x\n", r1, r0);
  __ dispatch_next(state, step);

  return entry;
}

address TemplateInterpreterGenerator::generate_deopt_entry_for(TosState state,
                                                               int step) {
  address entry = __ pc();
  __ restore_bcp();
  __ restore_locals();
  __ restore_constant_pool_cache();
  __ get_method(rmethod);

  __ get_dispatch();

  // Calculate stack limit
  __ ldr(rscratch1, Address(rmethod, Method::const_offset()));
  __ ldrh(rscratch1, Address(rscratch1, ConstMethod::max_stack_offset()));
  __ add(rscratch1, rscratch1, frame::interpreter_frame_monitor_size() + 2);
  __ ldr(rscratch2,
         Address(rfp, frame::interpreter_frame_initial_sp_offset * wordSize));
  __ sub(rscratch1, rscratch2, rscratch1, lsl(2));
  __ bic(sp, rscratch1, 0xf);

  // Restore expression stack pointer
  __ ldr(sp, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  // NULL last_sp until next java call
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));

  // handle exceptions
  {
    Label L;
    __ ldr(rscratch1, Address(rthread, Thread::pending_exception_offset()));
    __ cbz(rscratch1, L);
    __ call_VM(noreg,
               CAST_FROM_FN_PTR(address,
                                InterpreterRuntime::throw_pending_exception));
    __ should_not_reach_here();
    __ bind(L);
  }

  __ dispatch_next(state, step);
  return entry;
}


int AbstractInterpreter::BasicType_as_index(BasicType type) {
  int i = 0;
  switch (type) {
    case T_BOOLEAN: i = 0; break;
    case T_CHAR   : i = 1; break;
    case T_BYTE   : i = 2; break;
    case T_SHORT  : i = 3; break;
    case T_INT    : i = 4; break;
    case T_LONG   : i = 5; break;
    case T_VOID   : i = 6; break;
    case T_FLOAT  : i = 7; break;
    case T_DOUBLE : i = 8; break;
    case T_OBJECT : i = 9; break;
    case T_ARRAY  : i = 9; break;
    default       : ShouldNotReachHere();
  }
  assert(0 <= i && i < AbstractInterpreter::number_of_result_handlers,
         "index out of bounds");
  return i;
}


address TemplateInterpreterGenerator::generate_result_handler_for(
        BasicType type) {
  address entry = __ pc();
  switch (type) {
  case T_BOOLEAN: __ uxtb(r0, r0);       break;
  case T_CHAR   : __ uxth(r0, r0);       break;
  case T_BYTE   : __ sxtb(r0, r0);       break;
  case T_SHORT  : __ sxth(r0, r0);       break;
  case T_INT    : /* nothing to do */    break;
  case T_LONG   : /* nothing to do */    break;
  case T_VOID   : /* nothing to do */    break;
  case T_FLOAT  :
#ifndef HARD_FLOAT_CC
      if(hasFPU()) {
          __ vmov_f32(d0, r0);
      }
#endif
      break;
  case T_DOUBLE :
#ifndef HARD_FLOAT_CC
      if(hasFPU()) {
          __ vmov_f64(d0, r0, r1);
      }
#endif
    break;
  case T_OBJECT :
    // retrieve result from frame
    __ reg_printf("In object result handler\n");
    __ ldr(r0, Address(rfp, frame::interpreter_frame_oop_temp_offset*wordSize));
    // and verify it
    __ verify_oop(r0);
    break;
  default       : ShouldNotReachHere();
  }
  __ b(lr);                                  // return from result handler
  return entry;
}

address TemplateInterpreterGenerator::generate_safept_entry_for(
        TosState state,
        address runtime_entry) {
  address entry = __ pc();
  __ push(state);
  __ call_VM(noreg, runtime_entry);
  __ membar(Assembler::AnyAny);
  __ dispatch_via(vtos, Interpreter::_normal_table.table_for(vtos));
  return entry;
}

// Helpers for commoning out cases in the various type of method entries.
//


// increment invocation count & check for overflow
//
// Note: checking for negative value instead of overflow
//       so we have a 'sticky' overflow test
//
// rmethod: method
//
void InterpreterGenerator::generate_counter_incr(
        Label* overflow,
        Label* profile_method,
        Label* profile_method_continue) {
  Label done;
  // Note: In tiered we increment either counters in Method* or in MDO depending if we're profiling or not.
  if (TieredCompilation) {
    int increment = InvocationCounter::count_increment;
    int mask = ((1 << Tier0InvokeNotifyFreqLog) - 1) << InvocationCounter::count_shift;
    Label no_mdo;
    if (ProfileInterpreter) {
      // Are we profiling?
      __ ldr(r0, Address(rmethod, Method::method_data_offset()));
      __ cbz(r0, no_mdo);
      // Increment counter in the MDO
      const Address mdo_invocation_counter(r0, in_bytes(MethodData::invocation_counter_offset()) +
                                           in_bytes(InvocationCounter::counter_offset()));
      __ increment_mask_and_jump(mdo_invocation_counter, increment, mask, rscratch1, rscratch2, false, Assembler::EQ, overflow);
      __ b(done);
    }
    __ bind(no_mdo);
    // Increment counter in MethodCounters
    const Address invocation_counter(rscratch2,
                  MethodCounters::invocation_counter_offset() +
                  InvocationCounter::counter_offset());
    __ get_method_counters(rmethod, rscratch2, done);
    __ increment_mask_and_jump(invocation_counter, increment, mask, rscratch1, rscratch2, false, Assembler::EQ, overflow);
    __ bind(done);
  } else {
    const Address backedge_counter(rscratch2,
                  MethodCounters::backedge_counter_offset() +
                  InvocationCounter::counter_offset());
    const Address invocation_counter(rscratch2,
                  MethodCounters::invocation_counter_offset() +
                  InvocationCounter::counter_offset());

    __ get_method_counters(rmethod, rscratch2, done);

    if (ProfileInterpreter) { // %%% Merge this into MethodData*
      __ ldr(r1, Address(rscratch2, MethodCounters::interpreter_invocation_counter_offset()));
      __ add(r1, r1, 1);
      __ str(r1, Address(rscratch2, MethodCounters::interpreter_invocation_counter_offset()));
    }
    // Update standard invocation counters
    __ ldr(r1, invocation_counter);
    __ ldr(r0, backedge_counter);

    __ add(r1, r1, InvocationCounter::count_increment);
    __ mov(rscratch1, InvocationCounter::count_mask_value);
    __ andr(r0, r0, rscratch1);

    __ str(r1, invocation_counter);
    __ add(r0, r0, r1);                // add both counters

    // profile_method is non-null only for interpreted method so
    // profile_method != NULL == !native_call

    if (ProfileInterpreter && profile_method != NULL) {
      // Test to see if we should create a method data oop
      __ mov(rscratch2, ExternalAddress((address) &InvocationCounter::InterpreterProfileLimit));
      __ ldr(rscratch2, rscratch2);
      __ cmp(r0, rscratch2);
      __ b(*profile_method_continue, Assembler::LT);

      // if no method data exists, go to profile_method
      __ test_method_data_pointer(r0, *profile_method);
    }

    {
      __ mov(rscratch2, ExternalAddress((address) &InvocationCounter::InterpreterInvocationLimit));
      __ ldr(rscratch2, rscratch2);
      __ cmp(r0, rscratch2);
      __ b(*overflow, Assembler::HS);
    }
    __ bind(done);
  }
}

void InterpreterGenerator::generate_counter_overflow(Label* do_continue) {

  // Asm interpreter on entry
  // On return (i.e. jump to entry_point) [ back to invocation of interpreter ]
  // Everything as it was on entry

  // InterpreterRuntime::frequency_counter_overflow takes two
  // arguments, the first (thread) is passed by call_VM, the second
  // indicates if the counter overflow occurs at a backwards branch
  // (NULL bcp).  We pass zero for it.  The call returns the address
  // of the verified entry point for the method or NULL if the
  // compilation did not complete (either went background or bailed
  // out).
  __ mov(c_rarg1, 0);
  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address,
                              InterpreterRuntime::frequency_counter_overflow),
             c_rarg1);

  __ b(*do_continue);
}

// See if we've got enough room on the stack for locals plus overhead.
// The expression stack grows down incrementally, so the normal guard
// page mechanism will work for that.
//
// NOTE: Since the additional locals are also always pushed (wasn't
// obvious in generate_method_entry) so the guard should work for them
// too.
//
// Args:
//      r3: number of additional locals this frame needs (what we must check)
//      rmethod: Method*
//
// Kills:
//      r0
void InterpreterGenerator::generate_stack_overflow_check(void) {

  // monitor entry size: see picture of stack set
  // (generate_method_entry) and frame_amd64.hpp
  const int entry_size = frame::interpreter_frame_monitor_size() * wordSize;

  // total overhead size: entry_size + (saved rbp through expr stack
  // bottom).  be sure to change this if you add/subtract anything
  // to/from the overhead area
  const int overhead_size =
    -(frame::interpreter_frame_initial_sp_offset * wordSize) + entry_size;

  const int page_size = os::vm_page_size();

  Label after_frame_check;

  // see if the frame is greater than one page in size. If so,
  // then we need to verify there is enough stack space remaining
  // for the additional locals.
  //
  __ mov(rscratch1, (page_size - overhead_size) / Interpreter::stackElementSize);
  __ cmp(r3, rscratch1);
  __ b(after_frame_check, Assembler::LS);

  // compute rsp as if this were going to be the last frame on
  // the stack before the red zone

  const Address stack_base(rthread, Thread::stack_base_offset());
  const Address stack_size(rthread, Thread::stack_size_offset());

  // locals + overhead, in bytes
  __ mov(r0, overhead_size);
  __ add(r0, r0, r3, lsl(Interpreter::logStackElementSize));  // 1 slot per parameter.

  __ ldr(rscratch1, stack_base);
  __ ldr(rscratch2, stack_size);

#ifdef ASSERT
  Label stack_base_okay, stack_size_okay;
  // verify that thread stack base is non-zero
  __ cbnz(rscratch1, stack_base_okay);
  __ stop("stack base is zero");
  __ bind(stack_base_okay);
  // verify that thread stack size is non-zero
  __ cbnz(rscratch2, stack_size_okay);
  __ stop("stack size is zero");
  __ bind(stack_size_okay);
#endif

  // Add stack base to locals and subtract stack size
  __ sub(rscratch1, rscratch1, rscratch2); // Stack limit
  __ add(r0, r0, rscratch1);

  // Use the maximum number of pages we might bang.
  const int max_pages = StackShadowPages > (StackRedPages+StackYellowPages) ? StackShadowPages :
                                                                              (StackRedPages+StackYellowPages);

  // add in the red and yellow zone sizes
  __ add(r0, r0, max_pages * page_size * 2);

  // check against the current stack bottom
  __ cmp(sp, r0);
  __ b(after_frame_check, Assembler::HI);

  // Remove the incoming args, peeling the machine SP back to where it
  // was in the caller.  This is not strictly necessary, but unless we
  // do so the stack frame may have a garbage FP; this ensures a
  // correct call stack that we can always unwind.
  __ mov(sp, r4);

  // Note: the restored frame is not necessarily interpreted.
  // Use the shared runtime version of the StackOverflowError.
  assert(StubRoutines::throw_StackOverflowError_entry() != NULL, "stub not yet generated");
  __ far_jump(RuntimeAddress(StubRoutines::throw_StackOverflowError_entry()));

  // all done with frame size check
  __ bind(after_frame_check);
}

// Allocate monitor and lock method (asm interpreter)
//
// Args:
//      rmethod: Method*
//      rlocals: locals
//
// Kills:
//      r0
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, ...(param regs)
//      rscratch1, rscratch2 (scratch regs)
void InterpreterGenerator::lock_method(void) {
  // synchronize method
  const Address access_flags(rmethod, Method::access_flags_offset());
  const Address monitor_block_top(
        rfp,
        frame::interpreter_frame_monitor_block_top_offset * wordSize);
  const int entry_size = frame::interpreter_frame_monitor_size() * wordSize;

#ifdef ASSERT
  {
    Label L;
    __ ldr(r0, access_flags);
    __ tst(r0, JVM_ACC_SYNCHRONIZED);
    __ b(L, Assembler::NE);
    __ stop("method doesn't need synchronization");
    __ bind(L);
  }
#endif // ASSERT

  // get synchronization object
  {
    const int mirror_offset = in_bytes(Klass::java_mirror_offset());
    Label done;
    __ ldr(r0, access_flags);
    __ tst(r0, JVM_ACC_STATIC);
    // get receiver (assume this is frequent case)
    __ ldr(r0, Address(rlocals, Interpreter::local_offset_in_bytes(0)));
    __ b(done, Assembler::EQ);
    __ ldr(r0, Address(rmethod, Method::const_offset()));
    __ ldr(r0, Address(r0, ConstMethod::constants_offset()));
    __ ldr(r0, Address(r0,
                           ConstantPool::pool_holder_offset_in_bytes()));
    __ ldr(r0, Address(r0, mirror_offset));

#ifdef ASSERT
    {
      Label L;
      __ cbnz(r0, L);
      __ stop("synchronization object is NULL");
      __ bind(L);
    }
#endif // ASSERT

    __ bind(done);
  }

  // add space for monitor & lock
  __ sub(sp, sp, entry_size); // add space for a monitor entry
  __ mov(rscratch1, sp);
  __ str(rscratch1, monitor_block_top);  // set new monitor block top
  // store object
  __ str(r0, Address(sp, BasicObjectLock::obj_offset_in_bytes()));
  __ mov(c_rarg1, sp); // object address
  __ lock_object(c_rarg1);
}

// Generate a fixed interpreter frame. This is identical setup for
// interpreted methods and for native methods hence the shared code.
//
// Args:
//      lr: return address
//      rmethod: Method*
//      rlocals: pointer to locals
//      rcpool: cp cache
//      stack_pointer: previous sp
//      r4 contains the sender sp
void TemplateInterpreterGenerator::generate_fixed_frame(bool native_call) {
  // initialize fixed part of activation frame
  __ reg_printf("About to print native entry, rmethod = %p\n", rmethod);
  /*__ mov(rscratch1, (address)0x62829d20);
  __ cmp(rscratch1, rmethod);
  Label skip;
  __ b(skip, Assembler::NE);
  __ bkpt(111);
  __ bind(skip);*/

  __ print_method_entry(rmethod, native_call);

  if (native_call) {
    __ sub(sp, sp, 12 *  wordSize);
    __ mov(rbcp, 0);
    __ strd(sp, rbcp, Address(sp));
    // add 2 zero-initialized slots for native calls
    __ strd(rbcp, rbcp, Address(sp, 10 * wordSize));
    // Note using rbcp in strd
  } else {
    __ sub(sp, sp, 10 *  wordSize);
    __ ldr(rscratch1, Address(rmethod, Method::const_offset()));      // get ConstMethod
    __ add(rbcp, rscratch1, in_bytes(ConstMethod::codes_offset())); // get codebase
    __ strd(sp, rbcp, Address(sp));
  }

  if (ProfileInterpreter) {
    Label method_data_continue;
    __ ldr(rscratch1, Address(rmethod, Method::method_data_offset()));
    __ cbz(rscratch1, method_data_continue);
    __ lea(rscratch1, Address(rscratch1, in_bytes(MethodData::data_offset())));
    __ bind(method_data_continue);
    __ strd(rscratch1, rmethod, Address(sp, 4 * wordSize));  // save Method* and mdp (method data pointer)
  } else {
    __ mov(rscratch1, 0);
    __ strd(rscratch1, rmethod, Address(sp, 4 * wordSize));        // save Method* (no mdp)
  }
  __ ldr(rcpool, Address(rmethod, Method::const_offset()));
  __ ldr(rcpool, Address(rcpool, ConstMethod::constants_offset()));
  __ ldr(rcpool, Address(rcpool, ConstantPool::cache_offset_in_bytes()));
  __ strd(rlocals, rcpool, Address(sp, 2 * wordSize));

  // this code sets up the stack frame, in the same fashion as enter()
  __ strd(rfp, lr, Address(sp, 8 * wordSize));
  // point rfp to location of old pc
  __ add(rfp, sp, 9 * wordSize);

  __ reg_printf("Three-quarters through\n");
  // set sender sp
  // leave last_sp as null
  __ mov(rscratch1, 0);
  // r4 contains the sender sp
  __ strd(rscratch1, r4, Address(sp, 6 * wordSize));

  // Move SP out of the way
  /*if (! native_call) {
    __ ldr(rscratch1, Address(rmethod, Method::const_offset()));
    __ ldrh(rscratch1, Address(rscratch1, ConstMethod::max_stack_offset()));
    __ add(rscratch1, rscratch1, frame::interpreter_frame_monitor_size() + 2);
    __ sub(rscratch1, sp, rscratch1, lsl(2));
    __ bic(sp, rscratch1, 0xf);
  }*/
  // FIXME This code moves the sp to after the end of the stack - if this is what's happening
  // some calls out of the VM may need to be patched
  __ reg_printf("Fully through\n");
}

// End of helpers

// Various method entries
//------------------------------------------------------------------------------------------------------------------------
//
//

// Method entry for java.lang.ref.Reference.get.
address InterpreterGenerator::generate_Reference_get_entry(void) {
#if INCLUDE_ALL_GCS
  // Code: _aload_0, _getfield, _areturn
  // parameter size = 1
  //
  // The code that gets generated by this routine is split into 2 parts:
  //    1. The "intrinsified" code for G1 (or any SATB based GC),
  //    2. The slow path - which is an expansion of the regular method entry.
  //
  // Notes:-
  // * In the G1 code we do not check whether we need to block for
  //   a safepoint. If G1 is enabled then we must execute the specialized
  //   code for Reference.get (except when the Reference object is null)
  //   so that we can log the value in the referent field with an SATB
  //   update buffer.
  //   If the code for the getfield template is modified so that the
  //   G1 pre-barrier code is executed when the current method is
  //   Reference.get() then going through the normal method entry
  //   will be fine.
  // * The G1 code can, however, check the receiver object (the instance
  //   of java.lang.Reference) and jump to the slow path if null. If the
  //   Reference object is null then we obviously cannot fetch the referent
  //   and so we don't need to call the G1 pre-barrier. Thus we can use the
  //   regular method entry code to generate the NPE.
  //
  // This code is based on generate_accessor_enty.
  //
  // rmethod: Method*
  // r13: senderSP must preserve for slow path, set SP to it on fast path

  address entry = __ pc();

  const int referent_offset = java_lang_ref_Reference::referent_offset;
  guarantee(referent_offset > 0, "referent offset not initialized");

  if (UseG1GC) {
    Label slow_path;
    const Register local_0 = c_rarg0;
    // Check if local 0 != NULL
    // If the receiver is null then it is OK to jump to the slow path.
    __ ldr(local_0, Address(sp, 0));
    __ cbz(local_0, slow_path);


    // Load the value of the referent field.
    const Address field_address(local_0, referent_offset);
    __ load_heap_oop(local_0, field_address);

    // Generate the G1 pre-barrier code to log the value of
    // the referent field in an SATB buffer.
    __ enter(); // g1_write may call runtime
    __ g1_write_barrier_pre(noreg /* obj */,
                            local_0 /* pre_val */,
                            rthread /* thread */,
                            rscratch2 /* tmp */,
                            true /* tosca_live */,
                            true /* expand_call */);
    __ leave();
    // areturn
    __ mov(sp, r4);           // set sp to sender sp
    __ stop("Check sp restored correctly, may be get_dispatch()?");
    //__ bic(sp, r13, 0xf);  // done with stack
    __ b(lr);

    // generate a vanilla interpreter entry as the slow path
    __ bind(slow_path);
    (void) generate_normal_entry(false);

    return entry;
  }
#endif // INCLUDE_ALL_GCS

  // If G1 is not enabled then attempt to go through the accessor entry point
  // Reference.get is an accessor
  return generate_accessor_entry();
}

void InterpreterGenerator::bang_stack_shadow_pages(bool native_call, Register rscratch) {
  // Bang each page in the shadow zone. We can't assume it's been done for
  // an interpreter frame with greater than a page of locals, so each page
  // needs to be checked.  Only true for non-native.
  assert(rscratch != rscratch2, "can't be");
  if (UseStackBanging) {
    const int start_page = native_call ? StackShadowPages : 1;
    const int page_size = os::vm_page_size();
    __ mov(rscratch, 0);
    for (int pages = start_page; pages <= StackShadowPages ; pages++) {
      __ sub(rscratch2, sp, pages*page_size);
      __ str(rscratch, Address(rscratch2));
    }
  }
}


// Interpreter stub for calling a native method. (asm interpreter)
// This sets up a somewhat different looking stack for calling the
// native method than the typical interpreter frame setup.
address InterpreterGenerator::generate_native_entry(bool synchronized) {
  // determine code generation flags
  bool inc_counter  = UseCompiler || CountCompiledCalls;

  // r1: Method*
  // r4: sender sp

  address entry_point = __ pc();
  __ reg_printf("entering generate_native_entry, lr = %p, rfp = %p\n\tRBCP = %p\n", lr, rfp, rbcp);

  const Address constMethod       (rmethod, Method::const_offset());
  const Address access_flags      (rmethod, Method::access_flags_offset());
  const Address size_of_parameters(r2, ConstMethod::
                                       size_of_parameters_offset());

  // get parameter size (always needed)
  __ ldr(r2, constMethod);
  __ load_unsigned_short(r2, size_of_parameters);

  // native calls don't need the stack size check since they have no
  // expression stack and the arguments are already on the stack and
  // we only add a handful of words to the stack

  // rmethod: Method*
  // r2: size of parameters
  // r4: sender sp

  // for natives the size of locals is zero

  // compute beginning of parameters (rlocals)
  __ add(rlocals, sp, r2, lsl(2));
  __ sub(rlocals, rlocals, wordSize);
  __ reg_printf("(start of parameters) rlocals = %p, nparams = %d\n", rlocals, r2);

  // initialize fixed part of activation frame
  generate_fixed_frame(true);
  __ reg_printf("pushed new fixed frame, lr = %p, rfp = %p\n", lr, rfp);

  Register locals_sp = r4; // the overwrites rdispatch, we can restore at end
  // !! If this canges, change the end of arguements in interpreterRT_aarch32.cpp
  //__ mov(r4, sp); //Save top of arguments

  // make sure method is native & not abstract
#ifdef ASSERT
  __ ldr(r0, access_flags);
  {
    Label L;
    __ tst(r0, JVM_ACC_NATIVE);
    __ b(L, Assembler::NE);
    __ stop("tried to execute non-native method as native");
    __ bind(L);
  }
  {
    Label L;
    __ tst(r0, JVM_ACC_ABSTRACT);
    __ b(L, Assembler::EQ);
    __ stop("tried to execute abstract method in interpreter");
    __ bind(L);
  }
#endif

  // Since at this point in the method invocation the exception
  // handler would try to exit the monitor of synchronized methods
  // which hasn't been entered yet, we set the thread local variable
  // _do_not_unlock_if_synchronized to true. The remove_activation
  // will check this flag.

   const Address do_not_unlock_if_synchronized(rthread,
        in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()));
  __ mov(rscratch2, true);
  __ strb(rscratch2, do_not_unlock_if_synchronized);

  // increment invocation count & check for overflow
  Label invocation_counter_overflow;
  if (inc_counter) {
    generate_counter_incr(&invocation_counter_overflow, NULL, NULL);
  }

  Label continue_after_compile;
  __ bind(continue_after_compile);

  bang_stack_shadow_pages(true, rscratch1);
  // Note rscratch1 will contain zero here due to bang_stack_shadow_pages
  // reset the _do_not_unlock_if_synchronized flag
  //__ mov(rscratch1, 0);
  __ strb(rscratch1, do_not_unlock_if_synchronized);

  // check for synchronized methods
  // Must happen AFTER invocation_counter check and stack overflow check,
  // so method is not locked if overflows.
  if (synchronized) {
    lock_method();
  } else {
    // no synchronization necessary
#ifdef ASSERT
    {
      Label L;
      __ ldr(r0, access_flags);
      __ tst(r0, JVM_ACC_SYNCHRONIZED);
      __ b(L, Assembler::EQ);
      __ stop("method needs synchronization");
      __ bind(L);
    }
#endif
  }

  // start execution
#ifdef ASSERT
  {
    Label L;
    const Address monitor_block_top(rfp,
                 frame::interpreter_frame_monitor_block_top_offset * wordSize);
    __ ldr(rscratch1, monitor_block_top);
    __ cmp(sp, rscratch1);
    __ b(L, Assembler::EQ);
    __ stop("broken stack frame setup in interpreter");
    __ bind(L);
  }
#endif

  // jvmti support
  __ notify_method_entry();

  const Register result_handler = rlocals;
  //This is recomputed for the new function and result_handler is not written until
  // after the function has been called

  // allocate space for parameters
  __ ldr(rscratch1, Address(rmethod, Method::const_offset()));
  __ load_unsigned_short(rscratch1, Address(rscratch1, ConstMethod::size_of_parameters_offset()));

  __ sub(sp, sp, rscratch1, lsl(Interpreter::logStackElementSize + 1));
  // This +1 is a hack to double the amount of space allocated for parameters, this is likely far
  // more than needed as in the worst case when parameters have to be placed on the stack they would be aligned
  // as follows LONG | INT | EMPTY | LONG ... This would only increase the space used by a half.
  __ align_stack();
  __ mov(locals_sp, sp);
  __ reg_printf("Stack Pointer on arg copy, sp = %p, locals_sp = %p, rlocals = %p\n", sp, locals_sp, rlocals);

  // get signature handler
  {
    Label L;
    __ ldr(rscratch1, Address(rmethod, Method::signature_handler_offset()));
    __ cmp(rscratch1, 0);
    __ b(L, Assembler::NE);
    __ reg_printf("Prepare_native_call, locals_sp = %p, rlocals = %p\n", locals_sp, rlocals);
    __ call_VM(noreg, CAST_FROM_FN_PTR(address,
                                       InterpreterRuntime::prepare_native_call), rmethod);
    __ reg_printf("Finished prepare_native_call, locals_sp = %p, rlocals = %p\n", locals_sp, rlocals);
    __ ldr(rscratch1, Address(rmethod, Method::signature_handler_offset()));
    __ bind(L);
  }

  // call signature handler
  assert(InterpreterRuntime::SignatureHandlerGenerator::from() == rlocals,
         "adjust this code");
  assert(InterpreterRuntime::SignatureHandlerGenerator::to() == locals_sp,
         "adjust this code");
  assert(InterpreterRuntime::SignatureHandlerGenerator::temp() == rscratch1,
          "adjust this code");

  // The generated handlers do not touch rmethod (the method).
  // However, large signatures cannot be cached and are generated
  // each time here.  The slow-path generator can do a GC on return,
  // so we must reload it after the call.
  __ reg_printf("**BEFORE**\nrlocals = %p,locals_sp = %p, sp = %p\n", rlocals, locals_sp, sp);
  __ reg_printf("About to call the Method::signature_handler = %p\n", rscratch1);
  __ bl(rscratch1);
  __ reg_printf("**AFTER**\nr0 : %p, r1 : %p, r2 : %p\n", r0, r1, r2);
  __ reg_printf("r3 : %p, sp : %p\n", r3, sp);
  __ get_method(rmethod);        // slow path can do a GC, reload rmethod



  // result handler is in r0
  // set result handler
  __ mov(result_handler, r0);
  // pass mirror handle if static call
  {
    Label L;
    const int mirror_offset = in_bytes(Klass::java_mirror_offset());
    __ ldr(rscratch1, Address(rmethod, Method::access_flags_offset()));
    __ tst(rscratch1, JVM_ACC_STATIC);
    __ b(L, Assembler::EQ);
    // get mirror
    __ ldr(rscratch1, Address(rmethod, Method::const_offset()));
    __ ldr(rscratch1, Address(rscratch1, ConstMethod::constants_offset()));
    __ ldr(rscratch1, Address(rscratch1, ConstantPool::pool_holder_offset_in_bytes()));
    __ ldr(rscratch1, Address(rscratch1, mirror_offset));
    // copy mirror into activation frame
    __ str(rscratch1, Address(rfp, frame::interpreter_frame_oop_temp_offset * wordSize));
    // pass handle to mirror
    __ add(c_rarg1, rfp, frame::interpreter_frame_oop_temp_offset * wordSize);
    __ bind(L);
  }

  // get native function entry point in r14
  Register native_entry_point = r14;

  {
    Label L;
    __ ldr(native_entry_point, Address(rmethod, Method::native_function_offset()));
    address unsatisfied = (SharedRuntime::native_method_throw_unsatisfied_link_error_entry());
    __ mov(rscratch2, unsatisfied);
    __ ldr(rscratch2, rscratch2);
    __ reg_printf("QWERTY native_entry_point = %p, unsatisfied_link_entry_point = %p\n", native_entry_point, rscratch2);
    __ cmp(native_entry_point, rscratch2);
    __ b(L, Assembler::NE);
    __ call_VM(noreg, CAST_FROM_FN_PTR(address,
                                       InterpreterRuntime::prepare_native_call), rmethod);
    __ get_method(rmethod);
    __ ldr(native_entry_point, Address(rmethod, Method::native_function_offset()));
    __ bind(L);
  }

  // pass JNIEnv
  __ add(c_rarg0, rthread, in_bytes(JavaThread::jni_environment_offset()));

  // It is enough that the pc() points into the right code
  // segment. It does not have to be the correct return pc.
  __ set_last_Java_frame(sp, rfp, (address)NULL, rscratch1);

  // change thread state
#ifdef ASSERT
  {
    Label L;
    __ ldr(rscratch1, Address(rthread, JavaThread::thread_state_offset()));
    __ cmp(rscratch1, _thread_in_Java);
    __ b(L, Assembler::EQ);
    __ stop("Wrong thread state in native stub");
    __ bind(L);
  }
#endif

  // Change state to native
  __ mov(rscratch1, _thread_in_native);
  __ lea(rscratch2, Address(rthread, JavaThread::thread_state_offset()));
  __ dmb(Assembler::ISH);
  __ str(rscratch1, Address(rscratch2));

  __ reg_printf("Calling native method, lr = %p & rmethod = %p\n", lr, rmethod);
  // Call the native method.
  /*__ reg_printf("**ONCALL**\nr0 : %p\nr1 : %p\nr2 : %p\n", r0, r1, r2);
  __ reg_printf("r3 : %p\n\nr4 : %p\nrloc : %p\n", r3, r4, rlocals);*/
  __ reg_printf("Stack Pointer on entry to native, sp = %p\n", sp);
  __ bl(native_entry_point);
  __ reg_printf("Returned from native, lr = %p, r1 = %p, r0 = %p\n", lr, r1, r0);
  __ maybe_isb();
  __ get_method(rmethod);
  // result potentially in r0, <r0:r1> or v0

  // make room for the pushes we're about to do
  //__ sub(rscratch1, sp, 4 * wordSize);
  //__ bic(sp, rscratch1, 0xf);
  // NOTE: The order of these pushes is known to frame::interpreter_frame_result
  // in order to extract the result of a method call. If the order of these
  // pushes change or anything else is added to the stack then the code in
  // interpreter_frame_result must also change.
  __ reg_printf("Before push dtos, ltos. sp = %p\n", sp);
  __ push(dtos);
  __ push(ltos);

  // change thread state
  __ mov(rscratch1, _thread_in_native_trans);
  __ lea(rscratch2, Address(rthread, JavaThread::thread_state_offset()));
  __ dmb(Assembler::ISH);
  __ str(rscratch1, Address(rscratch2));
  __ reg_printf("before os::is_MP\n");
  if (os::is_MP()) {
    if (UseMembar) {
      // Force this write out before the read below
      __ membar(Assembler::AnyAny);
    } else {
      // Write serialization page so VM thread can do a pseudo remote membar.
      // We use the current thread pointer to calculate a thread specific
      // offset to write to within the page. This minimizes bus traffic
      // due to cache line collision.
      __ serialize_memory(rthread, rscratch2);
    }
  }
  __ reg_printf("after os::is_MP\n");
  // check for safepoint operation in progress and/or pending suspend requests
  {
    Label Continue;
    __ lea(rscratch2, ExternalAddress(SafepointSynchronize::address_of_state()));
    assert(SafepointSynchronize::_not_synchronized == 0,
           "SafepointSynchronize::_not_synchronized");
    __ ldr(rscratch2, rscratch2);
    Label L;
    __ cbnz(rscratch2, L);
    __ ldr(rscratch2, Address(rthread, JavaThread::suspend_flags_offset()));
    __ cbz(rscratch2, Continue);
    __ bind(L);

    // Don't use call_VM as it will see a possible pending exception
    // and forward it and never return here preventing us from
    // clearing _last_native_pc down below. So we do a runtime call by
    // hand.
    //
    __ mov(c_rarg0, rthread);
    __ mov(rscratch2, CAST_FROM_FN_PTR(address, JavaThread::check_special_condition_for_native_trans));
    //__ blrt(rscratch2, 1, 0, 0);
    __ bl(rscratch2);
    __ maybe_isb();
    __ get_method(rmethod);
    __ bind(Continue);
  }
  __ reg_printf("finished safepoint check\n");
  // change thread state
  __ mov(rscratch1, _thread_in_Java);
  __ lea(rscratch2, Address(rthread, JavaThread::thread_state_offset()));
  __ dmb(Assembler::ISH);
  __ str(rscratch1, Address(rscratch2));

  // reset_last_Java_frame
  __ reset_last_Java_frame(true);

  // reset handle block
  __ ldr(rscratch2, Address(rthread, JavaThread::active_handles_offset()));
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rscratch2, JNIHandleBlock::top_offset_in_bytes()));

  // If result is an oop unbox and store it in frame where gc will see it
  // and result handler will pick it up
  __ reg_printf("finished checking last_Java_frame\n");
  {
    Label no_oop, store_result;
    //__ bkpt(345);
    //__ adr(rscratch2, ExternalAddress(AbstractInterpreter::result_handler(T_OBJECT)));
    __ mov(rscratch2, ExternalAddress(AbstractInterpreter::result_handler(T_OBJECT)));
    __ reg_printf("Comparing rscratch2 = %p and result_handler = %p\n", rscratch2, result_handler);

    __ cmp(rscratch2, result_handler);
    __ b(no_oop, Assembler::NE);
    __ reg_printf("It's an oop.\n");
    // retrieve result
    __ pop(ltos);
    __ resolve_jobject(r0, rthread, rscratch1);
    __ str(r0, Address(rfp, frame::interpreter_frame_oop_temp_offset*wordSize));
    // keep stack depth as expected by pushing oop which will eventually be discarded
    __ push(ltos);
    __ bind(no_oop);
  }

  {
    Label no_reguard;
    __ lea(rscratch1, Address(rthread, in_bytes(JavaThread::stack_guard_state_offset())));
    __ ldrb(rscratch1, Address(rscratch1));
    __ cmp(rscratch1, JavaThread::stack_guard_yellow_disabled);
    __ b(no_reguard, Assembler::NE);

    __ pusha(); // XXX only save smashed registers
    __ mov(c_rarg0, rthread);
    __ mov(rscratch2, CAST_FROM_FN_PTR(address, SharedRuntime::reguard_yellow_pages));
    __ bl(rscratch2);
    __ popa(); // XXX only restore smashed registers
    __ bind(no_reguard);
  }
  __ reg_printf("Restoring java-ish things\n");
  // The method register is junk from after the thread_in_native transition
  // until here.  Also can't call_VM until the bcp has been
  // restored.  Need bcp for throwing exception below so get it now.
  __ get_method(rmethod);
  __ get_dispatch(); // used to save sp in for args
  // restore bcp to have legal interpreter frame, i.e., bci == 0 <=>
  // rbcp == code_base()
  __ ldr(rbcp, Address(rmethod, Method::const_offset()));   // get ConstMethod*
  __ add(rbcp, rbcp, in_bytes(ConstMethod::codes_offset()));          // get codebase
  // handle exceptions (exception handling will handle unlocking!)
  {
    Label L;
    __ reg_printf("Checking pending exceptions\n");
    __ ldr(rscratch1, Address(rthread, Thread::pending_exception_offset()));
    __ cbz(rscratch1, L);
    // Note: At some point we may want to unify this with the code
    // used in call_VM_base(); i.e., we should use the
    // StubRoutines::forward_exception code. For now this doesn't work
    // here because the rsp is not correctly set at this point.
    __ reg_printf("Calling vm to throw_pending_exception\n");

    // Need to restore lr? - introduced on aarch32 port
    //__ ldr(lr, Address(rfp, frame::return_addr_offset));

    __ MacroAssembler::call_VM(noreg,
                               CAST_FROM_FN_PTR(address,
                               InterpreterRuntime::throw_pending_exception));
    __ should_not_reach_here();
    __ bind(L);
  }

  // do unlocking if necessary
  {
    Label L;
    __ reg_printf("testing if we need to unlock\n");
    __ ldr(rscratch1, Address(rmethod, Method::access_flags_offset()));
    __ tst(rscratch1, JVM_ACC_SYNCHRONIZED);
    __ b(L, Assembler::EQ);
    // the code below should be shared with interpreter macro
    // assembler implementation
    {
      Label unlock;
      // BasicObjectLock will be first in list, since this is a
      // synchronized method. However, need to check that the object
      // has not been unlocked by an explicit monitorexit bytecode.

      // monitor expect in c_rarg1 for slow unlock path
      __ lea (c_rarg1, Address(rfp,   // address of first monitor
                               (intptr_t)(frame::interpreter_frame_initial_sp_offset *
                                          wordSize - sizeof(BasicObjectLock))));

      __ ldr(rscratch1, Address(c_rarg1, BasicObjectLock::obj_offset_in_bytes()));
      __ reg_printf("Checking if we are already unlocked\n");
      __ cbnz(rscratch1, unlock);

      // Entry already unlocked, need to throw exception
      __ MacroAssembler::call_VM(noreg,
                                 CAST_FROM_FN_PTR(address,
                   InterpreterRuntime::throw_illegal_monitor_state_exception));
      __ should_not_reach_here();

      __ bind(unlock);
      __ reg_printf("Doing unlock\n");
      __ unlock_object(c_rarg1);
    }
    __ bind(L);
  }
  __ reg_printf("finished unlocking\n");
  // jvmti support
  // Note: This must happen _after_ handling/throwing any exceptions since
  //       the exception handler code notifies the runtime of method exits
  //       too. If this happens before, method entry/exit notifications are
  //       not properly paired (was bug - gri 11/22/99).
  __ notify_method_exit(vtos, InterpreterMacroAssembler::NotifyJVMTI);

  // restore potential result in r0:d0, call result handler to
  // restore potential result in ST0 & handle result
  __ reg_printf("Before pop dtos, ltos. sp = %p\n", sp);
  __ pop(ltos);
  __ pop(dtos);

  __ reg_printf("Calling result handler, r1 = %p, r0 = %p\n", r1, r0);
  __ bl(result_handler);
  __ reg_printf("Finished result_handler\n RFP NOW = %p, r0 = %p\n", rfp, r0);

  // remove activation restore sp to sender_sp
  __ ldr(rscratch1, Address(rfp,
                    frame::interpreter_frame_sender_sp_offset *
                    wordSize)); // get sender sp
  // remove frame anchor & restore sp
  __ leave();

  __ mov(sp, rscratch1); // Native frame so two extra fields
  __ reg_printf("Returning to Java execution, restored frame = %p, lr = %p\n\tRBCP = %p\n", rfp, lr, rbcp);
  __ b(lr);

  if (inc_counter) {
    // Handle overflow of counter and compile method
    __ bind(invocation_counter_overflow);
    generate_counter_overflow(&continue_after_compile);
  }

  return entry_point;
}

address InterpreterGenerator::generate_CRC32_update_entry() {
  if (UseCRC32Intrinsics) {
    address entry = __ pc();

    // rmethod: Method*
    // sp: args

    Label slow_path;
    // If we need a safepoint check, generate full interpreter entry.
    __ lea(rscratch2, ExternalAddress(SafepointSynchronize::address_of_state()));
    assert(SafepointSynchronize::_not_synchronized == 0, "rewrite this code");
    __ ldr(rscratch2, Address(rscratch2));
    __ cbnz(rscratch2, slow_path);

    // We don't generate local frame and don't align stack because
    // we call stub code and there is no safepoint on this path.

    // Load parameters
    const Register crc = c_rarg0;  // crc
    const Register val = c_rarg1;  // source java byte value
    const Register tbl = c_rarg2;  // scratch

    // Arguments are reversed on java expression stack
    __ ldr(val, Address(sp, 0));              // byte value
    __ ldr(crc, Address(sp, wordSize));       // Initial CRC

    __ lea(tbl, ExternalAddress(StubRoutines::crc_table_addr()));
    __ inv(crc, crc);
    __ update_byte_crc32(crc, val, tbl);
    __ inv(crc, crc); // result in c_rarg0

    __ mov(sp, r4);
    __ ret(lr);

    // generate a vanilla native entry as the slow path
    __ bind(slow_path);

    (void) generate_native_entry(false);

    return entry;
  }
  return generate_native_entry(false);
}

address InterpreterGenerator::generate_CRC32_updateBytes_entry(AbstractInterpreter::MethodKind kind) {
  if (UseCRC32Intrinsics) {
    address entry = __ pc();

    // rmethod,: Method*
    // sp: senderSP must preserved for slow path

    Label slow_path;
    // If we need a safepoint check, generate full interpreter entry.
    __ lea(rscratch2, ExternalAddress(SafepointSynchronize::address_of_state()));
    assert(SafepointSynchronize::_not_synchronized == 0, "rewrite this code");
    __ ldr(rscratch2, Address(rscratch2));
    __ cbnz(rscratch2, slow_path);

    // We don't generate local frame and don't align stack because
    // we call stub code and there is no safepoint on this path.

    // Load parameters
    const Register crc = c_rarg0;  // crc
    const Register buf = c_rarg1;  // source java byte array address
    const Register len = c_rarg2;  // length
    const Register off = len;      // offset (never overlaps with 'len')

    // Arguments are reversed on java expression stack
    // Calculate address of start element
    if (kind == Interpreter::java_util_zip_CRC32_updateByteBuffer) {
      __ ldr(buf, Address(sp, 2*wordSize)); // long buf
      __ ldr(off, Address(sp, wordSize)); // offset
      __ add(buf, buf, off); // + offset
      __ ldr(crc, Address(sp, 4*wordSize)); // Initial CRC
    } else {
      __ ldr(buf, Address(sp, 2*wordSize)); // byte[] array
      __ add(buf, buf, arrayOopDesc::base_offset_in_bytes(T_BYTE)); // + header size
      __ ldr(off, Address(sp, wordSize)); // offset
      __ add(buf, buf, off); // + offset
      __ ldr(crc, Address(sp, 3*wordSize)); // Initial CRC
    }
    // Can now load 'len' since we're finished with 'off'
    __ ldr(len, Address(sp)); // Length

    __ mov(sp, r4); // Restore the caller's SP

    // We are frameless so we can just jump to the stub.
    __ b(CAST_FROM_FN_PTR(address, StubRoutines::updateBytesCRC32()));

    // generate a vanilla native entry as the slow path
    __ bind(slow_path);

    (void) generate_native_entry(false);

    return entry;
  }
  return generate_native_entry(false);
}

//
// Generic interpreted method entry to (asm) interpreter
//
address InterpreterGenerator::generate_normal_entry(bool synchronized) {
  // determine code generation flags
  bool inc_counter = UseCompiler || CountCompiledCalls;

  // r4: sender sp
  address entry_point = __ pc();

  const Address constMethod(rmethod, Method::const_offset());
  const Address access_flags(rmethod, Method::access_flags_offset());
  const Address size_of_parameters(r3,
                                   ConstMethod::size_of_parameters_offset());
  const Address size_of_locals(r3, ConstMethod::size_of_locals_offset());

  // get parameter size (always needed)
  // need to load the const method first
  __ ldr(r3, constMethod);
  __ load_unsigned_short(r2, size_of_parameters);

  // r2: size of parameters

  __ load_unsigned_short(r3, size_of_locals); // get size of locals in words
  __ sub(r3, r3, r2); // r3 = no. of additional locals

  // see if we've got enough room on the stack for locals plus overhead.
  generate_stack_overflow_check();

  // compute beginning of parameters (rlocals)
  __ add(rlocals, sp, r2, lsl(2));
  __ sub(rlocals, rlocals, wordSize);

  // Make room for locals
  __ sub(rscratch1, sp, r3, lsl(2));
  // Align the sp value
  __ bic(sp, rscratch1, StackAlignmentInBytes-1);

  // r3 - # of additional locals
  // allocate space for locals
  // explicitly initialize locals
  {
    Label exit, loop;
    __ mov(rscratch2, 0);
    __ cmp(r3, 0);
    __ b(exit, Assembler::LE); // do nothing if r3 <= 0
    __ bind(loop);
    __ str(rscratch2, Address(__ post(rscratch1, wordSize)));
    __ subs(r3, r3, 1); // until everything initialized
    __ b(loop, Assembler::NE);
    __ bind(exit);
  }
  __ reg_printf("Done locals space\n", r2);

  // initialize fixed part of activation frame
  __ reg_printf("About to do fixed frame\n", r2);
  generate_fixed_frame(false);
  // And the base dispatch table
  __ get_dispatch();
  // make sure method is not native & not abstract
  __ reg_printf("Just done generate_fixed_frame; rmethod = %p\n", rmethod);
#ifdef ASSERT
  __ ldr(r0, access_flags);
  {
    Label L;
    __ tst(r0, JVM_ACC_NATIVE);
    __ b(L, Assembler::EQ);
    __ stop("tried to execute native method as non-native");
    __ bind(L);
  }
  {
    Label L;
    __ tst(r0, JVM_ACC_ABSTRACT);
    __ b(L, Assembler::EQ);
    __ stop("tried to execute abstract method in interpreter");
    __ bind(L);
  }
#endif

  // Since at this point in the method invocation the exception
  // handler would try to exit the monitor of synchronized methods
  // which hasn't been entered yet, we set the thread local variable
  // _do_not_unlock_if_synchronized to true. The remove_activation
  // will check this flag.

   const Address do_not_unlock_if_synchronized(rthread,
        in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()));
  __ mov(rscratch2, true);
  __ strb(rscratch2, do_not_unlock_if_synchronized);

  // increment invocation count & check for overflow
  Label invocation_counter_overflow;
  Label profile_method;
  Label profile_method_continue;
  if (inc_counter) {
    generate_counter_incr(&invocation_counter_overflow,
                          &profile_method,
                          &profile_method_continue);
    if (ProfileInterpreter) {
      __ bind(profile_method_continue);
    }
  }

  Label continue_after_compile;
  __ bind(continue_after_compile);

  bang_stack_shadow_pages(false, rscratch1);
  // Note rscratch1 will contain zero here
  // reset the _do_not_unlock_if_synchronized flag
  __ strb(rscratch1, do_not_unlock_if_synchronized);

  // check for synchronized methods
  // Must happen AFTER invocation_counter check and stack overflow check,
  // so method is not locked if overflows.
  if (synchronized) {
    // Allocate monitor and lock method
    lock_method();
  } else {
    // no synchronization necessary
#ifdef ASSERT
    {
      Label L;
      __ reg_printf("Checking synchronization, rmethod = %p\n", rmethod);
      __ ldr(r0, access_flags);
      __ tst(r0, JVM_ACC_SYNCHRONIZED);
      __ b(L, Assembler::EQ);
      __ stop("method needs synchronization");
      __ bind(L);
    }
#endif
  }

  // start execution
#ifdef ASSERT
  {
    Label L;
     const Address monitor_block_top (rfp,
                 frame::interpreter_frame_monitor_block_top_offset * wordSize);
    __ ldr(rscratch1, monitor_block_top);
    __ cmp(sp, rscratch1);
    __ b(L, Assembler::EQ);
    __ stop("broken stack frame setup in interpreter");
    __ bind(L);
  }
#endif

  // jvmti support
  __ notify_method_entry();
  __ reg_printf("About to dispatch, rmethod = %p, rlocals = %p\n", rmethod, rlocals);
  __ dispatch_next(vtos);
  __ reg_printf("Finshed dispatch? rmethod = %p\n", rmethod);
  // invocation counter overflow
  if (inc_counter) {
    if (ProfileInterpreter) {
      // We have decided to profile this method in the interpreter
      __ bind(profile_method);
      __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::profile_method));
      __ set_method_data_pointer_for_bcp();
      // don't think we need this
      __ get_method(r1);
      __ b(profile_method_continue);
    }
    // Handle overflow of counter and compile method
    __ bind(invocation_counter_overflow);
    generate_counter_overflow(&continue_after_compile);
  }

  __ reg_printf("Just completed normal entry, rmethod = %p\n", rmethod);
  return entry_point;
}

address AbstractInterpreterGenerator::generate_method_entry(
    AbstractInterpreter::MethodKind kind) {
  bool synchronized = false;
  address entry_point = NULL;
  InterpreterGenerator* ig = (InterpreterGenerator*) this;

  switch (kind) {
  case Interpreter::zerolocals:
    break;
  case Interpreter::zerolocals_synchronized:
    synchronized = true;
    break;
  case Interpreter::native:
    entry_point = ig->generate_native_entry(false);
    break;
  case Interpreter::native_synchronized:
    entry_point = ig->generate_native_entry(true);
    break;
  case Interpreter::empty:
    entry_point = ig->generate_empty_entry();
    break;
  case Interpreter::accessor:
    entry_point = ig->generate_accessor_entry();
    break;
  case Interpreter::abstract:
    entry_point = ig->generate_abstract_entry();
    break;
  case Interpreter::java_lang_math_sin:
  case Interpreter::java_lang_math_cos:
  case Interpreter::java_lang_math_tan:
  case Interpreter::java_lang_math_abs:
  case Interpreter::java_lang_math_sqrt:
  case Interpreter::java_lang_math_log:
  case Interpreter::java_lang_math_log10:
  case Interpreter::java_lang_math_pow:
  case Interpreter::java_lang_math_exp:
    entry_point = ig->generate_math_entry(kind);
    break;
  case Interpreter::java_lang_ref_reference_get:
    entry_point = ig->generate_Reference_get_entry();
    break;
  case Interpreter::java_util_zip_CRC32_update:
    entry_point = ig->generate_CRC32_update_entry();
    break;
  case Interpreter::java_util_zip_CRC32_updateBytes:
  case Interpreter::java_util_zip_CRC32_updateByteBuffer:
    entry_point = ig->generate_CRC32_updateBytes_entry(kind);
    break;
  default:
    ShouldNotReachHere();
    break;
  }

  if (entry_point != NULL) {
    return entry_point;
  }

  return ig->generate_normal_entry(synchronized);
}

// These should never be compiled since the interpreter will prefer
// the compiled version to the intrinsic version.
bool AbstractInterpreter::can_be_compiled(methodHandle m) {
  switch (method_kind(m)) {
    case Interpreter::java_lang_math_sin     : // fall thru
    case Interpreter::java_lang_math_cos     : // fall thru
    case Interpreter::java_lang_math_tan     : // fall thru
    case Interpreter::java_lang_math_abs     : // fall thru
    case Interpreter::java_lang_math_log     : // fall thru
    case Interpreter::java_lang_math_log10   : // fall thru
    case Interpreter::java_lang_math_sqrt    : // fall thru
    case Interpreter::java_lang_math_pow     : // fall thru
    case Interpreter::java_lang_math_exp     :
      return false;
    default:
      return true;
  }
}

// How much stack a method activation needs in words.
int AbstractInterpreter::size_top_interpreter_activation(Method* method) {
  const int entry_size = frame::interpreter_frame_monitor_size();

  // total overhead size: entry_size + (saved rfp thru expr stack
  // bottom).  be sure to change this if you add/subtract anything
  // to/from the overhead area
  const int overhead_size =
    -(frame::interpreter_frame_initial_sp_offset) + entry_size;

  const int stub_code = frame::entry_frame_after_call_words;
  const int method_stack = (method->max_locals() + method->max_stack()) *
                           Interpreter::stackElementWords;
  return (overhead_size + method_stack + stub_code);
}

// asm based interpreter deoptimization helpers
int AbstractInterpreter::size_activation(int max_stack,
                                         int temps,
                                         int extra_args,
                                         int monitors,
                                         int callee_params,
                                         int callee_locals,
                                         bool is_top_frame) {
  // Note: This calculation must exactly parallel the frame setup
  // in AbstractInterpreterGenerator::generate_method_entry.

  // fixed size of an interpreter frame:
  int overhead = frame::sender_sp_offset -
                 frame::interpreter_frame_initial_sp_offset;
  // Our locals were accounted for by the caller (or last_frame_adjust
  // on the transistion) Since the callee parameters already account
  // for the callee's params we only need to account for the extra
  // locals.
  int size = overhead +
         (callee_locals - callee_params)*Interpreter::stackElementWords +
         monitors * frame::interpreter_frame_monitor_size() +
         temps* Interpreter::stackElementWords + extra_args;

  // On AArch64 we always keep the stack pointer 16-aligned, so we
  // must round up here.
  size = round_to(size, 2);

  return size;
}

void AbstractInterpreter::layout_activation(Method* method,
                                            int tempcount,
                                            int popframe_extra_args,
                                            int moncount,
                                            int caller_actual_parameters,
                                            int callee_param_count,
                                            int callee_locals,
                                            frame* caller,
                                            frame* interpreter_frame,
                                            bool is_top_frame,
                                            bool is_bottom_frame) {
  // The frame interpreter_frame is guaranteed to be the right size,
  // as determined by a previous call to the size_activation() method.
  // It is also guaranteed to be walkable even though it is in a
  // skeletal state

  int max_locals = method->max_locals() * Interpreter::stackElementWords;
  int extra_locals = (method->max_locals() - method->size_of_parameters()) *
    Interpreter::stackElementWords;

#ifdef ASSERT
  assert(caller->sp() == interpreter_frame->sender_sp(), "Frame not properly walkable");
#endif

  interpreter_frame->interpreter_frame_set_method(method);
  // NOTE the difference in using sender_sp and
  // interpreter_frame_sender_sp interpreter_frame_sender_sp is
  // the original sp of the caller (the unextended_sp) and
  // sender_sp is fp+8/16 (32bit/64bit) XXX
  intptr_t* locals = interpreter_frame->sender_sp() + max_locals - 1;

#ifdef ASSERT
  if (caller->is_interpreted_frame()) {
    assert(locals < caller->fp() + frame::interpreter_frame_initial_sp_offset, "bad placement");
  }
#endif

  interpreter_frame->interpreter_frame_set_locals(locals);
  BasicObjectLock* montop = interpreter_frame->interpreter_frame_monitor_begin();
  BasicObjectLock* monbot = montop - moncount;
  interpreter_frame->interpreter_frame_set_monitor_end(monbot);

  // Set last_sp
  intptr_t*  last_sp = (intptr_t*) monbot -
    tempcount*Interpreter::stackElementWords -
    popframe_extra_args;
  interpreter_frame->interpreter_frame_set_last_sp(last_sp);

  // All frames but the initial (oldest) interpreter frame we fill in have
  // a value for sender_sp that allows walking the stack but isn't
  // truly correct. Correct the value here.
  if (extra_locals != 0 &&
      interpreter_frame->sender_sp() ==
      interpreter_frame->interpreter_frame_sender_sp()) {
    interpreter_frame->set_interpreter_frame_sender_sp(caller->sp() +
                                                       extra_locals);
  }
  *interpreter_frame->interpreter_frame_cache_addr() =
    method->constants()->cache();
}


//-----------------------------------------------------------------------------
// Exceptions

void TemplateInterpreterGenerator::generate_throw_exception() {
  // Entry point in previous activation (i.e., if the caller was
  // interpreted)
  Interpreter::_rethrow_exception_entry = __ pc();
  __ reg_printf("rethrow_exception_entry\n");

  // Restore sp to interpreter_frame_last_sp even though we are going
  // to empty the expression stack for the exception processing.
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  // r0: exception
  // r3: return address/pc that threw exception
  __ restore_bcp();    // rbcp points to call/send
  __ restore_locals();
  __ restore_constant_pool_cache();
  __ get_dispatch();

  // Entry point for exceptions thrown within interpreter code
  Interpreter::_throw_exception_entry = __ pc();
  __ reg_printf("throw_exception_entry\n");
  // If we came here via a NullPointerException on the receiver of a
  // method, rmethod may be corrupt.
  __ get_method(rmethod);
  // expression stack is undefined here
  // r0: exception
  // rbcp: exception bcp
  __ verify_oop(r0);
  __ mov(c_rarg1, r0);

  // expression stack must be empty before entering the VM in case of
  // an exception
  __ empty_expression_stack();
  // find exception handler address and preserve exception oop
  __ call_VM(r3,
             CAST_FROM_FN_PTR(address,
                          InterpreterRuntime::exception_handler_for_exception),
             c_rarg1);

  // Calculate stack limit
  /*__ ldr(rscratch1, Address(rmethod, Method::const_offset()));
  __ ldrh(rscratch1, Address(rscratch1, ConstMethod::max_stack_offset()));
  __ add(rscratch1, rscratch1, frame::interpreter_frame_monitor_size() + 4);
  __ ldr(rscratch2,
         Address(rfp, frame::interpreter_frame_initial_sp_offset * wordSize));
  __ sub(rscratch1, rscratch2, rscratch1, lsl(2));
  __ bic(sp, rscratch1, 0xf);*/
  // Don't do this as we don't have a stack pointer

  // r0: exception handler entry point
  // r3: preserved exception oop
  // rbcp: bcp for exception handler
  __ push_ptr(r3); // push exception which is now the only value on the stack
  __ b(r0); // jump to exception handler (may be _remove_activation_entry!)

  // If the exception is not handled in the current frame the frame is
  // removed and the exception is rethrown (i.e. exception
  // continuation is _rethrow_exception).
  //
  // Note: At this point the bci is still the bxi for the instruction
  // which caused the exception and the expression stack is
  // empty. Thus, for any VM calls at this point, GC will find a legal
  // oop map (with empty expression stack).

  //
  // JVMTI PopFrame support
  //

  Interpreter::_remove_activation_preserving_args_entry = __ pc();
  __ print_method_exit(false);
  __ reg_printf("remove_activation_preserving_args_entry\n");
  __ empty_expression_stack();
  // Set the popframe_processing bit in pending_popframe_condition
  // indicating that we are currently handling popframe, so that
  // call_VMs that may happen later do not trigger new popframe
  // handling cycles.
  __ ldr(r3, Address(rthread, JavaThread::popframe_condition_offset()));
  __ orr(r3, r3, JavaThread::popframe_processing_bit);
  __ str(r3, Address(rthread, JavaThread::popframe_condition_offset()));

  {
    // Check to see whether we are returning to a deoptimized frame.
    // (The PopFrame call ensures that the caller of the popped frame is
    // either interpreted or compiled and deoptimizes it if compiled.)
    // In this case, we can't call dispatch_next() after the frame is
    // popped, but instead must save the incoming arguments and restore
    // them after deoptimization has occurred.
    //
    // Note that we don't compare the return PC against the
    // deoptimization blob's unpack entry because of the presence of
    // adapter frames in C2.
    Label caller_not_deoptimized;
    __ ldr(c_rarg1, Address(rfp, frame::return_addr_offset * wordSize));
    __ super_call_VM_leaf(CAST_FROM_FN_PTR(address,
                               InterpreterRuntime::interpreter_contains), c_rarg1);
    __ cbnz(r0, caller_not_deoptimized);

    // Compute size of arguments for saving when returning to
    // deoptimized caller
    __ get_method(r0);
    __ ldr(r0, Address(r0, Method::const_offset()));
    __ load_unsigned_short(r0, Address(r0, in_bytes(ConstMethod::
                                                    size_of_parameters_offset())));
    __ lsl(r0, r0, Interpreter::logStackElementSize);
    __ restore_locals(); // XXX do we need this?
    __ sub(rlocals, rlocals, r0);
    __ add(rlocals, rlocals, wordSize);
    // Save these arguments
    __ super_call_VM_leaf(CAST_FROM_FN_PTR(address,
                                           Deoptimization::
                                           popframe_preserve_args),
                          rthread, r0, rlocals);

    __ remove_activation(vtos,
                         /* throw_monitor_exception */ false,
                         /* install_monitor_exception */ false,
                         /* notify_jvmdi */ false);

    // Inform deoptimization that it is responsible for restoring
    // these arguments
    __ mov(rscratch1, JavaThread::popframe_force_deopt_reexecution_bit);
    __ str(rscratch1, Address(rthread, JavaThread::popframe_condition_offset()));

    // Continue in deoptimization handler
    __ b(lr);

    __ bind(caller_not_deoptimized);
  }

  __ remove_activation(vtos,
                       /* throw_monitor_exception */ false,
                       /* install_monitor_exception */ false,
                       /* notify_jvmdi */ false);

  // Restore the last_sp and null it out
  __ ldr(sp, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  __ mov(rscratch1, 0);
  __ str(rscratch1, Address(rfp, frame::interpreter_frame_last_sp_offset * wordSize));
  // remove_activation restores sp?

  __ restore_bcp();
  __ restore_locals();
  __ restore_constant_pool_cache();
  __ get_method(rmethod);
  __ get_dispatch();

  // The method data pointer was incremented already during
  // call profiling. We have to restore the mdp for the current bcp.
  if (ProfileInterpreter) {
    __ set_method_data_pointer_for_bcp();
  }

  // Clear the popframe condition flag
  __ mov(rscratch1, JavaThread::popframe_inactive);
  __ str(rscratch1, Address(rthread, JavaThread::popframe_condition_offset()));
  assert(JavaThread::popframe_inactive == 0, "fix popframe_inactive");

#if INCLUDE_JVMTI
  {
    Label L_done;
    __ ldrb(rscratch1, Address(rbcp, 0));
    __ cmp(rscratch1, Bytecodes::_invokestatic);
    __ b(L_done, Assembler::NE);

    // The member name argument must be restored if _invokestatic is re-executed after a PopFrame call.
    // Detect such a case in the InterpreterRuntime function and return the member name argument, or NULL.

    __ ldr(c_rarg0, Address(rlocals, 0));
    __ call_VM(r0, CAST_FROM_FN_PTR(address, InterpreterRuntime::member_name_arg_or_null), c_rarg0, rmethod, rbcp);

    __ cbz(r0, L_done);

    __ str(r0, Address(sp, 0));
    __ bind(L_done);
  }
#endif // INCLUDE_JVMTI

  // Restore machine SP
  /*__ ldr(rscratch1, Address(rmethod, Method::const_offset()));
  __ ldrh(rscratch1, Address(rscratch1, ConstMethod::max_stack_offset()));
  __ add(rscratch1, rscratch1, frame::interpreter_frame_monitor_size() + 4);
  __ ldr(rscratch2,
         Address(rfp, frame::interpreter_frame_initial_sp_offset * wordSize));
  __ sub(rscratch1, rscratch2, rscratch1, lsl(2));
  __ bic(sp, rscratch1, 0xf);*/

  __ dispatch_next(vtos);
  // end of PopFrame support

  Interpreter::_remove_activation_entry = __ pc();
  __ print_method_exit(false);
  __ reg_printf("remove_activation_entry\n");

  // preserve exception over this code sequence
  __ pop_ptr(r0);
  __ str(r0, Address(rthread, JavaThread::vm_result_offset()));
  // remove the activation (without doing throws on illegalMonitorExceptions)
  __ remove_activation(vtos, false, true, false);
  // restore exception
  // restore exception
  __ get_vm_result(r0, rthread);

  // In between activations - previous activation type unknown yet
  // compute continuation point - the continuation point expects the
  // following registers set up:
  //
  // r0: exception
  // lr: return address/pc that threw exception
  // rsp: expression stack of caller
  // rfp: fp of caller
  // FIXME: There's no point saving LR here because VM calls don't trash it
  __ strd(r0, lr, Address(__ pre(sp, -2 * wordSize)));  // save exception & return address
  __ super_call_VM_leaf(CAST_FROM_FN_PTR(address,
                          SharedRuntime::exception_handler_for_return_address),
                        rthread, lr);
  __ mov(r1, r0);                               // save exception handler
  __ ldrd(r0, lr, Address(__ post(sp, 2 * wordSize)));  // restore exception & return address
  // We might be returning to a deopt handler that expects r3 to
  // contain the exception pc
  __ mov(r3, lr);
  // Note that an "issuing PC" is actually the next PC after the call
  __ b(r1);                                    // jump to exception
                                                // handler of caller
}


//
// JVMTI ForceEarlyReturn support
//
address TemplateInterpreterGenerator::generate_earlyret_entry_for(TosState state) {
  address entry = __ pc();
  __ restore_bcp();
  __ restore_locals();
  __ empty_expression_stack();
  __ load_earlyret_value(state);

  __ ldr(rscratch1, Address(rthread, JavaThread::jvmti_thread_state_offset()));
  Address cond_addr(rscratch1, JvmtiThreadState::earlyret_state_offset());

  // Clear the earlyret state
  assert(JvmtiThreadState::earlyret_inactive == 0, "should be");
  __ mov(rscratch2, 0);
  __ str(rscratch2, cond_addr);

  __ remove_activation(state,
                       false, /* throw_monitor_exception */
                       false, /* install_monitor_exception */
                       true); /* notify_jvmdi */
  __ b(lr);

  return entry;
} // end of ForceEarlyReturn support



//-----------------------------------------------------------------------------
// Helper for vtos entry point generation

void TemplateInterpreterGenerator::set_vtos_entry_points(Template* t,
                                                         address& bep,
                                                         address& cep,
                                                         address& sep,
                                                         address& aep,
                                                         address& iep,
                                                         address& lep,
                                                         address& fep,
                                                         address& dep,
                                                         address& vep) {
  assert(t->is_valid() && t->tos_in() == vtos, "illegal template");
  Label L;
  aep = __ pc();  __ push_ptr();  __ b(L);
  dep = __ pc();
  if(hasFPU()){
    __ push_d(); __ b(L);
  }
  lep = __ pc();  __ push_l();    __ b(L);
  fep = __ pc();
  if(hasFPU()){
    __ push_f();    __ b(L);
  }
  bep = cep = sep =
  iep = __ pc();  __ push_i();
  vep = __ pc();
  __ bind(L);
  generate_and_dispatch(t);
}

//-----------------------------------------------------------------------------
// Generation of individual instructions

// helpers for generate_and_dispatch


InterpreterGenerator::InterpreterGenerator(StubQueue* code)
  : TemplateInterpreterGenerator(code) {
   generate_all(); // down here so it can be "virtual"
}

//-----------------------------------------------------------------------------

// Non-product code
#ifndef PRODUCT
address TemplateInterpreterGenerator::generate_trace_code(TosState state) {
  address entry = __ pc();

  __ push(state);
  // Save all registers on stack, so omit SP and PC
  const RegSet push_set = RegSet::range(r0, r12) + lr;
  const int push_set_cnt = __builtin_popcount(push_set.bits());
  __ push(push_set, sp);
  __ ldr(c_rarg2, Address(sp, push_set_cnt*wordSize));      // Pass top of stack
  __ ldr(c_rarg3, Address(sp, (push_set_cnt+1)*wordSize));  // Pass top of stack high part/2nd stack word
  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address, SharedRuntime::trace_bytecode),
             c_rarg1, c_rarg2, c_rarg3);
  __ pop(RegSet::range(r0, r12) + lr, sp);
  __ pop(state);
  __ b(lr);                                   // return from result handler

  return entry;
}

void TemplateInterpreterGenerator::count_bytecode() {
  __ push(c_rarg0);
  __ push(rscratch1);
  __ push(rscratch2);
  Label L;
  __ mov(rscratch2, (address) &BytecodeCounter::_counter_value);
  __ bind(L);
  __ ldrex(rscratch1, rscratch2);
  __ add(rscratch1, rscratch1, 1);
  // strex stores 2nd arg to dest adressed by 3rd arg,
  // stores status to 1st arg. So, 1st and 2nd shoud be different.
  __ strex(c_rarg0, rscratch1, rscratch2);
  __ cmp(c_rarg0, 0);
  __ b(L, Assembler::NE);
  __ pop(rscratch2);
  __ pop(rscratch1);
  __ pop(c_rarg0);
}

void TemplateInterpreterGenerator::histogram_bytecode(Template* t) { ; }

void TemplateInterpreterGenerator::histogram_bytecode_pair(Template* t) { ; }


void TemplateInterpreterGenerator::trace_bytecode(Template* t) {
  // Call a little run-time stub to avoid blow-up for each bytecode.
  // The run-time runtime saves the right registers, depending on
  // the tosca in-state for the given template.

  assert(Interpreter::trace_code(t->tos_in()) != NULL,
         "entry must have been generated");
  __ bl(Interpreter::trace_code(t->tos_in()));
}


void TemplateInterpreterGenerator::stop_interpreter_at() {
  Label L;
  __ push(rscratch1);
  __ mov(rscratch1, (address) &BytecodeCounter::_counter_value);
  __ ldr(rscratch1, Address(rscratch1));
  __ mov(rscratch2, StopInterpreterAt);
  __ cmp(rscratch1, rscratch2);
  __ b(L, Assembler::NE);
  __ bkpt(0);
  __ bind(L);
  __ pop(rscratch1);
}

#endif // !PRODUCT
#endif // ! CC_INTERP
