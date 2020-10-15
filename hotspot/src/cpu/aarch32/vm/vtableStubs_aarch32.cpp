/*
 * Copyright (c) 2003, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "assembler_aarch32.inline.hpp"
#include "code/vtableStubs.hpp"
#include "interp_masm_aarch32.hpp"
#include "memory/resourceArea.hpp"
#include "oops/compiledICHolder.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klassVtable.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch32.inline.hpp"
#ifdef COMPILER2
#include "opto/runtime.hpp"
#endif

// machine-dependent part of VtableStubs: create VtableStub of correct size and
// initialize its code

#define __ masm->

#ifndef PRODUCT
extern "C" void bad_compiled_vtable_index(JavaThread* thread,
                                          oop receiver,
                                          int index);
#endif

VtableStub* VtableStubs::create_vtable_stub(int vtable_index) {
  const int aarch32_code_length = VtableStub::pd_code_size_limit(true);
  VtableStub* s = new(aarch32_code_length) VtableStub(true, vtable_index);
  // Can be NULL if there is no free space in the code cache.
  if (s == NULL) {
    return NULL;
  }

  ResourceMark rm;
  CodeBuffer cb(s->entry_point(), aarch32_code_length);
  MacroAssembler* masm = new MacroAssembler(&cb);

#ifndef PRODUCT
  if (CountCompiledCalls) {
    // FIXME SharedRuntime::nof_megamorphic_calls_addr() returns un-encodable address
    __ increment(ExternalAddress((address) SharedRuntime::nof_megamorphic_calls_addr()), 1);
  }
#endif

  // get receiver (need to skip return address on top of stack)
  assert(VtableStub::receiver_location() == j_rarg0->as_VMReg(), "receiver expected in j_rarg0");

  // get receiver klass
  address npe_addr = __ pc();
  __ load_klass(rscratch2, j_rarg0);

#ifndef PRODUCT
  if (DebugVtables) {
    Label L;
    // check offset vs vtable length
    __ ldr(rscratch1, Address(rscratch2, InstanceKlass::vtable_length_offset() * wordSize));
    __ cmp(rscratch1, vtable_index * vtableEntry::size());
    __ b(L, Assembler::GT);
    __ enter();
    __ mov(r2, vtable_index);
    __ call_VM(noreg,
               CAST_FROM_FN_PTR(address, bad_compiled_vtable_index), j_rarg0, r2);
    __ leave();
    __ bind(L);
  }
#endif // PRODUCT

  __ lookup_virtual_method(rscratch2, vtable_index, rmethod);

  if (DebugVtables) {
    Label L;
    __ cbz(rmethod, L);
    __ ldr(rscratch1, Address(rmethod, Method::from_compiled_offset()));
    __ cbnz(rscratch1, L);
    __ stop("Vtable entry is NULL");
    __ bind(L);
  }
  // r0: receiver klass
  // rmethod: Method*
  // r2: receiver
  address ame_addr = __ pc();
  __ ldr(rscratch1, Address(rmethod, Method::from_compiled_offset()));
  __ b(rscratch1);

  __ flush();

  if (PrintMiscellaneous && (WizardMode || Verbose)) {
    tty->print_cr("vtable #%d at " PTR_FORMAT "[%d] left over: %d",
                  vtable_index, p2i(s->entry_point()),
                  (int)(s->code_end() - s->entry_point()),
                  (int)(s->code_end() - __ pc()));
  }
  guarantee(__ pc() <= s->code_end(), "overflowed buffer");

  s->set_exception_points(npe_addr, ame_addr);
  return s;
}


VtableStub* VtableStubs::create_itable_stub(int itable_index) {
  // Note well: pd_code_size_limit is the absolute minimum we can get
  // away with.  If you add code here, bump the code stub size
  // returned by pd_code_size_limit!
  const int code_length = VtableStub::pd_code_size_limit(false);
  VtableStub* s = new(code_length) VtableStub(false, itable_index);
  // Can be NULL if there is no free space in the code cache.
  if (s == NULL) {
    return NULL;
  }

  ResourceMark rm;
  CodeBuffer cb(s->entry_point(), code_length);
  MacroAssembler* masm = new MacroAssembler(&cb);

#ifndef PRODUCT
  if (CountCompiledCalls) {
    // FIXME SharedRuntime::nof_megamorphic_calls_addr() returns un-encodable address
    __ increment(ExternalAddress((address) SharedRuntime::nof_megamorphic_calls_addr()), 1);
  }
#endif

  // Entry arguments:
  //  rscratch2: CompiledICHolder
  //  j_rarg0: Receiver

  // Free registers (non-args) are r0 (interface), rmethod

  // Most registers are in use; we'll use r0, rmethod, rscratch1, r4
  // IMPORTANT: r4 is used as a temp register, if it's changed callee-save
  // the code should be fixed
  // TODO: put an assert here to ensure r4 is caller-save
  const Register recv_klass_reg     = rscratch1;
  const Register holder_klass_reg   = rscratch2; // declaring interface klass (DECC)
  const Register resolved_klass_reg = rmethod; // resolved interface klass (REFC)
  const Register temp_reg           = r4;

  __ ldr(resolved_klass_reg, Address(rscratch2, CompiledICHolder::holder_klass_offset()));
  __ ldr(holder_klass_reg,   Address(rscratch2, CompiledICHolder::holder_metadata_offset()));

  Label L_no_such_interface;

  // get receiver klass (also an implicit null-check)
  address npe_addr = __ pc();
  assert(VtableStub::receiver_location() == j_rarg0->as_VMReg(), "receiver expected in j_rarg0");
  __ load_klass(recv_klass_reg, j_rarg0);

  // Receiver subtype check against REFC.
  // Destroys recv_klass_reg value.
  __ lookup_interface_method(// inputs: rec. class, interface
                             recv_klass_reg, resolved_klass_reg, noreg,
                             // outputs:  scan temp. reg1, scan temp. reg2
                             recv_klass_reg, temp_reg,
                             L_no_such_interface,
                             /*return_method=*/false);

  // Get selected method from declaring class and itable index
  __ load_klass(recv_klass_reg, j_rarg0); // restore recv_klass_reg
  __ lookup_interface_method(// inputs: rec. class, interface, itable index
                             recv_klass_reg, holder_klass_reg, itable_index,
                             // outputs: method, scan temp. reg
                             rmethod, temp_reg,
                             L_no_such_interface);
  // rmethod: Method*
  // j_rarg0: receiver

#ifdef ASSERT
  if (DebugVtables) {
    Label L2;
    __ cbz(rmethod, L2);
    __ ldr(recv_klass_reg, Address(rmethod, Method::from_compiled_offset()));
    __ cbnz(recv_klass_reg, L2);
    __ stop("compiler entrypoint is null");
    __ bind(L2);
  }
#endif // ASSERT

  address ame_addr = __ pc();
  __ ldr(recv_klass_reg, Address(rmethod, Method::from_compiled_offset()));
  __ b(recv_klass_reg);

  __ bind(L_no_such_interface);
  __ far_jump(RuntimeAddress(StubRoutines::throw_IncompatibleClassChangeError_entry()));

  __ flush();

  if (PrintMiscellaneous && (WizardMode || Verbose)) {
    tty->print_cr("itable #%d at "PTR_FORMAT"[%d] left over: %d",
                  itable_index, p2i(s->entry_point()),
                  (int)(s->code_end() - s->entry_point()),
                  (int)(s->code_end() - __ pc()));
  }
  guarantee(__ pc() <= s->code_end(), "overflowed buffer");

  s->set_exception_points(npe_addr, ame_addr);
  return s;
}


int VtableStub::pd_code_size_limit(bool is_vtable_stub) {
  int size = DebugVtables ? 216 : 0; // FIXME
  if (CountCompiledCalls)
    size += 6 * 4; // FIXME. cannot measure, CountCalls does not work
  if (is_vtable_stub) {
    size += 26;
  } else {
    size += 160;
    if (!(VM_Version::features() & (FT_ARMV7 | FT_ARMV6T2))) {
      size += (NativeMovConstReg::mov_n_three_orr_sz - NativeMovConstReg::movw_movt_pair_sz);
    }
  }
  return size;

  // In order to tune these parameters, run the JVM with VM options
  // +PrintMiscellaneous and +WizardMode to see information about
  // actual itable stubs.  Run it with -Xmx31G -XX:+UseCompressedOops.
  //
  // If Universe::narrow_klass_base is nonzero, decoding a compressed
  // class can take zeveral instructions.  Run it with -Xmx31G
  // -XX:+UseCompressedOops.
  //
  // The JVM98 app. _202_jess has a megamorphic interface call.
}

int VtableStub::pd_code_alignment() { return 4; }
