/*
 * Copyright (c) 2004, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/resourceArea.hpp"
#include "prims/jniFastGetField.hpp"
#include "prims/jvm_misc.hpp"
#include "runtime/safepoint.hpp"

#define __ masm->

#define BUFFER_SIZE_ARMV7 31*wordSize
#define BUFFER_SIZE_ARMV6 51*wordSize

// Instead of issuing a LoadLoad barrier we create an address
// dependency between loads; this might be more efficient.

// Common register usage:
// r0/v0:      result
// c_rarg0:    jni env
// c_rarg1:    obj
// c_rarg2:    jfield id

address JNI_FastGetField::generate_fast_get_int_field0(BasicType type) {
  Register result   = c_rarg0;
  Register robj     = c_rarg1;
  Register rcounter = c_rarg3;
  int      args     = RegSet::of(c_rarg0, c_rarg1, c_rarg2).bits();
  int      nargs    = 3;

  const char *name;
  switch (type) {
    case T_BOOLEAN: name = "jni_fast_GetBooleanField"; break;
    case T_BYTE:    name = "jni_fast_GetByteField";    break;
    case T_CHAR:    name = "jni_fast_GetCharField";    break;
    case T_SHORT:   name = "jni_fast_GetShortField";   break;
    case T_INT:     name = "jni_fast_GetIntField";     break;
    case T_LONG:    name = "jni_fast_GetLongField";    break;
    case T_FLOAT:   name = "jni_fast_GetFloatField";   break;
    case T_DOUBLE:  name = "jni_fast_GetDoubleField";  break;
    default:        ShouldNotReachHere();
  }
  ResourceMark rm;
  BufferBlob* blob = BufferBlob::create(name,
                                        VM_Version::features() & FT_ARMV7 ?
                                         BUFFER_SIZE_ARMV7 :
                                         BUFFER_SIZE_ARMV6 );
  CodeBuffer cbuf(blob);
  MacroAssembler* masm = new MacroAssembler(&cbuf);
  address fast_entry = __ pc();

  Label slow;

  __ lea(rcounter, SafepointSynchronize::safepoint_counter_addr());
  __ ldr(rcounter, rcounter);
  __ tst(rcounter, 1);
  __ b(slow, Assembler::NE);
  __ stmdb(sp, args);
  // doesn't change c_rarg1 but does force a dependency on rcounter before
  // performing __ ldr(robj, ...
  __ eor(robj, c_rarg1, rcounter);
  __ eor(robj, robj, rcounter);

  __ clear_jweak_tag(robj);

  __ ldr(robj, Address(robj, 0)); // *obj

  assert(count < LIST_CAPACITY, "LIST_CAPACITY too small");
  speculative_load_pclist[count] = __ pc();   // Used by the segfault handler
  // c_rarg2 * 2 is offset
  // Only ldr & ldrb support shifted loads
  switch (type) {
    case T_FLOAT:
    case T_INT:     __ ldr (result, Address(robj, c_rarg2, lsr(2))); break;
    case T_BOOLEAN: __ ldrb(result, Address(robj, c_rarg2, lsr(2))); break;
    default: {
      __ lsr(c_rarg2, c_rarg2, 2);
      switch(type) {
        case T_BYTE:    __ ldrsb   (result, Address(robj, c_rarg2)); break;
        case T_CHAR:    __ ldrh    (result, Address(robj, c_rarg2)); break;
        case T_SHORT:   __ ldrsh   (result, Address(robj, c_rarg2)); break;
        case T_DOUBLE:
        case T_LONG:    __ ldrd    (result, Address(robj, c_rarg2)); break;
        default:        ShouldNotReachHere();
      }
    }
  }
  __ lea(rscratch2, SafepointSynchronize::safepoint_counter_addr());
  // rscratch2 is address dependent on result.
  // TODO Do we need to force dependency on r1 too?
  __ eor(rscratch2, rscratch2, result);
  __ eor(rscratch2, rscratch2, result);
  __ ldr(rscratch2, rscratch2);
  __ cmp(rcounter, rscratch2);

#ifdef HARD_FLOAT_CC
  switch (type) {
    case T_FLOAT:   __ vmov_f32(d0, result, Assembler::EQ); break;
    case T_DOUBLE:  __ vmov_f64(d0, r0, r1, Assembler::EQ); break; // Change me if result changes
    default:                                                break;
  }
#endif//HARD_FLOAT_CC

  __ add(sp, sp, nargs * wordSize, Assembler::EQ); // Pop args if we don't need them.
  __ b(lr, Assembler::EQ);

  // Restore args for slowcase call into the vm
  __ ldmia(sp, args);

  // Slowcase
  slowcase_entry_pclist[count++] = __ pc();
  __ bind(slow);

  address slow_case_addr = NULL;
  switch (type) {
    case T_BOOLEAN: slow_case_addr = jni_GetBooleanField_addr(); break;
    case T_BYTE:    slow_case_addr = jni_GetByteField_addr();    break;
    case T_CHAR:    slow_case_addr = jni_GetCharField_addr();    break;
    case T_SHORT:   slow_case_addr = jni_GetShortField_addr();   break;
    case T_INT:     slow_case_addr = jni_GetIntField_addr();     break;
    case T_LONG:    slow_case_addr = jni_GetLongField_addr();    break;
    case T_FLOAT:   slow_case_addr = jni_GetFloatField_addr();   break;
    case T_DOUBLE:  slow_case_addr = jni_GetDoubleField_addr();  break;
    default:        ShouldNotReachHere();
  }

  {
    __ enter();
    __ lea(rscratch2, ExternalAddress(slow_case_addr));
    __ bl(rscratch2);
    __ maybe_isb();
    __ leave();
    __ b(lr);
  }
  __ flush ();

  return fast_entry;
}

address JNI_FastGetField::generate_fast_get_boolean_field() {
  return generate_fast_get_int_field0(T_BOOLEAN);
}

address JNI_FastGetField::generate_fast_get_byte_field() {
  return generate_fast_get_int_field0(T_BYTE);
}

address JNI_FastGetField::generate_fast_get_char_field() {
  return generate_fast_get_int_field0(T_CHAR);
}

address JNI_FastGetField::generate_fast_get_short_field() {
  return generate_fast_get_int_field0(T_SHORT);
}

address JNI_FastGetField::generate_fast_get_int_field() {
  return generate_fast_get_int_field0(T_INT);
}

address JNI_FastGetField::generate_fast_get_long_field() {
  return generate_fast_get_int_field0(T_LONG);
}

address JNI_FastGetField::generate_fast_get_float_field() {
  return generate_fast_get_int_field0(T_FLOAT);
}

address JNI_FastGetField::generate_fast_get_double_field() {
  return generate_fast_get_int_field0(T_DOUBLE);
}

