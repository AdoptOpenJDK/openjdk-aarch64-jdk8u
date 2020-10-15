/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "asm/macroAssembler.inline.hpp"
#include "runtime/os.hpp"
#include "runtime/threadLocalStorage.hpp"


// get_thread can be called anywhere inside generated code so we need
// to save whatever non-callee save context might get clobbered by the
// call to the C thread_local lookup call or, indeed, the call setup
// code. x86 appears to save C arg registers.

void MacroAssembler::get_thread(Register dst) {
  // call pthread_getspecific
  // void * pthread_getspecific(pthread_key_t key);

  // Save all call-clobbered regs except dst, plus rscratch1 and rscratch2.
  RegSet saved_regs = RegSet::range(r0, r3) + rscratch1 + rscratch2 + lr - dst;
  push(saved_regs, sp);

  // Align stack and save value for return
  mov(c_rarg1, sp);
  sub(sp, sp, wordSize);
  bic(sp, sp, 7);
  str(c_rarg1, Address(sp));

  mov(c_rarg0, ThreadLocalStorage::thread_index());
  mov(r14, CAST_FROM_FN_PTR(address, pthread_getspecific));

  bl(r14);
  //undo alignment
  ldr(sp, Address(sp));

  if (dst != c_rarg0) {
    mov(dst, c_rarg0);
  }

  // restore pushed registers
  pop(saved_regs, sp);
}

