/*
 * Copyright (c) 2002, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "asm/assembler.hpp"
#include "asm/register.hpp"

REGISTER_DEFINITION(Register, noreg);

REGISTER_DEFINITION(Register, r0);
REGISTER_DEFINITION(Register, r1);
REGISTER_DEFINITION(Register, r2);
REGISTER_DEFINITION(Register, r3);
REGISTER_DEFINITION(Register, r4);
REGISTER_DEFINITION(Register, r5);
REGISTER_DEFINITION(Register, r6);
REGISTER_DEFINITION(Register, r7);
REGISTER_DEFINITION(Register, r8);
REGISTER_DEFINITION(Register, r9);
REGISTER_DEFINITION(Register, r10);
REGISTER_DEFINITION(Register, r11);
REGISTER_DEFINITION(Register, r12);
REGISTER_DEFINITION(Register, r13);
REGISTER_DEFINITION(Register, r14);
REGISTER_DEFINITION(Register, r15);

REGISTER_DEFINITION(FloatRegister, fnoreg);

REGISTER_DEFINITION(FloatRegister, d0);
REGISTER_DEFINITION(FloatRegister, d1);
REGISTER_DEFINITION(FloatRegister, d2);
REGISTER_DEFINITION(FloatRegister, d3);
REGISTER_DEFINITION(FloatRegister, d4);
REGISTER_DEFINITION(FloatRegister, d5);
REGISTER_DEFINITION(FloatRegister, d6);
REGISTER_DEFINITION(FloatRegister, d7);
REGISTER_DEFINITION(FloatRegister, d8);
REGISTER_DEFINITION(FloatRegister, d9);
REGISTER_DEFINITION(FloatRegister, d10);
REGISTER_DEFINITION(FloatRegister, d11);
REGISTER_DEFINITION(FloatRegister, d12);
REGISTER_DEFINITION(FloatRegister, d13);
REGISTER_DEFINITION(FloatRegister, d14);
REGISTER_DEFINITION(FloatRegister, d15);
REGISTER_DEFINITION(FloatRegister, d16);
REGISTER_DEFINITION(FloatRegister, d17);
REGISTER_DEFINITION(FloatRegister, d18);
REGISTER_DEFINITION(FloatRegister, d19);
REGISTER_DEFINITION(FloatRegister, d20);
REGISTER_DEFINITION(FloatRegister, d21);
REGISTER_DEFINITION(FloatRegister, d22);
REGISTER_DEFINITION(FloatRegister, d23);
REGISTER_DEFINITION(FloatRegister, d24);
REGISTER_DEFINITION(FloatRegister, d25);
REGISTER_DEFINITION(FloatRegister, d26);
REGISTER_DEFINITION(FloatRegister, d27);
REGISTER_DEFINITION(FloatRegister, d28);
REGISTER_DEFINITION(FloatRegister, d29);
REGISTER_DEFINITION(FloatRegister, d30);
REGISTER_DEFINITION(FloatRegister, d31);

REGISTER_DEFINITION(FloatRegister, q0);
REGISTER_DEFINITION(FloatRegister, q1);
REGISTER_DEFINITION(FloatRegister, q2);
REGISTER_DEFINITION(FloatRegister, q3);
REGISTER_DEFINITION(FloatRegister, q4);
REGISTER_DEFINITION(FloatRegister, q5);
REGISTER_DEFINITION(FloatRegister, q6);
REGISTER_DEFINITION(FloatRegister, q7);
REGISTER_DEFINITION(FloatRegister, q8);
REGISTER_DEFINITION(FloatRegister, q9);
REGISTER_DEFINITION(FloatRegister, q10);
REGISTER_DEFINITION(FloatRegister, q11);
REGISTER_DEFINITION(FloatRegister, q12);
REGISTER_DEFINITION(FloatRegister, q13);
REGISTER_DEFINITION(FloatRegister, q14);
REGISTER_DEFINITION(FloatRegister, q15);

REGISTER_DEFINITION(FloatRegister, f0);
REGISTER_DEFINITION(FloatRegister, f1);
REGISTER_DEFINITION(FloatRegister, f2);
REGISTER_DEFINITION(FloatRegister, f3);
REGISTER_DEFINITION(FloatRegister, f4);
REGISTER_DEFINITION(FloatRegister, f5);
REGISTER_DEFINITION(FloatRegister, f6);
REGISTER_DEFINITION(FloatRegister, f7);
REGISTER_DEFINITION(FloatRegister, f8);
REGISTER_DEFINITION(FloatRegister, f9);
REGISTER_DEFINITION(FloatRegister, f10);
REGISTER_DEFINITION(FloatRegister, f11);
REGISTER_DEFINITION(FloatRegister, f12);
REGISTER_DEFINITION(FloatRegister, f13);
REGISTER_DEFINITION(FloatRegister, f14);
REGISTER_DEFINITION(FloatRegister, f15);

REGISTER_DEFINITION(Register, c_rarg0);
REGISTER_DEFINITION(Register, c_rarg1);
REGISTER_DEFINITION(Register, c_rarg2);
REGISTER_DEFINITION(Register, c_rarg3);

REGISTER_DEFINITION(Register, j_rarg0);
REGISTER_DEFINITION(Register, j_rarg1);
REGISTER_DEFINITION(Register, j_rarg2);
REGISTER_DEFINITION(Register, j_rarg3);

REGISTER_DEFINITION(Register, rdispatch);
REGISTER_DEFINITION(Register, rbcp);
REGISTER_DEFINITION(Register, rlocals);
REGISTER_DEFINITION(Register, rcpool);
REGISTER_DEFINITION(Register, rthread);
REGISTER_DEFINITION(Register, rscratch1);
REGISTER_DEFINITION(Register, rmethod);
REGISTER_DEFINITION(Register, rfp);
REGISTER_DEFINITION(Register, rscratch2);
REGISTER_DEFINITION(Register, sp);
REGISTER_DEFINITION(Register, lr);
REGISTER_DEFINITION(Register, r15_pc);
