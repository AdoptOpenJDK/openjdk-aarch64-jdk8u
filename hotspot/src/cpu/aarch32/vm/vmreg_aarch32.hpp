/*
 * Copyright (c) 2006, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH32_VM_VMREG_AARCH32_HPP
#define CPU_AARCH32_VM_VMREG_AARCH32_HPP

  bool is_Register() {
    // BAD_REG should not pass this test.
    return (unsigned int) value() <
           (unsigned int) ConcreteRegisterImpl::max_gpr;
  }

  bool is_FloatRegister() {
    return value() >= ConcreteRegisterImpl::max_gpr &&
           value() < ConcreteRegisterImpl::max_fpr;
  }

  Register as_Register() {
    assert(is_Register(), "sanity check");
    return ::as_Register(value());
  }

  FloatRegister as_FloatRegister() {
    assert(is_FloatRegister(), "sanity check");
    return ::as_FloatRegister(value() - ConcreteRegisterImpl::max_gpr);
  }

#endif // CPU_AARCH32_VM_VMREG_AARCH32_HPP
