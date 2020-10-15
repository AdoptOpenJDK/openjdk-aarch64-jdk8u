/*
 * Copyright (c) 2005, 2010, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, Red Hat Inc. All rights reserved.
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
// This file is a derivative work resulting from (and including) modifications
// made by Azul Systems, Inc.  The dates of such changes are 2013-2016.
// Copyright 2013-2016 Azul Systems, Inc.  All Rights Reserved.
//
// Please contact Azul Systems, 385 Moffett Park Drive, Suite 115, Sunnyvale,
// CA 94089 USA or visit www.azul.com if you need additional information or
// have any questions.

#ifndef CPU_AARCH32_VM_C1_LINEARSCAN_AARCH32_HPP
#define CPU_AARCH32_VM_C1_LINEARSCAN_AARCH32_HPP

inline bool LinearScan::is_processed_reg_num(int reg_num) {
  return reg_num <= pd_last_cpu_reg || reg_num >= pd_nof_cpu_regs_frame_map;
}

inline int LinearScan::num_physical_regs(BasicType type) {
  if (type == T_LONG || type == T_DOUBLE) {
    return 2;
  }
  return 1;
}

inline bool LinearScan::requires_adjacent_regs(BasicType type) {
  if (type == T_DOUBLE) {
    return true;
  }
  return false;
}

inline bool LinearScan::is_caller_save(int assigned_reg) {
  assert(assigned_reg >= 0 && assigned_reg < nof_regs,
         "should call this only for registers");
  // TODO: Remove the following line when support for callee-saved registers
  // is added
  return true;
  if (assigned_reg < pd_first_callee_saved_cpu_reg) {
    return true;
  }
  if (assigned_reg > pd_last_callee_saved_cpu_reg &&
      assigned_reg < pd_first_callee_saved_fpu_reg) {
    return true;
  }
  if (assigned_reg > pd_last_callee_saved_fpu_reg &&
      assigned_reg <= pd_last_fpu_reg) {
    return true;
  }
  return false;
}

// If there are special cases when some particular LIR operations kill some
// specific registers, this behavior should be described here. An example
// can be found in x86 port.
inline void LinearScan::pd_add_temps(LIR_Op* op) {
  if (op->code() == lir_move) {
    LIR_Op1* move_op = op->as_Op1();
    if (move_op->move_kind() == lir_move_volatile) {
      bool is_long = move_op->type() == T_LONG;
      bool is_double = move_op->type() == T_DOUBLE;
      bool is_store = move_op->in_opr()->is_register();
      if (is_double) {
        add_temp(reg_num(FrameMap::long0_opr), op->id(), noUse, T_ILLEGAL);
        add_temp(reg_numHi(FrameMap::long0_opr), op->id(), noUse, T_ILLEGAL);
      }
      if (is_store && (is_long || is_double)) {
        add_temp(reg_num(FrameMap::long1_opr), op->id(), noUse, T_ILLEGAL);
        add_temp(reg_numHi(FrameMap::long1_opr), op->id(), noUse, T_ILLEGAL);
      }
    }
  }
}

inline bool LinearScanWalker::pd_init_regs_for_alloc(Interval* cur) {
#ifndef HARD_FLOAT_CC
    BasicType type = cur->type();
    if(!hasFPU()) {
        if (type == T_FLOAT || type == T_DOUBLE) {
            _first_reg = pd_first_cpu_reg;
            _last_reg = FrameMap::last_cpu_reg();;
            return true;
        }
    }
#endif
  return false;
}

#endif // CPU_AARCH32_VM_C1_LINEARSCAN_AARCH32_HPP
