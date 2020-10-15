/*
 * Copyright (c) 2005, 2011, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "c1/c1_Compilation.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_Instruction.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_LIRGenerator.hpp"
#include "c1/c1_Runtime1.hpp"
#include "c1/c1_ValueStack.hpp"
#include "ci/ciArray.hpp"
#include "ci/ciObjArrayKlass.hpp"
#include "ci/ciTypeArrayKlass.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "vmreg_aarch32.inline.hpp"
#include "vm_version_aarch32.hpp"

#ifdef ASSERT
#define __ gen()->lir(__FILE__, __LINE__)->
#else
#define __ gen()->lir()->
#endif

// Item will be loaded into a byte register; Intel only
void LIRItem::load_byte_item() {
  load_item();
}


void LIRItem::load_nonconstant() {
  LIR_Opr r = value()->operand();
  if (r->is_constant()) {
    _result = r;
  } else {
    load_item();
  }
}

//--------------------------------------------------------------
//               LIRGenerator
//--------------------------------------------------------------


LIR_Opr LIRGenerator::exceptionOopOpr() { return FrameMap::r0_oop_opr; }
LIR_Opr LIRGenerator::exceptionPcOpr()  { return FrameMap::r3_opr; }
LIR_Opr LIRGenerator::divInOpr()        { Unimplemented(); return LIR_OprFact::illegalOpr; }
LIR_Opr LIRGenerator::divOutOpr()       { Unimplemented(); return LIR_OprFact::illegalOpr; }
LIR_Opr LIRGenerator::remOutOpr()       { Unimplemented(); return LIR_OprFact::illegalOpr; }
LIR_Opr LIRGenerator::shiftCountOpr()   { Unimplemented(); return LIR_OprFact::illegalOpr; }
LIR_Opr LIRGenerator::syncTempOpr()     { return FrameMap::r0_opr; }
LIR_Opr LIRGenerator::getThreadTemp()   { return LIR_OprFact::illegalOpr; }


LIR_Opr LIRGenerator::java_result_register_for(ValueType* type, bool callee) {
  LIR_Opr opr;
  switch (type->tag()) {
    case floatTag:
        if(hasFPU()) {
            opr = FrameMap::fpu0_float_opr;  break;;
        }
    case doubleTag:
        if(hasFPU()) {
            opr = FrameMap::fpu0_double_opr;  break;
        }
    default: opr = result_register_for(type, callee);
  }
  return opr;
}
LIR_Opr LIRGenerator::result_register_for(ValueType* type, bool callee) {
  LIR_Opr opr;
  switch (type->tag()) {
    case floatTag:
#ifdef HARD_FLOAT_CC
        opr = FrameMap::fpu0_float_opr;  break;
#endif
    case intTag:     opr = FrameMap::r0_opr;          break;
    case objectTag:  opr = FrameMap::r0_oop_opr;      break;
    case doubleTag:
#ifdef HARD_FLOAT_CC
        opr = FrameMap::fpu0_double_opr;  break;
#endif
    case longTag:    opr = FrameMap::long0_opr;        break;

    case addressTag:
    default: ShouldNotReachHere(); return LIR_OprFact::illegalOpr;
  }
#ifndef HARD_FLOAT_CC
  assert(type->is_float_kind() || opr->type_field() == as_OprType(as_BasicType(type)), "type mismatch");
#else
  assert(opr->type_field() == as_OprType(as_BasicType(type)), "type mismatch");
#endif
  return opr;
}


LIR_Opr LIRGenerator::rlock_byte(BasicType type) {
  LIR_Opr reg = new_register(T_INT);
  set_vreg_flag(reg, LIRGenerator::byte_reg);
  return reg;
}


//--------- loading items into registers --------------------------------


bool LIRGenerator::can_store_as_constant(Value v, BasicType type) const {
  if (v->type()->as_IntConstant() != NULL) {
    return v->type()->as_IntConstant()->value() == 0L;
  } else if (v->type()->as_LongConstant() != NULL) {
    return v->type()->as_LongConstant()->value() == 0L;
  } else if (v->type()->as_ObjectConstant() != NULL) {
    return v->type()->as_ObjectConstant()->value()->is_null_object();
  } else {
    return false;
  }
}

bool LIRGenerator::can_inline_as_constant(Value v) const {
  if (v->type()->as_IntConstant() != NULL) {
    return Assembler::operand_valid_for_add_sub_immediate(v->type()->as_IntConstant()->value());
  } else if (v->type()->as_LongConstant() != NULL) {
    return Assembler::operand_valid_for_add_sub_immediate(v->type()->as_LongConstant()->value());
  } else if (v->type()->as_ObjectConstant() != NULL) {
    return v->type()->as_ObjectConstant()->value()->is_null_object();
  } else {
    return false;
  }
}


bool LIRGenerator::can_inline_as_constant(LIR_Const* c) const {
  switch (c->type()) {
  case T_BOOLEAN:
  case T_CHAR:
  case T_BYTE:
  case T_SHORT:
  case T_INT:
    return Assembler::operand_valid_for_add_sub_immediate(c->as_jint());
  case T_LONG:
    return Assembler::operand_valid_for_add_sub_immediate(c->as_jlong());

  case T_OBJECT:
    return c->as_jobject() == (jobject) NULL;
  case T_METADATA:
    return c->as_metadata() == (Metadata*) NULL;

  case T_FLOAT:
    if( hasFPU()) {
        return Assembler::operand_valid_for_float_immediate(c->as_jfloat());
    } else {
       return Assembler::operand_valid_for_add_sub_immediate(c->as_jint());
    }
  case T_DOUBLE:
    if( hasFPU()) {
        return Assembler::operand_valid_for_float_immediate(c->as_jdouble());
    } else {
        return Assembler::operand_valid_for_add_sub_immediate(c->as_jlong());
    }
  }
  return false;
}

LIR_Opr LIRGenerator::safepoint_poll_register() {
  return LIR_OprFact::illegalOpr;
}

LIR_Address* LIRGenerator::generate_address(LIR_Opr base, LIR_Opr index,
                                            int shift, int disp, BasicType type) {
  const Address::InsnDataType insn_type = Address::toInsnDataType(type);
  assert(base->is_register(), "must be");

  // accumulate fixed displacements
  if (index->is_constant()) {
    disp += index->as_constant_ptr()->as_jint() << shift;
    index = LIR_OprFact::illegalOpr;
    shift = 0;
  }

  // aarch32 cannot handle natively both index and offset at the same time
  // need to calculate effective value
  if (index->is_register()) {
    if ((disp != 0) &&
        Address::shift_ok_for_index(lsl(shift), insn_type) &&
        Assembler::operand_valid_for_add_sub_immediate(disp)) {
      // add tmp, base, disp
      // ldr r, [tmp, index, LSL #shift ]
      LIR_Opr tmp = new_pointer_register();
      __ add(base, LIR_OprFact::intptrConst(disp), tmp);
      base = tmp;
      disp = 0;
    } else {
      assert(shift <= (int) LIR_Address::times_8, "no large shift could be here");
      // add tmp, base, index, LSL #shift
      // ...
      // ldr r, [tmp, ...]
      LIR_Opr tmp = new_pointer_register();
      __ leal(LIR_OprFact::address(new LIR_Address(base, index, (LIR_Address::Scale) shift, 0, type)), tmp);
      base = tmp;
      index = LIR_OprFact::illegalOpr;
      shift = 0;
    }
  }

  assert(!index->is_register() || (disp == 0), "should be");

  if (!Address::offset_ok_for_immed(disp, insn_type)) {
    assert(!index->is_valid(), "should be");
    // here index should be illegal so we can replace it with the displacement
    // loaded into a register
    // mov tmp, disp
    // ldr r, [base, tmp]
    index = new_pointer_register();
    __ move(LIR_OprFact::intptrConst(disp), index);
    disp = 0;
  }

  assert(Address::offset_ok_for_immed(disp, Address::toInsnDataType(type)), "must be");
  return new LIR_Address(base, index, (LIR_Address::Scale) shift, disp, type);
}


LIR_Address* LIRGenerator::emit_array_address(LIR_Opr array_opr, LIR_Opr index_opr,
                                              BasicType type, bool needs_card_mark) {
  int offset_in_bytes = arrayOopDesc::base_offset_in_bytes(type);
  int elem_size = type2aelembytes(type);
  int shift = exact_log2(elem_size);

  LIR_Address* addr = generate_address(array_opr, index_opr, shift, offset_in_bytes, type);

  if (needs_card_mark) {
    // This store will need a precise card mark, so go ahead and
    // compute the full adddres instead of computing once for the
    // store and again for the card mark.
    LIR_Opr tmp = new_pointer_register();
    __ leal(LIR_OprFact::address(addr), tmp);
    return new LIR_Address(tmp, type);
  } else {
    return addr;
  }
}

LIR_Opr LIRGenerator::load_immediate(int x, BasicType type) {
  LIR_Opr r;
  if (type == T_LONG) {
    r = LIR_OprFact::longConst(x);
    if (!Assembler::operand_valid_for_logical_immediate(false, x)) {
      LIR_Opr tmp = new_register(type);
      __ move(r, tmp);
      return tmp;
    }
  } else if (type == T_INT) {
    r = LIR_OprFact::intConst(x);
    if (!Assembler::operand_valid_for_logical_immediate(true, x)) {
      // This is all rather nasty.  We don't know whether our constant
      // is required for a logical or an arithmetic operation, wo we
      // don't know what the range of valid values is!!
      LIR_Opr tmp = new_register(type);
      __ move(r, tmp);
      return tmp;
    }
  } else {
    ShouldNotReachHere();
  }
  return r;
}



void LIRGenerator::increment_counter(address counter, BasicType type, int step) {
  LIR_Opr pointer = new_pointer_register();
  __ move(LIR_OprFact::intptrConst(counter), pointer);
  LIR_Address* addr = new LIR_Address(pointer, type);
  increment_counter(addr, step);
}


void LIRGenerator::increment_counter(LIR_Address* addr, int step) {
  LIR_Opr imm = NULL;
  switch(addr->type()) {
  case T_INT:
    imm = LIR_OprFact::intConst(step);
    break;
  case T_LONG:
    imm = LIR_OprFact::longConst(step);
    break;
  default:
    ShouldNotReachHere();
  }
  LIR_Opr reg = new_register(addr->type());
  __ load(addr, reg);
  __ add(reg, imm, reg);
  __ store(reg, addr);
}

void LIRGenerator::cmp_mem_int(LIR_Condition condition, LIR_Opr base, int disp, int c, CodeEmitInfo* info) {
  LIR_Opr reg = new_register(T_INT);
  __ load(generate_address(base, disp, T_INT), reg, info);
  __ cmp(condition, reg, LIR_OprFact::intConst(c));
}

void LIRGenerator::cmp_reg_mem(LIR_Condition condition, LIR_Opr reg, LIR_Opr base, int disp, BasicType type, CodeEmitInfo* info) {
  LIR_Opr reg1 = new_register(T_INT);
  __ load(generate_address(base, disp, type), reg1, info);
  __ cmp(condition, reg, reg1);
}


bool LIRGenerator::strength_reduce_multiply(LIR_Opr left, int c, LIR_Opr result, LIR_Opr tmp) {

  if (is_power_of_2(c - 1)) {
    __ shift_left(left, exact_log2(c - 1), tmp);
    __ add(tmp, left, result);
    return true;
  } else if (is_power_of_2(c + 1)) {
    __ shift_left(left, exact_log2(c + 1), tmp);
    __ sub(tmp, left, result);
    return true;
  } else {
    return false;
  }
}

void LIRGenerator::store_stack_parameter (LIR_Opr item, ByteSize offset_from_sp) {
  BasicType type = item->type();
  __ store(item, new LIR_Address(FrameMap::sp_opr, in_bytes(offset_from_sp), type));
}

//----------------------------------------------------------------------
//             visitor functions
//----------------------------------------------------------------------


void LIRGenerator::do_StoreIndexed(StoreIndexed* x) {
  assert(x->is_pinned(),"");
  bool needs_range_check = x->compute_needs_range_check();
  bool use_length = x->length() != NULL;
  bool obj_store = x->elt_type() == T_ARRAY || x->elt_type() == T_OBJECT;
  bool needs_store_check = obj_store && (x->value()->as_Constant() == NULL ||
                                         !get_jobject_constant(x->value())->is_null_object() ||
                                         x->should_profile());

  LIRItem array(x->array(), this);
  LIRItem index(x->index(), this);
  LIRItem value(x->value(), this);
  LIRItem length(this);

  array.load_item();
  index.load_nonconstant();

  if (use_length && needs_range_check) {
    length.set_instruction(x->length());
    length.load_item();

  }
  if (needs_store_check  || x->check_boolean()) {
    value.load_item();
  } else {
    value.load_for_store(x->elt_type());
  }

  set_no_result(x);

  // the CodeEmitInfo must be duplicated for each different
  // LIR-instruction because spilling can occur anywhere between two
  // instructions and so the debug information must be different
  CodeEmitInfo* range_check_info = state_for(x);
  CodeEmitInfo* null_check_info = NULL;
  if (x->needs_null_check()) {
    null_check_info = new CodeEmitInfo(range_check_info);
  }

  // emit array address setup early so it schedules better
  // FIXME?  No harm in this on aarch64, and it might help
  LIR_Address* array_addr = emit_array_address(array.result(), index.result(), x->elt_type(), obj_store);

  if (GenerateRangeChecks && needs_range_check) {
    if (use_length) {
      __ cmp(lir_cond_belowEqual, length.result(), index.result());
      __ branch(lir_cond_belowEqual, T_INT, new RangeCheckStub(range_check_info, index.result()));
    } else {
      array_range_check(array.result(), index.result(), null_check_info, range_check_info);
      // range_check also does the null check
      null_check_info = NULL;
    }
  }

  if (GenerateArrayStoreCheck && needs_store_check) {
    LIR_Opr tmp1 = new_register(objectType);
    LIR_Opr tmp2 = new_register(objectType);
    LIR_Opr tmp3 = new_register(objectType);

    CodeEmitInfo* store_check_info = new CodeEmitInfo(range_check_info);
    __ store_check(value.result(), array.result(), tmp1, tmp2, tmp3, store_check_info, x->profiled_method(), x->profiled_bci());
  }

  if (obj_store) {
    // Needs GC write barriers.
    pre_barrier(LIR_OprFact::address(array_addr), LIR_OprFact::illegalOpr /* pre_val */,
                true /* do_load */, false /* patch */, NULL);
    __ move(value.result(), array_addr, null_check_info);
    // Seems to be a precise
    post_barrier(LIR_OprFact::address(array_addr), value.result());
  } else {
    LIR_Opr result = maybe_mask_boolean(x, array.result(), value.result(), null_check_info);
    __ move(result, array_addr, null_check_info);
  }
}

void LIRGenerator::do_MonitorEnter(MonitorEnter* x) {
  assert(x->is_pinned(),"");
  LIRItem obj(x->obj(), this);
  obj.load_item();

  set_no_result(x);

  // "lock" stores the address of the monitor stack slot, so this is not an oop
  LIR_Opr lock = new_register(T_INT);
  // Need a scratch register for biased locking
  LIR_Opr scratch = LIR_OprFact::illegalOpr;
  if (UseBiasedLocking) {
    scratch = new_register(T_INT);
  }

  CodeEmitInfo* info_for_exception = NULL;
  if (x->needs_null_check()) {
    info_for_exception = state_for(x);
  }
  // this CodeEmitInfo must not have the xhandlers because here the
  // object is already locked (xhandlers expect object to be unlocked)
  CodeEmitInfo* info = state_for(x, x->state(), true);
  monitor_enter(obj.result(), lock, syncTempOpr(), scratch,
                        x->monitor_no(), info_for_exception, info);
}


void LIRGenerator::do_MonitorExit(MonitorExit* x) {
  assert(x->is_pinned(),"");

  LIRItem obj(x->obj(), this);
  obj.dont_load_item();

  LIR_Opr lock = new_register(T_INT);
  LIR_Opr obj_temp = new_register(T_INT);
  set_no_result(x);
  monitor_exit(obj_temp, lock, syncTempOpr(), LIR_OprFact::illegalOpr, x->monitor_no());
}


void LIRGenerator::do_NegateOp(NegateOp* x) {
#ifdef __SOFTFP__
  if(x->x()->type()->is_float_kind() && !(hasFPU())) {
      address entry;
      if (x->x()->type()->is_float()) {
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::fneg);
      } else {
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::dneg);
      }
      LIR_Opr result = call_runtime(x->x(), entry, x->type(), NULL);
      set_result(x, result);
  } else
#endif
  {
  LIRItem from(x->x(), this);
  from.load_item();
  LIR_Opr result = rlock_result(x);
  __ negate (from.result(), result);
  }
}

// for  _fadd, _fmul, _fsub, _fdiv, _frem
//      _dadd, _dmul, _dsub, _ddiv, _drem
void LIRGenerator::do_ArithmeticOp_FPU(ArithmeticOp* x) {

  if (x->op() == Bytecodes::_frem || x->op() == Bytecodes::_drem) {
    address entry;
    if (x->op() == Bytecodes::_frem) {
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::frem);
    } else {
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::drem);
    }
    LIR_Opr result = call_runtime(x->x(), x->y(), entry, x->type(), NULL);
    set_result(x, result);

    return;
  }

  if(hasFPU()) {
        LIRItem left(x->x(),  this);
        LIRItem right(x->y(), this);
        LIRItem* left_arg  = &left;
        LIRItem* right_arg = &right;

        // Always load right hand side.
        right.load_item();

        if (!left.is_register())
          left.load_item();

        LIR_Opr reg = rlock(x);
        LIR_Opr tmp = LIR_OprFact::illegalOpr;
        if (x->is_strictfp() && (x->op() == Bytecodes::_dmul || x->op() == Bytecodes::_ddiv)) {
          tmp = new_register(T_DOUBLE);
        }

        arithmetic_op_fpu(x->op(), reg, left.result(), right.result(), NULL);

        set_result(x, round_item(reg));
  } else {
#ifdef __SOFTFP__
    address entry;

    switch (x->op()) {
      case Bytecodes::_fmul:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::fmul);
        break;
      case Bytecodes::_dmul:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::dmul);
        break;
      case Bytecodes::_fdiv:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::fdiv);
        break;
      case Bytecodes::_ddiv:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::ddiv);
        break;
      case Bytecodes::_fadd:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::fadd);
        break;
      case Bytecodes::_dadd:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::dadd);
        break;
      case Bytecodes::_fsub:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::fsub);
        break;
      case Bytecodes::_dsub:
        entry = CAST_FROM_FN_PTR(address, SharedRuntime::dsub);
        break;
      default:
          ShouldNotReachHere();
    }
    LIR_Opr result = call_runtime(x->x(), x->y(),  entry,  x->type(), NULL);
    set_result(x, result);
#else
    ShouldNotReachHere();// check your compiler settings
#endif
  }
}

// for  _ladd, _lmul, _lsub, _ldiv, _lrem
void LIRGenerator::do_ArithmeticOp_Long(ArithmeticOp* x) {

  // missing test if instr is commutative and if we should swap
  LIRItem left(x->x(), this);
  LIRItem right(x->y(), this);

  if (x->op() == Bytecodes::_ldiv || x->op() == Bytecodes::_lrem) {

    BasicTypeList signature(2);
    signature.append(T_LONG);
    signature.append(T_LONG);
    CallingConvention* cc = frame_map()->c_calling_convention(&signature);

    // check for division by zero (destroys registers of right operand!)
    CodeEmitInfo* info = state_for(x);

    right.load_item();

    __ cmp(lir_cond_equal, right.result(), LIR_OprFact::longConst(0));
    __ branch(lir_cond_equal, T_LONG, new DivByZeroStub(info));

    const LIR_Opr result_reg = result_register_for(x->type());
    left.load_item_force(cc->at(1));
    __ move(right.result(), cc->at(0));

    address entry;
    switch (x->op()) {
    case Bytecodes::_lrem:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::lrem);
      break; // check if dividend is 0 is done elsewhere
    case Bytecodes::_ldiv:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::ldiv);
      break; // check if dividend is 0 is done elsewhere
    default:
      ShouldNotReachHere();
    }

    LIR_Opr result = rlock_result(x);
    __ call_runtime_leaf(entry, getThreadTemp(), result_reg, cc->args());
    __ move(result_reg, result);
  } else {
    assert (x->op() == Bytecodes::_lmul || x->op() == Bytecodes::_ladd || x->op() == Bytecodes::_lsub,
            "expect lmul, ladd or lsub");
    // add, sub, mul
    left.load_item();
    if (! right.is_register()) {
      if (x->op() == Bytecodes::_lmul
          || ! right.is_constant()
          || ! Assembler::operand_valid_for_add_sub_immediate(right.get_jlong_constant())) {
        right.load_item();
      } else { // add, sub
        assert (x->op() == Bytecodes::_ladd || x->op() == Bytecodes::_lsub, "expect ladd or lsub");
        // don't load constants to save register
        right.load_nonconstant();
      }
    }
    rlock_result(x);
    arithmetic_op_long(x->op(), x->operand(), left.result(), right.result(), NULL);
  }
}

// for: _iadd, _imul, _isub, _idiv, _irem
void LIRGenerator::do_ArithmeticOp_Int(ArithmeticOp* x) {

  // Test if instr is commutative and if we should swap
  LIRItem left(x->x(),  this);
  LIRItem right(x->y(), this);
  LIRItem* left_arg = &left;
  LIRItem* right_arg = &right;
  if (x->is_commutative() && left.is_stack() && right.is_register()) {
    // swap them if left is real stack (or cached) and right is real register(not cached)
    left_arg = &right;
    right_arg = &left;
  }

  left_arg->load_item();

  // do not need to load right, as we can handle stack and constants
  if (x->op() == Bytecodes::_idiv || x->op() == Bytecodes::_irem) {

    right_arg->load_item();
    rlock_result(x);

    if (!(VM_Version::features() & FT_HW_DIVIDE)) {
      // MacroAssembler::divide32 destroys both operand registers
      left_arg->set_destroys_register();
      right_arg->set_destroys_register();
    }

    CodeEmitInfo* info = state_for(x);
    LIR_Opr tmp = new_register(T_INT);
    __ cmp(lir_cond_equal, right_arg->result(), LIR_OprFact::intConst(0));
    __ branch(lir_cond_equal, T_INT, new DivByZeroStub(info));
    info = state_for(x);

    if (x->op() == Bytecodes::_irem) {
      __ irem(left_arg->result(), right_arg->result(), x->operand(), tmp, NULL);
    } else if (x->op() == Bytecodes::_idiv) {
      __ idiv(left_arg->result(), right_arg->result(), x->operand(), tmp, NULL);
    }

  } else if (x->op() == Bytecodes::_iadd || x->op() == Bytecodes::_isub) {
    if (right.is_constant()
        && Assembler::operand_valid_for_add_sub_immediate(right.get_jint_constant())) {
      right.load_nonconstant();
    } else {
      right.load_item();
    }
    rlock_result(x);
    arithmetic_op_int(x->op(), x->operand(), left_arg->result(), right_arg->result(), LIR_OprFact::illegalOpr);
  } else {
    assert (x->op() == Bytecodes::_imul, "expect imul");
    if (right.is_constant()) {
      int c = right.get_jint_constant();
      if (! is_power_of_2(c) && ! is_power_of_2(c + 1) && ! is_power_of_2(c - 1)) {
        // Cannot use constant op.
        right.load_item();
      } else {
        right.dont_load_item();
      }
    } else {
      right.load_item();
    }
    rlock_result(x);
    arithmetic_op_int(x->op(), x->operand(), left_arg->result(), right_arg->result(), new_register(T_INT));
  }
}

void LIRGenerator::do_ArithmeticOp(ArithmeticOp* x) {
  // when an operand with use count 1 is the left operand, then it is
  // likely that no move for 2-operand-LIR-form is necessary
  if (x->is_commutative() && x->y()->as_Constant() == NULL && x->x()->use_count() > x->y()->use_count()) {
    x->swap_operands();
  }

  ValueTag tag = x->type()->tag();
  assert(x->x()->type()->tag() == tag && x->y()->type()->tag() == tag, "wrong parameters");
  switch (tag) {
    case floatTag:
    case doubleTag:  do_ArithmeticOp_FPU(x);  return;
    case longTag:    do_ArithmeticOp_Long(x); return;
    case intTag:     do_ArithmeticOp_Int(x);  return;
  }
  ShouldNotReachHere();
}

// _ishl, _lshl, _ishr, _lshr, _iushr, _lushr
void LIRGenerator::do_ShiftOp(ShiftOp* x) {

  LIRItem left(x->x(),  this);
  LIRItem right(x->y(), this);

  left.load_item();

  rlock_result(x);
  if (right.is_constant()) {
    right.dont_load_item();

    switch (x->op()) {
    case Bytecodes::_ishl: {
      int c = right.get_jint_constant() & 0x1f;
      __ shift_left(left.result(), c, x->operand());
      break;
    }
    case Bytecodes::_ishr: {
      int c = right.get_jint_constant() & 0x1f;
      __ shift_right(left.result(), c, x->operand());
      break;
    }
    case Bytecodes::_iushr: {
      int c = right.get_jint_constant() & 0x1f;
      __ unsigned_shift_right(left.result(), c, x->operand());
      break;
    }
    case Bytecodes::_lshl: {
      int c = right.get_jint_constant() & 0x3f;
      __ shift_left(left.result(), c, x->operand());
      break;
    }
    case Bytecodes::_lshr: {
      int c = right.get_jint_constant() & 0x3f;
      __ shift_right(left.result(), c, x->operand());
      break;
    }
    case Bytecodes::_lushr: {
      int c = right.get_jint_constant() & 0x3f;
      __ unsigned_shift_right(left.result(), c, x->operand());
      break;
    }
    default:
      ShouldNotReachHere();
    }
  } else {
    right.load_item();
    LIR_Opr tmp = LIR_OprFact::illegalOpr;
    if (left.result()->type() == T_LONG)
      left.set_destroys_register();
    switch (x->op()) {
    case Bytecodes::_ishl: {
      __ shift_left(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    case Bytecodes::_ishr: {
      __ shift_right(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    case Bytecodes::_iushr: {
      __ unsigned_shift_right(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    case Bytecodes::_lshl: {
      __ shift_left(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    case Bytecodes::_lshr: {
      __ shift_right(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    case Bytecodes::_lushr: {
      __ unsigned_shift_right(left.result(), right.result(), x->operand(), tmp);
      break;
    }
    default:
      ShouldNotReachHere();
    }
  }
}

// _iand, _land, _ior, _lor, _ixor, _lxor
void LIRGenerator::do_LogicOp(LogicOp* x) {

  LIRItem left(x->x(),  this);
  LIRItem right(x->y(), this);

  left.load_item();

  rlock_result(x);
  if (right.is_constant()
      && ((right.type()->tag() == intTag
           && Assembler::operand_valid_for_logical_immediate(true, right.get_jint_constant()))
          || (right.type()->tag() == longTag
              && Assembler::operand_valid_for_logical_immediate(false, right.get_jlong_constant()))))  {
    right.dont_load_item();
  } else {
    right.load_item();
  }
  switch (x->op()) {
  case Bytecodes::_iand:
  case Bytecodes::_land:
    __ logical_and(left.result(), right.result(), x->operand()); break;
  case Bytecodes::_ior:
  case Bytecodes::_lor:
    __ logical_or (left.result(), right.result(), x->operand()); break;
  case Bytecodes::_ixor:
  case Bytecodes::_lxor:
    __ logical_xor(left.result(), right.result(), x->operand()); break;
  default: Unimplemented();
  }
}

// _lcmp, _fcmpl, _fcmpg, _dcmpl, _dcmpg
void LIRGenerator::do_CompareOp(CompareOp* x) {
  LIRItem left(x->x(), this);
  LIRItem right(x->y(), this);
  ValueTag tag = x->x()->type()->tag();
  left.load_item();
  right.load_item();

  if (x->x()->type()->is_float_kind()) {
    Bytecodes::Code code = x->op();
    if(hasFPU()) {
        LIR_Opr reg = rlock_result(x);
        __ fcmp2int(left.result(), right.result(), reg, (code == Bytecodes::_fcmpl || code == Bytecodes::_dcmpl));
    } else {
#ifdef __SOFTFP__
        address entry;
        switch (code) {
        case Bytecodes::_fcmpl:
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::fcmpl);
          break;
        case Bytecodes::_fcmpg:
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::fcmpg);
          break;
        case Bytecodes::_dcmpl:
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::dcmpl);
          break;
        case Bytecodes::_dcmpg:
          entry = CAST_FROM_FN_PTR(address, SharedRuntime::dcmpg);
          break;
        default:
          ShouldNotReachHere();
        }

        LIR_Opr result = call_runtime(x->x(), x->y(),  entry,  x->type(), NULL);
        set_result(x, result);
#else
        ShouldNotReachHere(); // check your compiler settings
#endif
    }
  } else if (x->x()->type()->tag() == longTag) {
    LIR_Opr reg = rlock_result(x);
    __ lcmp2int(left.result(), right.result(), reg);
  } else {
    Unimplemented();
  }
}

void LIRGenerator::do_CompareAndSwap(Intrinsic* x, ValueType* type) {
  assert(x->number_of_arguments() == 4, "wrong type");
  LIRItem obj   (x->argument_at(0), this);  // object
  LIRItem offset(x->argument_at(1), this);  // offset of field
  LIRItem cmp   (x->argument_at(2), this);  // value to compare with field
  LIRItem val   (x->argument_at(3), this);  // replace field with val if matches cmp

  assert(obj.type()->tag() == objectTag, "invalid type");

  // In 64bit the type can be long, sparc doesn't have this assert
  // assert(offset.type()->tag() == intTag, "invalid type");

  assert(cmp.type()->tag() == type->tag(), "invalid type");
  assert(val.type()->tag() == type->tag(), "invalid type");

  // get address of field
  obj.load_item();
  offset.load_nonconstant();
  if (type == longType) {
    // not need if allocator reserves correct pairs
    val.load_item_force(FrameMap::long0_opr);
  } else {
    val.load_item();
  }
  cmp.load_item();

  LIR_Address* a;
  if(offset.result()->is_constant()) {
    jint c = offset.result()->as_jint();
    a = new LIR_Address(obj.result(),
                        c,
                        as_BasicType(type));
  } else {
    a = new LIR_Address(obj.result(),
                        offset.result(),
                        LIR_Address::times_1,
                        0,
                        as_BasicType(type));
  }
  LIR_Opr addr = new_pointer_register();
  __ leal(LIR_OprFact::address(a), addr);

  if (type == objectType) {  // Write-barrier needed for Object fields.
    // Do the pre-write barrier, if any.
    pre_barrier(addr, LIR_OprFact::illegalOpr /* pre_val */,
                true /* do_load */, false /* patch */, NULL);
  }

  LIR_Opr result = rlock_result(x);

  LIR_Opr ill = LIR_OprFact::illegalOpr;  // for convenience
  if (type == objectType)
    __ cas_obj(addr, cmp.result(), val.result(), ill, ill, result);
  else if (type == intType)
    __ cas_int(addr, cmp.result(), val.result(), ill, ill, result);
  else if (type == longType)
    __ cas_long(addr, cmp.result(), val.result(), FrameMap::long1_opr, ill,
                result);
  else {
    ShouldNotReachHere();
  }

  __ logical_xor(result, LIR_OprFact::intConst(1), result);

  if (type == objectType) {   // Write-barrier needed for Object fields.
    // Seems to be precise
    post_barrier(addr, val.result());
  }
}

void LIRGenerator::do_MathIntrinsic(Intrinsic* x) {
  switch (x->id()) {
    default:
        ShouldNotReachHere();
        break;
    case vmIntrinsics::_dabs:
    case vmIntrinsics::_dsqrt:
        if(hasFPU()) {
            assert(x->number_of_arguments() == 1, "wrong type");
            LIRItem value(x->argument_at(0), this);
            value.load_item();
            LIR_Opr dst = rlock_result(x);

            switch (x->id()) {
            case vmIntrinsics::_dsqrt: {
              __ sqrt(value.result(), dst, LIR_OprFact::illegalOpr);
              break;
            }
            case vmIntrinsics::_dabs: {
              __ abs(value.result(), dst, LIR_OprFact::illegalOpr);
              break;
            }
            }
            break;
      }// fall through for FPU less cores
    case vmIntrinsics::_dlog10: // fall through
    case vmIntrinsics::_dlog: // fall through
    case vmIntrinsics::_dsin: // fall through
    case vmIntrinsics::_dtan: // fall through
    case vmIntrinsics::_dcos: // fall through
    case vmIntrinsics::_dexp: {
      assert(x->number_of_arguments() == 1, "wrong type");

      address runtime_entry = NULL;
      switch (x->id()) {
#ifdef __SOFTFP__
      case vmIntrinsics::_dabs:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dabs);
        break;
      case vmIntrinsics::_dsqrt:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dsqrt);
        break;
#endif
      case vmIntrinsics::_dsin:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dsin);
        break;
      case vmIntrinsics::_dcos:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dcos);
        break;
      case vmIntrinsics::_dtan:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dtan);
        break;
      case vmIntrinsics::_dlog:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dlog);
        break;
      case vmIntrinsics::_dlog10:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dlog10);
        break;
      case vmIntrinsics::_dexp:
        runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dexp);
        break;
      default:
        ShouldNotReachHere();
      }

      LIR_Opr result = call_runtime(x->argument_at(0), runtime_entry, x->type(), NULL);
      set_result(x, result);
      break;
    }
    case vmIntrinsics::_dpow: {
      assert(x->number_of_arguments() == 2, "wrong type");
      address runtime_entry = CAST_FROM_FN_PTR(address, SharedRuntime::dpow);
      LIR_Opr result = call_runtime(x->argument_at(0), x->argument_at(1), runtime_entry, x->type(), NULL);
      set_result(x, result);
      break;
    }
  }
}


void LIRGenerator::do_ArrayCopy(Intrinsic* x) {
  assert(x->number_of_arguments() == 5, "wrong type");

  // Make all state_for calls early since they can emit code
  CodeEmitInfo* info = state_for(x, x->state());

  LIRItem src(x->argument_at(0), this);
  LIRItem src_pos(x->argument_at(1), this);
  LIRItem dst(x->argument_at(2), this);
  LIRItem dst_pos(x->argument_at(3), this);
  LIRItem length(x->argument_at(4), this);

  // operands for arraycopy must use fixed registers, otherwise
  // LinearScan will fail allocation (because arraycopy always needs a
  // call)

  // The java calling convention does not give us enough registers
  // so we occupy two more: r4 and r5. The fast path code will be able to
  // make use of these registers for performance purpose. If going into
  // slow path we'll spill extra data to the stack as necessary

  src.load_item_force     (FrameMap::as_oop_opr(j_rarg0));
  src_pos.load_item_force (FrameMap::as_opr(j_rarg1));
  dst.load_item_force     (FrameMap::as_oop_opr(j_rarg2));
  dst_pos.load_item_force (FrameMap::as_opr(j_rarg3));

  length.load_item_force  (FrameMap::as_opr(r4));
  LIR_Opr tmp =           FrameMap::as_opr(r5);

  set_no_result(x);

  int flags;
  ciArrayKlass* expected_type;
  arraycopy_helper(x, &flags, &expected_type);

  __ arraycopy(src.result(), src_pos.result(), dst.result(), dst_pos.result(), length.result(), tmp, expected_type, flags, info); // does add_safepoint
}

void LIRGenerator::do_update_CRC32(Intrinsic* x) {
  assert(UseCRC32Intrinsics, "why are we here?");
  // Make all state_for calls early since they can emit code
  LIR_Opr result = rlock_result(x);
  switch (x->id()) {
    case vmIntrinsics::_updateCRC32: {
      LIRItem crc(x->argument_at(0), this);
      LIRItem val(x->argument_at(1), this);
      // val is destroyed by update_crc32
      val.set_destroys_register();
      crc.load_item();
      val.load_item();
      __ update_crc32(crc.result(), val.result(), result);
      break;
    }
    case vmIntrinsics::_updateBytesCRC32:
    case vmIntrinsics::_updateByteBufferCRC32: {
      bool is_updateBytes = (x->id() == vmIntrinsics::_updateBytesCRC32);

      LIRItem crc(x->argument_at(0), this);
      LIRItem buf(x->argument_at(1), this);
      LIRItem off(x->argument_at(2), this);
      LIRItem len(x->argument_at(3), this);
      buf.load_item();
      off.load_nonconstant();

      LIR_Opr index = off.result();
      int offset = is_updateBytes ? arrayOopDesc::base_offset_in_bytes(T_BYTE) : 0;
      if(off.result()->is_constant()) {
        index = LIR_OprFact::illegalOpr;
       offset += off.result()->as_jint();
      }
      LIR_Opr base_op = buf.result();

      if (!is_updateBytes) { // long b raw address
         base_op = new_register(T_INT);
         __ convert(Bytecodes::_l2i, buf.result(), base_op);
      }

      if (offset) {
        LIR_Opr tmp = new_pointer_register();
        __ add(base_op, LIR_OprFact::intConst(offset), tmp);
        base_op = tmp;
        offset = 0;
      }

      LIR_Address* a = new LIR_Address(base_op,
                                       index,
                                       LIR_Address::times_1,
                                       offset,
                                       T_BYTE);
      BasicTypeList signature(3);
      signature.append(T_INT);
      signature.append(T_ADDRESS);
      signature.append(T_INT);
      CallingConvention* cc = frame_map()->c_calling_convention(&signature);
      const LIR_Opr result_reg = result_register_for(x->type());

      LIR_Opr addr = new_pointer_register();
      __ leal(LIR_OprFact::address(a), addr);

      crc.load_item_force(cc->at(0));
      __ move(addr, cc->at(1));
      len.load_item_force(cc->at(2));

      __ call_runtime_leaf(StubRoutines::updateBytesCRC32(), getThreadTemp(), result_reg, cc->args());
      __ move(result_reg, result);

      break;
    }
    default: {
      ShouldNotReachHere();
    }
  }
}

// _i2l, _i2f, _i2d, _l2i, _l2f, _l2d, _f2i, _f2l, _f2d, _d2i, _d2l, _d2f
// _i2b, _i2c, _i2s
void LIRGenerator::do_Convert(Convert* x) {
    address entry = NULL;
  switch (x->op()) {
  case Bytecodes::_d2i:
  case Bytecodes::_f2i:
  case Bytecodes::_i2f:
  case Bytecodes::_i2d:
  case Bytecodes::_f2d:
  case Bytecodes::_d2f:
      if(hasFPU()) {
          break;
      }// fall through for FPU-less cores
  case Bytecodes::_d2l:
  case Bytecodes::_f2l:
  case Bytecodes::_l2d:
  case Bytecodes::_l2f: {

    switch (x->op()) {
#ifdef __SOFTFP__
    case Bytecodes::_i2f:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::i2f);
      break;
    case Bytecodes::_i2d:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::i2d);
      break;
    case Bytecodes::_f2d:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::f2d);
      break;
    case Bytecodes::_d2f:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::d2f);
      break;
    case Bytecodes::_d2i:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::d2i);
      break;
    case Bytecodes::_f2i:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::f2i);
      break;
#endif
    case Bytecodes::_d2l:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::d2l);
      break;
    case Bytecodes::_f2l:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::f2l);
      break;
    case Bytecodes::_l2d:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::l2d);
      break;
    case Bytecodes::_l2f:
      entry = CAST_FROM_FN_PTR(address, SharedRuntime::l2f);
      break;
    default:
      ShouldNotReachHere();
    }

    LIR_Opr result = call_runtime(x->value(), entry, x->type(), NULL);
    set_result(x, result);
  }
  break;

  default:
    break;
}
    if(NULL == entry) {
    LIRItem value(x->value(), this);
    value.load_item();

    if (x->op() == Bytecodes::_f2i || x->op() == Bytecodes::_d2i) {
      value.set_destroys_register();
    }

    LIR_Opr input = value.result();
    LIR_Opr result = rlock(x);

    __ convert(x->op(), input, result);

    assert(result->is_virtual(), "result must be virtual register");
    set_result(x, result);
  }
}

void LIRGenerator::do_NewInstance(NewInstance* x) {
#ifndef PRODUCT
  if (PrintNotLoaded && !x->klass()->is_loaded()) {
    tty->print_cr("   ###class not loaded at new bci %d", x->printable_bci());
  }
#endif
  CodeEmitInfo* info = state_for(x, x->state());
  LIR_Opr reg = result_register_for(x->type());
  new_instance(reg, x->klass(), x->is_unresolved(),
                       FrameMap::r2_oop_opr,
                       FrameMap::r5_oop_opr,
                       FrameMap::r4_oop_opr,
                       LIR_OprFact::illegalOpr,
                       FrameMap::r3_metadata_opr, info);
  LIR_Opr result = rlock_result(x);
  __ move(reg, result);
}

void LIRGenerator::do_NewTypeArray(NewTypeArray* x) {
  CodeEmitInfo* info = state_for(x, x->state());

  LIRItem length(x->length(), this);
  length.load_item_force(FrameMap::r6_opr);

  LIR_Opr reg = result_register_for(x->type());
  LIR_Opr tmp1 = FrameMap::r2_oop_opr;
  LIR_Opr tmp2 = FrameMap::r4_oop_opr;
  LIR_Opr tmp3 = FrameMap::r5_oop_opr;
  LIR_Opr tmp4 = reg;
  LIR_Opr klass_reg = FrameMap::r3_metadata_opr;
  LIR_Opr len = length.result();
  BasicType elem_type = x->elt_type();

  __ metadata2reg(ciTypeArrayKlass::make(elem_type)->constant_encoding(), klass_reg);

  CodeStub* slow_path = new NewTypeArrayStub(klass_reg, len, reg, info);
  __ allocate_array(reg, len, tmp1, tmp2, tmp3, tmp4, elem_type, klass_reg, slow_path);

  LIR_Opr result = rlock_result(x);
  __ move(reg, result);
}

void LIRGenerator::do_NewObjectArray(NewObjectArray* x) {
  LIRItem length(x->length(), this);
  // in case of patching (i.e., object class is not yet loaded), we need to reexecute the instruction
  // and therefore provide the state before the parameters have been consumed
  CodeEmitInfo* patching_info = NULL;
  if (!x->klass()->is_loaded() || PatchALot) {
    patching_info =  state_for(x, x->state_before());
  }

  CodeEmitInfo* info = state_for(x, x->state());

  LIR_Opr reg = result_register_for(x->type());
  LIR_Opr tmp1 = FrameMap::r2_oop_opr;
  LIR_Opr tmp2 = FrameMap::r4_oop_opr;
  LIR_Opr tmp3 = FrameMap::r5_oop_opr;
  LIR_Opr tmp4 = reg;
  LIR_Opr klass_reg = FrameMap::r3_metadata_opr;

  length.load_item_force(FrameMap::r6_opr);
  LIR_Opr len = length.result();

  CodeStub* slow_path = new NewObjectArrayStub(klass_reg, len, reg, info);
  ciKlass* obj = (ciKlass*) ciObjArrayKlass::make(x->klass());
  if (obj == ciEnv::unloaded_ciobjarrayklass()) {
    BAILOUT("encountered unloaded_ciobjarrayklass due to out of memory error");
  }
  klass2reg_with_patching(klass_reg, obj, patching_info);
  __ allocate_array(reg, len, tmp1, tmp2, tmp3, tmp4, T_OBJECT, klass_reg, slow_path);

  LIR_Opr result = rlock_result(x);
  __ move(reg, result);
}


void LIRGenerator::do_NewMultiArray(NewMultiArray* x) {
  Values* dims = x->dims();
  int i = dims->length();
  LIRItemList* items = new LIRItemList(dims->length(), NULL);
  while (i-- > 0) {
    LIRItem* size = new LIRItem(dims->at(i), this);
    items->at_put(i, size);
  }

  // Evaluate state_for early since it may emit code.
  CodeEmitInfo* patching_info = NULL;
  if (!x->klass()->is_loaded() || PatchALot) {
    patching_info = state_for(x, x->state_before());

    // Cannot re-use same xhandlers for multiple CodeEmitInfos, so
    // clone all handlers (NOTE: Usually this is handled transparently
    // by the CodeEmitInfo cloning logic in CodeStub constructors but
    // is done explicitly here because a stub isn't being used).
    x->set_exception_handlers(new XHandlers(x->exception_handlers()));
  }
  CodeEmitInfo* info = state_for(x, x->state());

  i = dims->length();
  while (i-- > 0) {
    LIRItem* size = items->at(i);
    size->load_item();

    store_stack_parameter(size->result(), in_ByteSize(i*4));
  }

  LIR_Opr klass_reg = FrameMap::r1_metadata_opr;
  klass2reg_with_patching(klass_reg, x->klass(), patching_info);

  LIR_Opr rank = FrameMap::r2_opr;
  __ move(LIR_OprFact::intConst(x->rank()), rank);
  LIR_Opr varargs = FrameMap::r3_opr;
  __ move(FrameMap::sp_opr, varargs);
  LIR_OprList* args = new LIR_OprList(3);
  args->append(klass_reg);
  args->append(rank);
  args->append(varargs);
  LIR_Opr reg = result_register_for(x->type());
  __ call_runtime(Runtime1::entry_for(Runtime1::new_multi_array_id),
                  LIR_OprFact::illegalOpr,
                  reg, args, info);

  LIR_Opr result = rlock_result(x);
  __ move(reg, result);
}

void LIRGenerator::do_BlockBegin(BlockBegin* x) {
  // nothing to do for now
}

void LIRGenerator::do_CheckCast(CheckCast* x) {
  LIRItem obj(x->obj(), this);

  CodeEmitInfo* patching_info = NULL;
  if (!x->klass()->is_loaded() || (PatchALot && !x->is_incompatible_class_change_check())) {
    // must do this before locking the destination register as an oop register,
    // and before the obj is loaded (the latter is for deoptimization)
    patching_info = state_for(x, x->state_before());
  }
  obj.load_item();

  // info for exceptions
  CodeEmitInfo* info_for_exception =
    (x->needs_exception_state() ? state_for(x) :
     state_for(x, x->state_before(), true /*ignore_xhandler*/));

  CodeStub* stub;
  if (x->is_incompatible_class_change_check()) {
    assert(patching_info == NULL, "can't patch this");
    stub = new SimpleExceptionStub(Runtime1::throw_incompatible_class_change_error_id, LIR_OprFact::illegalOpr, info_for_exception);
  } else if (x->is_invokespecial_receiver_check()) {
    assert(patching_info == NULL, "can't patch this");
    stub = new DeoptimizeStub(info_for_exception);
  } else {
    stub = new SimpleExceptionStub(Runtime1::throw_class_cast_exception_id, obj.result(), info_for_exception);
  }
  LIR_Opr reg = rlock_result(x);
  LIR_Opr tmp3 = LIR_OprFact::illegalOpr;
  if (!x->klass()->is_loaded()) {
    tmp3 = new_register(objectType);
  }
  __ checkcast(reg, obj.result(), x->klass(),
               new_register(objectType), new_register(objectType), tmp3,
               x->direct_compare(), info_for_exception, patching_info, stub,
               x->profiled_method(), x->profiled_bci());
}

void LIRGenerator::do_InstanceOf(InstanceOf* x) {
  LIRItem obj(x->obj(), this);

  // result and test object may not be in same register
  LIR_Opr reg = rlock_result(x);
  CodeEmitInfo* patching_info = NULL;
  if ((!x->klass()->is_loaded() || PatchALot)) {
    // must do this before locking the destination register as an oop register
    patching_info = state_for(x, x->state_before());
  }
  obj.load_item();
  LIR_Opr tmp3 = LIR_OprFact::illegalOpr;
  if (!x->klass()->is_loaded()) {
    tmp3 = new_register(objectType);
  }
  __ instanceof(reg, obj.result(), x->klass(),
                new_register(objectType), new_register(objectType), tmp3,
                x->direct_compare(), patching_info, x->profiled_method(), x->profiled_bci());
}

void LIRGenerator::do_If(If* x) {
  assert(x->number_of_sux() == 2, "inconsistency");
  ValueTag tag = x->x()->type()->tag();

  If::Condition cond = x->cond();

  LIRItem xitem(x->x(), this);
  LIRItem yitem(x->y(), this);
  LIRItem* xin = &xitem;
  LIRItem* yin = &yitem;

  xin->load_item();

  if (yin->is_constant()) {
    if (tag == longTag
        && Assembler::operand_valid_for_add_sub_immediate(yin->get_jlong_constant())) {
      yin->dont_load_item();
    } else if (tag == intTag
        && Assembler::operand_valid_for_add_sub_immediate(yin->get_jint_constant())) {
      yin->dont_load_item();
    } else if (tag == addressTag
        && Assembler::operand_valid_for_add_sub_immediate(yin->get_address_constant())) {
      yin->dont_load_item();
    } else if (tag == objectTag && yin->get_jobject_constant()->is_null_object()) {
      yin->dont_load_item();
    } else {
      yin->load_item();
    }
  } else {
    yin->load_item();
  }

  // add safepoint before generating condition code so it can be recomputed
  if (x->is_safepoint()) {
    // increment backedge counter if needed
    increment_backedge_counter(state_for(x, x->state_before()), x->profiled_bci());
    __ safepoint(LIR_OprFact::illegalOpr, state_for(x, x->state_before()));
  }
  set_no_result(x);

  LIR_Opr left = xin->result();
  LIR_Opr right = yin->result();
  LIR_Condition lir_c = lir_cond(cond);

#ifdef __SOFTFP__
  if(x->x()->type()->is_float_kind() && !(hasFPU())) {// FPU-less cores
    address entry;
    bool unordered_flag = x->unordered_is_true() != (lir_c == lir_cond_greater || lir_c == lir_cond_lessEqual);
    if (x->x()->type()->is_float()) {
      entry = CAST_FROM_FN_PTR(address, unordered_flag ? SharedRuntime::fcmpg : SharedRuntime::fcmpl);
    } else if (x->x()->type()->is_double()) {
      entry = CAST_FROM_FN_PTR(address, unordered_flag ? SharedRuntime::dcmpg : SharedRuntime::dcmpl);
    } else {
        ShouldNotReachHere();
    }

    LIR_Opr fcmp_res = call_runtime(x->x(), x->y(), entry, intType, NULL);
    LIR_Opr zero = LIR_OprFact::intConst(0);
    __ cmp(lir_c, fcmp_res, zero);
  } else
#endif
  {
  __ cmp(lir_c, left, right);
  }

  // Generate branch profiling. Profiling code doesn't kill flags.
  profile_branch(x, cond);
  move_to_phi(x->state());
  if (x->x()->type()->is_float_kind()) {
      if(hasFPU()) {
        __ branch(lir_c, right->type(), x->tsux(), x->usux());
      } else {
        __ branch(lir_c, T_INT, x->tsux());
      }
  } else
  {
    __ branch(lir_c, right->type(), x->tsux());
  }
  assert(x->default_sux() == x->fsux(), "wrong destination above");
  __ jump(x->default_sux());
}

LIR_Opr LIRGenerator::getThreadPointer() {
   return FrameMap::as_pointer_opr(rthread);
}

void LIRGenerator::trace_block_entry(BlockBegin* block) {
  __ move(LIR_OprFact::intConst(block->block_id()), FrameMap::r0_opr);
  LIR_OprList* args = new LIR_OprList(1);
  args->append(FrameMap::r0_opr);
  address func = CAST_FROM_FN_PTR(address, Runtime1::trace_block_entry);
  __ call_runtime_leaf(func, LIR_OprFact::illegalOpr, LIR_OprFact::illegalOpr, args);
}

void LIRGenerator::volatile_field_store(LIR_Opr value, LIR_Address* address,
                                        CodeEmitInfo* info) {
  if (value->is_double_cpu()) {
    __ move(value, FrameMap::long0_opr);
    __ volatile_store_mem_reg(FrameMap::long0_opr, address, info);
  } else {
    __ volatile_store_mem_reg(value, address, info);
  }
}

void LIRGenerator::volatile_field_load(LIR_Address* address, LIR_Opr result,
                                       CodeEmitInfo* info) {
  if (result->is_double_cpu()) {
    __ volatile_load_mem_reg(address, FrameMap::long0_opr, info);
    __ move(FrameMap::long0_opr, result);
  } else {
    __ volatile_load_mem_reg(address, result, info);
  }
}

void LIRGenerator::get_Object_unsafe(LIR_Opr dst, LIR_Opr src, LIR_Opr offset,
                                     BasicType type, bool is_volatile) {
  LIR_Address* addr = new LIR_Address(src, offset, type);
  __ load(addr, dst);
}


void LIRGenerator::put_Object_unsafe(LIR_Opr src, LIR_Opr offset, LIR_Opr data,
                                     BasicType type, bool is_volatile) {
  LIR_Address* addr = new LIR_Address(src, offset, type);
  bool is_obj = (type == T_ARRAY || type == T_OBJECT);
  if (is_obj) {
    // Do the pre-write barrier, if any.
    pre_barrier(LIR_OprFact::address(addr), LIR_OprFact::illegalOpr /* pre_val */,
                true /* do_load */, false /* patch */, NULL);
    __ move(data, addr);
    assert(src->is_register(), "must be register");
    // Seems to be a precise address
    post_barrier(LIR_OprFact::address(addr), data);
  } else {
    __ move(data, addr);
  }
}

void LIRGenerator::do_UnsafeGetAndSetObject(UnsafeGetAndSetObject* x) {
  BasicType type = x->basic_type();
  LIRItem src(x->object(), this);
  LIRItem off(x->offset(), this);
  LIRItem value(x->value(), this);

  src.load_item();
  off.load_nonconstant();
  if (type == T_LONG && !x->is_add()) {
      // not need if allocator reserves correct pairs
      value.load_item_force(FrameMap::long1_opr);
  } else {
    // We can cope with a constant increment in an xadd
    if (! (x->is_add()
           && value.is_constant()
           && can_inline_as_constant(x->value()))) {
      value.load_item();
    }
  }

  bool is_long = (type == T_LONG);
  LIR_Opr dst = is_long ? FrameMap::long0_opr : rlock_result(x, type);
  LIR_Opr data = value.result();
  bool is_obj = (type == T_ARRAY || type == T_OBJECT);
  LIR_Opr offset = off.result();

  if (data == dst) {
    LIR_Opr tmp = new_register(data->type());
    __ move(data, tmp);
    data = tmp;
  }

  LIR_Address* addr;
  if (offset->is_constant()) {
    addr = new LIR_Address(src.result(), offset->as_jint(), type);
  } else {
    addr = new LIR_Address(src.result(), offset, type);
  }

  LIR_Opr tmp = new_register(T_INT);
  LIR_Opr ptr = LIR_OprFact::illegalOpr;

  if (x->is_add()) {
    __ xadd(LIR_OprFact::address(addr), data, dst, tmp);
  } else {
    if (is_obj) {
      // Do the pre-write barrier, if any.
      ptr = new_pointer_register();
      __ add(src.result(), off.result(), ptr);
      pre_barrier(ptr, LIR_OprFact::illegalOpr /* pre_val */,
                  true /* do_load */, false /* patch */, NULL);
    }
    __ xchg(LIR_OprFact::address(addr), data, dst, tmp);
    if (is_obj) {
      post_barrier(ptr, data);
    }
  }

  if (is_long) {
    dst = rlock_result(x, type);
    __ move(FrameMap::long0_opr, dst);
  }
}
