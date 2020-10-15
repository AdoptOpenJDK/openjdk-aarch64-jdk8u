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
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/universe.inline.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/icache.hpp"
#include "runtime/interfaceSupport.hpp"
#include "runtime/signature.hpp"

#define __ _masm->

/*#define print_copy(name, off) \
  __ mov(rscratch1, (address)name);\
  __ mov(rscratch2, off);\
  __ reg_printf("%s copied from offset %p + %d\n", rscratch1, from(), rscratch2);*/

#define print_copy(name, off)

// Implementation of SignatureHandlerGenerator
Register InterpreterRuntime::SignatureHandlerGenerator::from() { return rlocals; }
Register InterpreterRuntime::SignatureHandlerGenerator::to()   { return r4; }
Register InterpreterRuntime::SignatureHandlerGenerator::temp() { return rscratch1; }

void InterpreterRuntime::SignatureHandlerGenerator::pass_int() {
  print_copy(__FUNCTION__, Interpreter::local_offset_in_bytes(offset()));
  const Address src(from(), Interpreter::local_offset_in_bytes(offset()));

  switch (_num_int_args) {
  case 0:
    __ ldr(c_rarg1, src);
    _num_int_args++;
    break;
  case 1:
    __ ldr(c_rarg2, src);
    _num_int_args++;
    break;
  case 2:
    __ ldr(c_rarg3, src);
    _num_int_args++;
    break;
  default:
    __ ldr(r0, src);
    __ str(r0, Address(to(), _stack_offset));
    _stack_offset += wordSize;
    _num_int_args++;
    break;
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_long() {
  print_copy(__FUNCTION__, Interpreter::local_offset_in_bytes(offset() + 1));
  const Address src(from(), Interpreter::local_offset_in_bytes(offset() + 1));
  // Needs to be aligned to even registers. Means also won't be split across
  // registers and stack.

  switch (_num_int_args) {
  case 0:
  case 1:
    __ ldrd(c_rarg2, c_rarg3, src);
    _num_int_args = 3; // force next args onto stack
    break;
  default:
    __ ldrd(r0, temp(), src);
    _stack_offset = (_stack_offset + 7) & ~7; // Align on 8-byte boundary
    __ strd(r0, temp(), Address(to(), _stack_offset));
    _stack_offset += 2 * wordSize;
    _num_int_args += 2;
    break;
  }
}

#ifdef HARD_FLOAT_CC
void InterpreterRuntime::SignatureHandlerGenerator::pass_float() {
  print_copy(__FUNCTION__, Interpreter::local_offset_in_bytes(offset()));
  const Address src(from(), Interpreter::local_offset_in_bytes(offset()));

    if (_fp_arg_mask & ((1 << Argument::n_float_register_parameters_c*2)-1)) {
        unsigned index = __builtin_ctz(_fp_arg_mask);
        __ vldr_f32(as_FloatRegister(index), src);
        _fp_arg_mask &= ~(1 << index);
        _next_double_dex += (~index) & 1;
    } else {
        __ ldr(r0, src);
        __ str(r0, Address(to(), _stack_offset));
        _stack_offset += wordSize;
    }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_double() {
  print_copy(__FUNCTION__, Interpreter::local_offset_in_bytes(offset() + 1));
  const Address src(from(), Interpreter::local_offset_in_bytes(offset() + 1));

    if (_next_double_dex < Argument::n_float_register_parameters_c) {
        _fp_arg_mask &= ~((3 << _next_double_dex*2) | ((1 << _next_double_dex+16)));
        __ vldr_f64(as_DoubleFloatRegister(_next_double_dex++), src);
    } else {
        // make future floats allocate on stack too
        _fp_arg_mask &= ~((1 << Argument::n_float_register_parameters_c*2)-1);

        __ ldrd(r0, temp(), src);
        _stack_offset = (_stack_offset + 7) & ~7;
        __ strd(r0, temp(), Address(to(), _stack_offset));
        _stack_offset += 2 * wordSize;
    }
}
#else
// Just pass them in integer registers and on the stack as we would
// any other argument
void InterpreterRuntime::SignatureHandlerGenerator::pass_float() {
  pass_int();
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_double() {
  pass_long();
}
#endif //HARD_FLOAT_CC

void InterpreterRuntime::SignatureHandlerGenerator::pass_object() {
  print_copy(__FUNCTION__, Interpreter::local_offset_in_bytes(offset()));

  switch (_num_int_args) {
  case 0:
    assert(offset() == 0, "argument register 1 can only be (non-null) receiver");
    __ add(c_rarg1, from(), Interpreter::local_offset_in_bytes(offset()));
    _num_int_args++;
    break;
  case 1:
    {
      __ add(r0, from(), Interpreter::local_offset_in_bytes(offset()));
      __ mov(c_rarg2, 0);
      __ ldr(temp(), r0);
      Label L;
      __ cbz(temp(), L);
      __ mov(c_rarg2, r0);
      __ bind(L);
      _num_int_args++;
      break;
    }
  case 2:
    {
      __ add(r0, from(), Interpreter::local_offset_in_bytes(offset()));
      __ mov(c_rarg3, 0);
      __ ldr(temp(), r0);
      Label L;
      __ cbz(temp(), L);
      __ mov(c_rarg3, r0);
      __ bind(L);
      _num_int_args++;
      break;
    }
 default:
   {
      __ add(r0, from(), Interpreter::local_offset_in_bytes(offset()));
      __ ldr(temp(), r0);
      Label L;
      __ cbnz(temp(), L);
      __ mov(r0, 0);
      __ bind(L);
      __ str(r0, Address(to(), _stack_offset));
      _stack_offset += wordSize;
      _num_int_args++;
      break;
   }
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::generate(uint64_t fingerprint) {
  // generate code to handle arguments
  iterate(fingerprint);

  // return result handler
  __ lea(r0, ExternalAddress(Interpreter::result_handler(method()->result_type())));
  __ b(lr);

  __ flush();
}


// Implementation of SignatureHandlerLibrary

void SignatureHandlerLibrary::pd_set_handler(address handler) {}


class SlowSignatureHandler : public NativeSignatureIterator {
 private:
  address   _from;
  intptr_t* _to;
  intptr_t* _int_args;
  intptr_t* _fp_args;
  intptr_t* _fp_identifiers;

  int _num_int_reg_args;
  int _next_double_dex;

  virtual void pass_int()
  {
    jint from_obj = *(jint *)(_from+Interpreter::local_offset_in_bytes(0));
    _from -= Interpreter::stackElementSize;

    if (_num_int_reg_args < Argument::n_int_register_parameters_c-1) {
      *_int_args++ = from_obj;
      _num_int_reg_args++;
    } else {
      *_to++ = from_obj;
    }
  }

  virtual void pass_long()
  {
    intptr_t high_obj = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(0));
    intptr_t low_obj = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(1));
    _from -= 2*Interpreter::stackElementSize;

    if (_num_int_reg_args < Argument::n_int_register_parameters_c-2) {
      // Passing longs. As c_rarg0 is always reserved for jni_env we could only
      // possibly stash a long in r3:r2 due to alignment so we can only enter here
      // with either zero or one parameters.
      // Align to two
      _int_args += 1 - _num_int_reg_args; // 0 or 1
      *_int_args++ = low_obj;
      *_int_args++ = high_obj;
      _num_int_reg_args = 3;
    } else {
      _to = (intptr_t*)(((intptr_t)_to + 7) & ~7); // Align to eight bytes
      *_to++ = low_obj;
      *_to++ = high_obj;
      _num_int_reg_args = 3;
    }
  }

  virtual void pass_object()
  {
    intptr_t *from_addr = (intptr_t*)(_from + Interpreter::local_offset_in_bytes(0));
    _from -= Interpreter::stackElementSize;

    if (_num_int_reg_args < Argument::n_int_register_parameters_c-1) {
      *_int_args++ = (*from_addr == 0) ? NULL : (intptr_t)from_addr;
      _num_int_reg_args++;
    } else {
      *_to++ = (*from_addr == 0) ? NULL : (intptr_t) from_addr;
    }
  }
#ifdef HARD_FLOAT_CC
  virtual void pass_float()
  {
    jint from_obj = *(jint*)(_from+Interpreter::local_offset_in_bytes(0));
    _from -= Interpreter::stackElementSize;

    if ((*_fp_identifiers) & 0xffff) {
      unsigned index = __builtin_ctz(*_fp_identifiers);
      _fp_args[index] = from_obj;
      *_fp_identifiers ^= 1 << index;
      _next_double_dex += (~index) & 1;
    } else {
      *_to++ = from_obj;
    }
  }

  virtual void pass_double()
  {
    intptr_t high_obj = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(0));
    intptr_t low_obj = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(1));
    _from -= 2*Interpreter::stackElementSize;

    if (_next_double_dex < Argument::n_float_register_parameters_c) {
      //We can allocate to a register.
      int index = _next_double_dex++;
      *_fp_identifiers &= ~((3 << index*2) | (1 << index+16));
      _fp_args[index*2] = low_obj;
      _fp_args[index*2 + 1] = high_obj;
    } else {
      *_fp_identifiers &= ~0xffff; // make future floats allocate on stack too
      _to = (intptr_t*)(((intptr_t)_to + 7) & ~7); // Align to eight bytes
      *_to++ = low_obj;
      *_to++ = high_obj;
    }
  }
#else
  virtual void pass_float() { pass_int(); }
  virtual void pass_double() { pass_long(); }
#endif // HARD_FLOAT_CC

 public:
  SlowSignatureHandler(methodHandle method, address from, intptr_t* to)
    : NativeSignatureIterator(method)
  {
    _from = from;
    _to   = to;
    // See layout in interpreter_aarch32.cpp
    _int_args = to - (method->is_static() ? 19 : 20);
    _fp_args =  to - 16; //each slot is for a double
    _fp_identifiers = to - 21;
    *_fp_identifiers = (1 <<(Argument::n_float_register_parameters_c * 3)) - 1;

    _num_int_reg_args = (method->is_static() ? 1 : 0);
    _next_double_dex = 0;
  }
};


IRT_ENTRY(address,
          InterpreterRuntime::slow_signature_handler(JavaThread* thread,
                                                     Method* method,
                                                     intptr_t* from,
                                                     intptr_t* to))
  methodHandle m(thread, (Method*)method);
  assert(m->is_native(), "sanity check");

  // handle arguments
  SlowSignatureHandler ssh(m, (address)from, to);
  ssh.iterate(UCONST64(-1));

  // return result handler
  return Interpreter::result_handler(m->result_type());
IRT_END
