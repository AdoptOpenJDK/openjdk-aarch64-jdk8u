/*
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH32_VM_REGISTER_AARCH32_HPP
#define CPU_AARCH32_VM_REGISTER_AARCH32_HPP

#include "asm/register.hpp"

class VMRegImpl;
typedef VMRegImpl* VMReg;

// Implementation of integer registers for AArch32 architecture

class RegisterImpl;
typedef RegisterImpl* Register;

inline Register as_Register(int encoding) {
  return (Register)(intptr_t) encoding;
}

class RegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = 16
  };

  // Construction
  inline friend Register as_Register(int encoding);

  // Accessors
  int encoding() const {
    assert(is_valid(), "invalid register");
    return (intptr_t) this;
  }
  int encoding_nocheck() const {
    return (intptr_t) this;
  }
  VMReg as_VMReg();
  Register successor() const {
    return as_Register(encoding() + 1);
  }

  // Testers
  bool is_valid() const {
    return 0 <= (intptr_t) this && (intptr_t) this < number_of_registers;
  }

  // Return the bit which represents this register. This is intended to be
  // used in bitmasks. See RegSet class below.
  unsigned long bit(bool should_set = true) const {
    return should_set ? 1 << encoding() : 0;
  }

  // Return the name of this register
  const char* name() const;
};

// Integer registers of AArch32 architecture

CONSTANT_REGISTER_DECLARATION(Register, noreg, -1);

CONSTANT_REGISTER_DECLARATION(Register, r0,     0);
CONSTANT_REGISTER_DECLARATION(Register, r1,     1);
CONSTANT_REGISTER_DECLARATION(Register, r2,     2);
CONSTANT_REGISTER_DECLARATION(Register, r3,     3);
CONSTANT_REGISTER_DECLARATION(Register, r4,     4);
CONSTANT_REGISTER_DECLARATION(Register, r5,     5);
CONSTANT_REGISTER_DECLARATION(Register, r6,     6);
CONSTANT_REGISTER_DECLARATION(Register, r7,     7);
CONSTANT_REGISTER_DECLARATION(Register, r8,     8);
CONSTANT_REGISTER_DECLARATION(Register, r9,     9);
CONSTANT_REGISTER_DECLARATION(Register, r10,   10);
CONSTANT_REGISTER_DECLARATION(Register, r11,   11);
CONSTANT_REGISTER_DECLARATION(Register, r12,   12);
CONSTANT_REGISTER_DECLARATION(Register, r13,   13);
CONSTANT_REGISTER_DECLARATION(Register, r14,   14);
CONSTANT_REGISTER_DECLARATION(Register, r15,   15);

// Implementation of floating point registers for AArch32 (VFPv3-D16)
// architecture

class FloatRegisterImpl;
typedef FloatRegisterImpl* FloatRegister;

// Return FloatRegister corresponding to the given s-type (aka f-type in this
// port) register number
inline FloatRegister as_FloatRegister(int encoding) {
  return (FloatRegister)(intptr_t) encoding;
}

// Return FloatRegister corresponding to the given d-type register number
inline FloatRegister as_DoubleFloatRegister(int encoding) {
  return as_FloatRegister(2 * encoding);
}

class FloatRegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    // VFPv3-D16 architecture includes 16 doubleword registers, which can be
    // also observed as 32 singleword registers. We count the singleword
    // registers here.
    number_of_registers = 32
  };

  enum FloatRegisterSize {
    SINGLE = 1,
    DOUBLE = 2,
    QUAD = 4
  };

  // Construction
  inline friend FloatRegister as_FloatRegister(int encoding);
  inline friend FloatRegister as_DoubleFloatRegister(int encoding);

  // Accessors
  int encoding() const {
    assert(is_valid(), "invalid register");
    return (intptr_t) this;
  }
  int encoding_nocheck() const {
    return (intptr_t) this;
  }
  VMReg as_VMReg();
  FloatRegister successor(enum FloatRegisterSize size) const {
    return (as_FloatRegister((encoding() + (int)size) % number_of_registers |
            (encoding() + (int)size) / number_of_registers));
  }

  // Testers
  bool is_valid() const {
    return 0 <= (intptr_t) this && (intptr_t) this < number_of_registers;
  }

  // Return the bit which represents this register. This is intended to be
  // used in bitmasks. See FloatRegSet class below.
  unsigned long bit(bool should_set = true) const {
    return should_set ? 1 << encoding() : 0;
  }

  // Return the name of this register
  const char* name() const;
};

// Floating point registers of AArch32 (VFPv3-D16, D32 and SIMD) architecture

// Only the first 8 doubleword registers can be used for parameter passing
// and thus are caller-saved. The rest 8 registers are callee-saved.
// In VFPv3-D32 there are additional 16 doubleword registers that are
// caller-saved again.

// Here we introduce the symbolic names for doubleword registers and the
// corresponding singleword views for the first 16 of them. The instruction
// set allows us to encode the doubleword register numbers directly using
// the constants below.

// The respective names are as well defined for quad-word registers with
// encoding set by the same principles.

CONSTANT_REGISTER_DECLARATION(FloatRegister, fnoreg, -1);

CONSTANT_REGISTER_DECLARATION(FloatRegister, d0,      0);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d1,      2);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d2,      4);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d3,      6);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d4,      8);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d5,     10);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d6,     12);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d7,     14);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d8,     16);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d9,     18);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d10,    20);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d11,    22);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d12,    24);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d13,    26);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d14,    28);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d15,    30);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d16,     1);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d17,     3);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d18,     5);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d19,     7);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d20,     9);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d21,    11);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d22,    13);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d23,    15);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d24,    17);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d25,    19);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d26,    21);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d27,    23);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d28,    25);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d29,    27);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d30,    29);
CONSTANT_REGISTER_DECLARATION(FloatRegister, d31,    31);

CONSTANT_REGISTER_DECLARATION(FloatRegister, q0,      0);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q1,      4);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q2,      8);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q3,     12);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q4,     16);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q5,     20);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q6,     24);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q7,     28);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q8,      1);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q9,      5);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q10,     9);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q11,    13);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q12,    17);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q13,    21);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q14,    25);
CONSTANT_REGISTER_DECLARATION(FloatRegister, q15,    29);

CONSTANT_REGISTER_DECLARATION(FloatRegister, f0,      0);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f1,      1);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f2,      2);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f3,      3);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f4,      4);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f5,      5);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f6,      6);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f7,      7);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f8,      8);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f9,      9);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f10,    10);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f11,    11);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f12,    12);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f13,    13);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f14,    14);
CONSTANT_REGISTER_DECLARATION(FloatRegister, f15,    15);

// Total number of registers of all sorts

class ConcreteRegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    // Here we count the total number of 32-bit slots available in registers.
    // This number must be large enough to cover REG_COUNT (defined by C2)
    // registers. There is no requirement that any ordering here matches
    // any ordering C2 gives its OptoReg's.
    number_of_registers = RegisterImpl::number_of_registers +
                          FloatRegisterImpl::number_of_registers
  };

  static const int max_gpr;
  static const int max_fpr;
};

// Set of integer registers

class RegSet {
 private:
  uint32_t _bitset;

  RegSet(uint32_t bitset) : _bitset(bitset) { }

 public:
  RegSet() : _bitset(0) { }

  RegSet(Register r1) : _bitset(r1->bit()) { }

  RegSet operator+(const RegSet aSet) const {
    RegSet result(_bitset | aSet._bitset);
    return result;
  }

  RegSet operator-(const RegSet aSet) const {
    RegSet result(_bitset & ~aSet._bitset);
    return result;
  }

  RegSet& operator+=(const RegSet aSet) {
    *this = *this + aSet;
    return *this;
  }

  static RegSet of(Register r1) {
    return RegSet(r1);
  }

  static RegSet of(Register r1, Register r2) {
    return of(r1) + r2;
  }

  static RegSet of(Register r1, Register r2, Register r3) {
    return of(r1, r2) + r3;
  }

  static RegSet of(Register r1, Register r2, Register r3, Register r4) {
    return of(r1, r2, r3) + r4;
  }

  static RegSet of(Register r1, Register r2, Register r3, Register r4, Register r5) {
    return of(r1, r2, r3, r4) + r5;
  }

  static RegSet range(Register start, Register end) {
    uint32_t bits = ~0;
    bits <<= start->encoding();
    bits <<= 31 - end->encoding();
    bits >>= 31 - end->encoding();
    return RegSet(bits);
  }

  uint32_t bits() const {
    return _bitset;
  }
};

// Set of singleword floating point registers

class FloatRegSet {
 private:
  uint32_t _bitset;

  FloatRegSet(uint32_t bitset) : _bitset(bitset) { }

 public:
  FloatRegSet() : _bitset(0) { }

  FloatRegSet(FloatRegister r1) : _bitset(r1->bit()) { }

  FloatRegSet operator+(const FloatRegSet aSet) const {
    FloatRegSet result(_bitset | aSet._bitset);
    return result;
  }

  FloatRegSet operator-(const FloatRegSet aSet) const {
    FloatRegSet result(_bitset & ~aSet._bitset);
    return result;
  }

  FloatRegSet& operator+=(const FloatRegSet aSet) {
    *this = *this + aSet;
    return *this;
  }

  static FloatRegSet of(FloatRegister r1) {
    return FloatRegSet(r1);
  }

  static FloatRegSet of(FloatRegister r1, FloatRegister r2) {
    return of(r1) + r2;
  }

  static FloatRegSet of(FloatRegister r1, FloatRegister r2, FloatRegister r3) {
    return of(r1, r2) + r3;
  }

  static FloatRegSet of(FloatRegister r1, FloatRegister r2, FloatRegister r3,
                        FloatRegister r4) {
    return of(r1, r2, r3) + r4;
  }

  static FloatRegSet range(FloatRegister start, FloatRegister end) {
    uint32_t bits = ~0;
    bits <<= start->encoding();
    bits <<= 31 - end->encoding();
    bits >>= 31 - end->encoding();
    return FloatRegSet(bits);
  }

  uint32_t bits() const {
    return _bitset;
  }
};

// Set of doubleword floating point registers

class DoubleFloatRegSet {
 private:
  uint32_t _bitset;

  DoubleFloatRegSet(uint32_t bitset) : _bitset(bitset) { }

 public:
  DoubleFloatRegSet() : _bitset(0) { }

  DoubleFloatRegSet(FloatRegister r1) : _bitset(1 << (r1->encoding() >> 1)) { }

  DoubleFloatRegSet operator+(const DoubleFloatRegSet aSet) const {
    DoubleFloatRegSet result(_bitset | aSet._bitset);
    return result;
  }

  DoubleFloatRegSet operator-(const DoubleFloatRegSet aSet) const {
    DoubleFloatRegSet result(_bitset & ~aSet._bitset);
    return result;
  }

  DoubleFloatRegSet& operator+=(const DoubleFloatRegSet aSet) {
    *this = *this + aSet;
    return *this;
  }

  static DoubleFloatRegSet of(FloatRegister r1) {
    return DoubleFloatRegSet(r1);
  }

  static DoubleFloatRegSet of(FloatRegister r1, FloatRegister r2) {
    return of(r1) + r2;
  }

  static DoubleFloatRegSet of(FloatRegister r1, FloatRegister r2,
                              FloatRegister r3) {
    return of(r1, r2) + r3;
  }

  static DoubleFloatRegSet of(FloatRegister r1, FloatRegister r2,
                              FloatRegister r3, FloatRegister r4) {
    return of(r1, r2, r3) + r4;
  }

  static DoubleFloatRegSet range(FloatRegister start, FloatRegister end) {
    uint32_t bits = ~0;
    bits <<= start->encoding() >> 1;
    bits <<= 31 - (end->encoding() >> 1);
    bits >>= 31 - (end->encoding() >> 1);
    return DoubleFloatRegSet(bits);
  }

  uint32_t bits() const {
    return _bitset;
  }
};

#endif // CPU_AARCH32_VM_REGISTER_AARCH32_HPP
