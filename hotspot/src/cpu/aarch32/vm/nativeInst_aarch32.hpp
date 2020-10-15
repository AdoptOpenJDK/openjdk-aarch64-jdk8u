/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH32_VM_NATIVEINST_AARCH32_HPP
#define CPU_AARCH32_VM_NATIVEINST_AARCH32_HPP

#include "asm/assembler.hpp"
#include "memory/allocation.hpp"
#include "runtime/icache.hpp"
#include "runtime/os.hpp"
#include "utilities/top.hpp"

// We have interfaces for the following instructions:
// - NativeInstruction
// - - NativeCall
// - - NativeMovConstReg
// - - NativeMovRegMem
// - - NativeMovRegMemPatching
// - - NativeJump
// - - NativeIllegalOpCode
// - - NativeGeneralJump
// - - NativeReturn
// - - NativeReturnX (return with argument)
// - - NativePushConst
// - - NativeTstRegMem

// The base class for different kinds of native instruction abstractions.
// Provides the primitive operations to manipulate code relative to this.

class NativeInstruction VALUE_OBJ_CLASS_SPEC {
  friend class Relocation;
  friend bool is_NativeCallTrampolineStub_at(address);
 public:
  enum { arm_insn_sz = 4 };

  inline bool is_nop();
  inline bool is_barrer();
  inline bool is_illegal();
  inline bool is_return();
  inline bool is_jump_or_nop();
  inline bool is_cond_jump();
  bool is_safepoint_poll();
  bool is_movt();
  bool is_orr();
  bool is_sigill_zombie_not_entrant();

  bool is_movt(Register dst, unsigned imm, Assembler::Condition cond = Assembler::C_DFLT);
  bool is_movw(Register dst, unsigned imm, Assembler::Condition cond = Assembler::C_DFLT);
  bool is_ldr(Register dst, Address addr, Assembler::Condition cond = Assembler::C_DFLT);

  inline bool is_jump() const;
  inline bool is_call() const;

  inline bool is_mov_const_reg() const;
  inline bool is_reg_call() const;
  inline bool is_imm_call() const;
  inline bool is_reg_jump() const;
  inline bool is_imm_jump() const;

 protected:
  address addr() const { return address(this); }
  // TODO remove this, every command is 4byte long
#if 1
  address addr_at(int offset) const    { return addr() + offset; }

  s_char sbyte_at(int offset) const    { return *(s_char*) addr_at(offset); }
  u_char ubyte_at(int offset) const    { return *(u_char*) addr_at(offset); }

  jint int_at(int offset) const        { return *(jint*) addr_at(offset); }
  juint uint_at(int offset) const      { return *(juint*) addr_at(offset); }

  address ptr_at(int offset) const     { return *(address*) addr_at(offset); }

  oop  oop_at (int offset) const       { return *(oop*) addr_at(offset); }


  void set_char_at(int offset, char c)        { *addr_at(offset) = (u_char)c; }
  void set_int_at(int offset, jint  i)        { *(jint*)addr_at(offset) = i; }
  void set_uint_at(int offset, jint  i)       { *(juint*)addr_at(offset) = i; }
  void set_ptr_at (int offset, address  ptr)  { *(address*) addr_at(offset) = ptr; }
  void set_oop_at (int offset, oop  o)        { *(oop*) addr_at(offset) = o; }
#endif

  static juint as_uint(address addr) {
    return *(juint *) addr;
  }

  juint as_uint() const {
    return as_uint(addr());
  }

  void set_uint(juint v) {
    *(juint *) addr() = v;
  }

  void atomic_set_ulong_at(int offset, julong v) {
    address a = addr() + offset;
    assert(((uintptr_t) a) % 8 == 0, "should be aligned");
    Atomic::store(v, (volatile jlong *) a);
  }

 public:

  // unit test stuff
  static void test() {}                 // override for testing

  static bool is_at(address address);
  static NativeInstruction* from(address address);

};

inline NativeInstruction* nativeInstruction_at(address addr) {
  return NativeInstruction::from(addr);
}

inline NativeInstruction* nativeInstruction_at(uint32_t *addr) {
  return NativeInstruction::from(address(addr));
}

class NativeBranchType: public NativeInstruction {
 protected:
  static bool is_branch_type(uint32_t insn);
  void patch_offset_to(address addr);
 public:
  enum {
    instruction_size = arm_insn_sz,
  };

  address next_instruction_address() const {
    return addr() + arm_insn_sz;
  }
};

class NativeFarLdr: public NativeInstruction {
 private:
   static address skip_patching_prolog(address addr);
 public:
   static bool is_at(address addr);
   static NativeFarLdr* from(address addr);
   intptr_t *data_addr();
   void set_data_addr(intptr_t *data_addr);
   address next_instruction_address() const;
};

class NativeMovConstReg: public NativeInstruction {
  friend class Relocation;
  friend class NativeMovRegMem;
  friend class NativeGeneralJump;
  friend class NativeFarLdr;

 protected:
  static bool is_ldr_literal_at(address instr, Register from = r15_pc);
  static bool is_far_ldr_literal_at(address instr);
  static bool is_movw_movt_at(address instr);
  static bool is_mov_n_three_orr_at(address instr);
 public:
  enum {
    ldr_sz             = 1 * arm_insn_sz,
    far_ldr_sz         = 2 * arm_insn_sz,
    movw_movt_pair_sz  = 2 * arm_insn_sz,
    mov_n_three_orr_sz = 4 * arm_insn_sz,
    min_instruction_size = 1 * arm_insn_sz,
    max_instruction_size = 4 * arm_insn_sz,
  };

  address next_instruction_address() const  {
    if (is_ldr_literal_at(addr())) {
      return addr() + ldr_sz;
    } else if (is_far_ldr_literal_at(addr())) {
      return NativeFarLdr::from(addr())->next_instruction_address();;
    } else if (is_movw_movt_at(addr())) {
      return addr() + movw_movt_pair_sz;
    } else if (is_mov_n_three_orr_at(addr())) {
      return addr() + mov_n_three_orr_sz;
    }

    // Unknown instruction in NativeMovConstReg
    ShouldNotReachHere();
    return NULL;
  }

  intptr_t data() const;
  void set_data(intptr_t x);

  Register destination() const;
  void set_destination(Register r);

  void flush() {
    ICache::invalidate_range(addr(), max_instruction_size);
  }

  void  verify();
  void  print();

  // unit test stuff
  static void test() {}

  // Creation
  inline friend NativeMovConstReg* nativeMovConstReg_at(address address);

  static NativeMovConstReg* before(address addr) {
    address mov = NULL;
    if (is_ldr_literal_at(addr - ldr_sz)) {
      mov = addr - ldr_sz;
    } else if (is_far_ldr_literal_at(addr - far_ldr_sz)) {
      mov = addr - far_ldr_sz;
    } else if (is_movw_movt_at(addr - movw_movt_pair_sz)) {
      mov = addr - movw_movt_pair_sz;
    } else if (is_mov_n_three_orr_at(addr - mov_n_three_orr_sz)) {
      mov = addr - mov_n_three_orr_sz;
    }
    guarantee(mov, "Can't find NativeMovConstReg before");
    return NativeMovConstReg::from(mov);
  }

  static bool is_at(address instr);
  static NativeMovConstReg* from(address addr);
};

inline NativeMovConstReg* nativeMovConstReg_at(address address) {
  return NativeMovConstReg::from(address);
}

class NativeTrampolineCall: public NativeInstruction {
 public:
  // NativeTrampolineCall size is always equal to NativeCall::instruction_size
  address destination() const;
  void set_destination(address dest);
  void set_destination_mt_safe(address dest, bool assert_lock = true);

  static bool is_at(address address);
  static NativeTrampolineCall* from(address address);

  address next_instruction_address() const;
};

class NativeRegCall: public NativeBranchType {
 public:

  Register destination() const;
  void set_destination(Register r);

  static bool is_at(address address);
  static NativeRegCall* from(address address);
};

class NativeCall: public NativeInstruction {
  friend class Relocation;
 protected:
  NativeInstruction* is_long_jump_or_call_at(address addr);

  // NativeCall represents:
  //  NativeImmCall,
  //  NativeMovConstReg + NativeBranchType,
  //  NativeTrampolineCall
 public:
  enum {
    max_instruction_size = 5 * arm_insn_sz
  };

  static int instruction_size;
#ifdef ASSERT
  StaticAssert<NativeMovConstReg::movw_movt_pair_sz
      + NativeRegCall::instruction_size <= (int) max_instruction_size> dummy2;
  StaticAssert<NativeMovConstReg::mov_n_three_orr_sz
      + NativeRegCall::instruction_size <= (int) max_instruction_size> dummy3;
#endif

  address destination() const;
  void set_destination(address dest);

  static void init();
  void  verify_alignment()                       { ; }
  void  verify();
  void  print();

  address instruction_address() const       { return addr_at(0); }
  address next_instruction_address() const;
  address return_address() const;

  // MT-safe patching of a call instruction.
  static void insert(address code_pos, address entry);

  // Similar to replace_mt_safe, but just changes the destination.  The
  // important thing is that free-running threads are able to execute
  // this call instruction at all times.  If the call is an immediate BL
  // instruction we can simply rely on atomicity of 32-bit writes to
  // make sure other threads will see no intermediate states.

  // We cannot rely on locks here, since the free-running threads must run at
  // full speed.
  //
  // Used in the runtime linkage of calls; see class CompiledIC.
  // (Cf. 4506997 and 4479829, where threads witnessed garbage displacements.)

  // The parameter assert_lock disables the assertion during code generation.
  void set_destination_mt_safe(address dest, bool assert_lock = true);

  static bool is_at(address instr);
  static NativeCall* from(address instr);

  static bool is_call_before(address return_address);
};

inline address NativeTrampolineCall::next_instruction_address() const {
  assert(is_at(addr()), "not call");
  return addr() + NativeCall::instruction_size;
}

inline NativeCall* nativeCall_at(address address) {
  return NativeCall::from(address);
}

// An interface for accessing/manipulating native moves of the form:
//      mov[b/w/l/q] [reg + offset], reg   (instruction_code_reg2mem)
//      mov[b/w/l/q] reg, [reg+offset]     (instruction_code_mem2reg
//      mov[s/z]x[w/b/q] [reg + offset], reg
//      fld_s  [reg+offset]
//      fld_d  [reg+offset]
//      fstp_s [reg + offset]
//      fstp_d [reg + offset]
//      mov_literal64  scratch,<pointer> ; mov[b/w/l/q] 0(scratch),reg | mov[b/w/l/q] reg,0(scratch)
//
// Warning: These routines must be able to handle any instruction sequences
// that are generated as a result of the load/store byte,word,long
// macros.  For example: The load_unsigned_byte instruction generates
// an xor reg,reg inst prior to generating the movb instruction.  This
// class must skip the xor instruction.


// TODO Review
class NativeMovRegMem: public NativeInstruction {
 public:
  enum {
    instruction_size = 2 * arm_insn_sz, // TODO check this
  };
  // helper
  int instruction_start() const;

  address instruction_address() const;

  address next_instruction_address() const;

  int   offset() const;

  void  set_offset(int x);

  void  add_offset_in_bytes(int add_offset)     { set_offset ( ( offset() + add_offset ) ); }

  void verify();
  void print ();

  // unit test stuff
  static void test() {}

 private:
  inline friend NativeMovRegMem* nativeMovRegMem_at (address address);
};

inline NativeMovRegMem* nativeMovRegMem_at (address address) {
  NativeMovRegMem* test = (NativeMovRegMem*) address;
#ifdef ASSERT
  test->verify();
#endif
  return test;
}

class NativeMovRegMemPatching: public NativeMovRegMem {
 private:
  friend NativeMovRegMemPatching* nativeMovRegMemPatching_at (address address) {Unimplemented(); return 0;  }
};

class NativeJump: public NativeInstruction {
 public:
  enum {
    instruction_size = NativeMovConstReg::movw_movt_pair_sz + NativeBranchType::instruction_size,
  };
  address instruction_address() const {
    return addr();
  }

  address next_instruction_address() const;

  address jump_destination() const;
  void set_jump_destination(address dest);

  // Creation
  inline friend NativeJump* nativeJump_at(address address);

  void verify();

  // Unit testing stuff
  static void test() {}

  // Insertion of native jump instruction
  static void insert(address code_pos, address entry);
  // MT-safe insertion of native jump at verified method entry
  static void check_verified_entry_alignment(address entry, address verified_entry);
  static void patch_verified_entry(address entry, address verified_entry, address dest);

  static bool is_at(address instr);
  static NativeJump* from(address instr);
};

inline NativeJump* nativeJump_at(address addr) {
  return NativeJump::from(addr);
}

// TODO We don't really need NativeGeneralJump, NativeJump should be able to do
// everything that General Jump would.  Make this only interface to NativeJump
// from share code (c1_Runtime)
class NativeGeneralJump: public NativeJump {
public:
  enum {
    instruction_size = arm_insn_sz,
  };

  static void insert_unconditional(address code_pos, address entry);
  static void replace_mt_safe(address instr_addr, address code_buffer);
  static void verify();
};

inline NativeGeneralJump* nativeGeneralJump_at(address address) {
  NativeGeneralJump* jump = (NativeGeneralJump*)(address);
  debug_only(jump->verify();)
  return jump;
}

class NativePopReg : public NativeInstruction {
 public:
  // Insert a pop instruction
  static void insert(address code_pos, Register reg);
};


class NativeIllegalInstruction: public NativeInstruction {
 public:
  // Insert illegal opcode as specific address
  static void insert(address code_pos);
};

// return instruction that does not pop values of the stack
class NativeReturn: public NativeInstruction {
 public:
};

// return instruction that does pop values of the stack
class NativeReturnX: public NativeInstruction {
 public:
};

// Simple test vs memory
class NativeTstRegMem: public NativeInstruction {
 public:
};

inline bool NativeInstruction::is_nop()         {
  return (as_uint() & 0x0fffffff) == 0x0320f000;
}

inline bool NativeInstruction::is_barrer()         {
  return (as_uint() == 0xf57ff05b /* dmb ish */ ||
            as_uint() == 0xee070fba /* mcr 15, 0, r0, cr7, cr10, {5}) */);
}

inline bool NativeInstruction::is_jump_or_nop() {
  return is_nop() || is_jump();
}

class NativeImmCall: public NativeBranchType {
 public:
  address destination() const;
  void set_destination(address dest);

  static bool is_at(address address);
  static NativeImmCall* from(address address);
};

class NativeImmJump: public NativeBranchType {
 public:

  address destination() const;
  void set_destination(address r);

  static bool is_at(address address);
  static NativeImmJump* from(address address);
};

class NativeRegJump: public NativeBranchType {
 public:

  Register destination() const;
  void set_destination(Register r);

  static bool is_at(address address);
  static NativeRegJump* from(address address);
};

inline bool NativeInstruction::is_call() const          { return NativeCall::is_at(addr()); }
inline bool NativeInstruction::is_jump() const          { return NativeJump::is_at(addr()); }
inline bool NativeInstruction::is_mov_const_reg() const { return NativeMovConstReg::is_at(addr()); }
inline bool NativeInstruction::is_imm_call() const      { return NativeImmCall::is_at(addr()); }
inline bool NativeInstruction::is_reg_call() const      { return NativeRegCall::is_at(addr()); }
inline bool NativeInstruction::is_imm_jump() const      { return NativeImmJump::is_at(addr()); }
inline bool NativeInstruction::is_reg_jump() const      { return NativeRegJump::is_at(addr()); }

inline NativeCall* nativeCall_before(address return_address) {
  if (NativeTrampolineCall::is_at(return_address - NativeCall::instruction_size)) {
    return NativeCall::from(return_address - NativeCall::instruction_size);
  }
  if (NativeMovConstReg::is_at(return_address - NativeCall::instruction_size)) {
    NativeMovConstReg *nm = NativeMovConstReg::from(return_address - NativeCall::instruction_size);
    address next_instr = nm->next_instruction_address();
    if (NativeRegCall::is_at(next_instr) &&
            NativeRegCall::from(next_instr)->destination() == nm->destination()) {
      return NativeCall::from(return_address - NativeCall::instruction_size);
    }
  }
  if (NativeImmCall::is_at(return_address - NativeBranchType::instruction_size)) {
    return NativeCall::from(return_address - NativeBranchType::instruction_size);
  }

  ShouldNotReachHere();
  return NULL;
}

#endif // CPU_AARCH32_VM_NATIVEINST_AARCH32_HPP
