/*
 * Copyright (c) 1997, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "code/codeCache.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_aarch32.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/ostream.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

// LIRAssembler fills patching site with nops up to NativeCall::instruction_size
int NativeCall::instruction_size = 5 * arm_insn_sz;

NativeInstruction* NativeInstruction::from(address addr) {
  return (NativeInstruction*) addr;
}

//-------------------------------------------------------------------

void NativeCall::init() {
  instruction_size = (VM_Version::features() & (FT_ARMV6T2 | FT_ARMV7) ? 3 : 5) * arm_insn_sz;
}

void NativeCall::verify() {
  if (!is_call()) {
    fatal("not a call");
  }
}

address NativeCall::destination() const {
  assert(is_call(), "not a call");
  if (NativeImmCall::is_at(addr())) {
    return NativeImmCall::from(addr())->destination();
  } else if (NativeMovConstReg::is_at(addr())) {
    return address(NativeMovConstReg::from(addr())->data());
  } else if (NativeTrampolineCall::is_at(addr())) {
    return NativeTrampolineCall::from(addr())->destination();
  }
  ShouldNotReachHere();
  return NULL;
}

void NativeCall::set_destination(address dest) {
  assert(is_call(), "not a call");
  if (NativeImmCall::is_at(addr())) {
    NativeImmCall::from(addr())->set_destination(dest);
  } else if (NativeMovConstReg::is_at(addr())) {
    NativeMovConstReg::from(addr())->set_data((uintptr_t) dest);
  } else if (NativeTrampolineCall::is_at(addr())) {
    NativeTrampolineCall::from(addr())->set_destination(dest);
  } else {
    ShouldNotReachHere();
  }
}

void NativeCall::set_destination_mt_safe(address dest, bool assert_lock) {
  assert(is_call(), "not a call");

  // patching should be not only safe (i.e. this call could be executed by some thread),
  // but it also should be atomic (some other thread could call NativeCall::destination()
  // and see valid destination value)

  if (NativeImmCall::is_at(addr())) {
    NativeImmCall::from(addr())->set_destination(dest);
    ICache::invalidate_word(addr());
  } else if (NativeTrampolineCall::is_at(addr())) {
    NativeTrampolineCall::from(addr())->set_destination_mt_safe(dest);
  } else {
    ShouldNotReachHere();
  }
}

void NativeCall::insert(address code_pos, address entry) {
  Unimplemented();
}

bool NativeCall::is_call_before(address return_address) {
  if (NativeTrampolineCall::is_at(return_address - NativeCall::instruction_size)) {
    return true;
  } else if (NativeMovConstReg::is_at(return_address - NativeCall::instruction_size)) {
    NativeMovConstReg *nm = NativeMovConstReg::from(return_address - NativeCall::instruction_size);
    address next_instr = nm->next_instruction_address();
    return NativeRegCall::is_at(next_instr) && NativeRegCall::from(next_instr)->destination() == nm->destination();
  } else if (NativeImmCall::is_at(return_address - NativeBranchType::instruction_size)) {
    return true;
  }
  return false;
}

address NativeCall::next_instruction_address() const {
  assert(is_call(), "not a call");
  if (NativeImmCall::is_at(addr())) {
    return NativeImmCall::from(addr())->next_instruction_address();
  } else if (NativeMovConstReg::is_at(addr())) {
    NativeMovConstReg *nm = NativeMovConstReg::from(addr());
    address next_instr = nm->next_instruction_address();
    assert(NativeRegCall::is_at(next_instr), "should be");
    return NativeRegCall::from(next_instr)->next_instruction_address();
  } else if (NativeTrampolineCall::is_at(addr())) {
    return NativeTrampolineCall::from(addr())->next_instruction_address();
  } else {
    ShouldNotReachHere();
    return NULL;
  }
}

address NativeCall::return_address() const {
  return next_instruction_address();
}

bool NativeCall::is_at(address addr) {
  if (NativeImmCall::is_at(addr)) {
    return true;
  } else if (NativeMovConstReg::is_at(addr)) {
    NativeMovConstReg *nm = NativeMovConstReg::from(addr);
    address next_instr = nm->next_instruction_address();
    return NativeRegCall::is_at(next_instr) &&
      NativeRegCall::from(next_instr)->destination() == nm->destination();
  } else if (NativeTrampolineCall::is_at(addr)) {
    return true;
  }
  return false;
}

NativeCall* NativeCall::from(address addr) {
  assert(NativeCall::is_at(addr), "");
  return (NativeCall*) addr;
}

//-------------------------------------------------------------------

address NativeTrampolineCall::destination() const {
  assert(is_at(addr()), "not call");
  return (address) uint_at(8);
}

void NativeTrampolineCall::set_destination(address dest) {
  assert(is_at(addr()), "not call");
  set_uint_at(8, (uintptr_t) dest);
}

void NativeTrampolineCall::set_destination_mt_safe(address dest, bool assert_lock) {
  assert(is_at(addr()), "not call");
  set_destination(dest);
  // FIXME invalidate data cache
}

bool NativeTrampolineCall::is_at(address addr) {
  return (as_uint(addr    ) & ~0xffu) == 0xe28fe000 // add     lr, pc, #disp
       && as_uint(addr + 4)          == 0xe51ff004; // ldr     pc, [pc, -4]
}

NativeTrampolineCall* NativeTrampolineCall::from(address addr) {
  assert(NativeTrampolineCall::is_at(addr), "");
  return (NativeTrampolineCall*) addr;
}

//-------------------------------------------------------------------

address NativeImmCall::destination() const {
  assert(is_imm_call(), "not call");
  uint32_t insn = as_uint();
  intptr_t off = Instruction_aarch32::sextract(insn, 23, 0);
  address destination = addr() + 8 + (off << 2);
  return destination;
}

void NativeImmCall::set_destination(address dest) {
  assert(is_imm_call(), "not call");
  patch_offset_to(dest);
}

bool NativeImmCall::is_at(address addr) {
  return Instruction_aarch32::extract(as_uint(addr), 27, 24)  == 0b1011;
}

NativeImmCall* NativeImmCall::from(address addr) {
  assert(NativeImmCall::is_at(addr), "");
  return (NativeImmCall*) addr;
}

//-------------------------------------------------------------------

Register NativeRegCall::destination() const {
  assert(is_reg_call(), "not call");
  return (Register) Instruction_aarch32::extract(as_uint(), 3, 0);
}

bool NativeRegCall::is_at(address addr) {
  unsigned insn = as_uint(addr);
  return is_branch_type(insn) && Instruction_aarch32::extract(insn, 7, 4) == 0b0011;
}

NativeRegCall* NativeRegCall::from(address addr) {
  assert(NativeRegCall::is_at(addr), "");
  return (NativeRegCall*) addr;
}

//-------------------------------------------------------------------

address NativeFarLdr::skip_patching_prolog(address addr) {
  if (NativeInstruction::from(addr)->is_nop() &&
      NativeInstruction::from(addr + arm_insn_sz)->is_barrer()) {
    return addr+2*arm_insn_sz;
  }
  return addr;
}

bool NativeFarLdr::is_at(address addr) {
  addr = skip_patching_prolog(addr);
  unsigned add_condidate = as_uint(addr);
  if (((Instruction_aarch32::extract(add_condidate, 27, 21)  != 0b0010100) /*add*/ &&
        (Instruction_aarch32::extract(add_condidate, 27, 21) != 0b0010010) /*sub*/) ||
      (Instruction_aarch32::extract(add_condidate, 19, 16) != (unsigned) r15_pc->encoding())) {
    return false;
  }
  Register dest = as_Register(Instruction_aarch32::extract(add_condidate, 15, 12));
  return NativeMovConstReg::is_ldr_literal_at(addr + arm_insn_sz, dest);
}

NativeFarLdr* NativeFarLdr::from(address addr) {
  assert(is_at(addr), "");
  return (NativeFarLdr*) addr;
}

intptr_t* NativeFarLdr::data_addr() {
  address self = skip_patching_prolog(addr());
  off_t offset = 8;
  off_t add_off = Assembler::decode_imm12(as_uint(self) & 0xfff);
  if (Instruction_aarch32::extract(as_uint(self), 24, 21) == 0x4) {
    offset += add_off;
  } else {
    offset -= add_off;
  }
  off_t ldr_off = as_uint(self + arm_insn_sz) & 0xfff;
  if (Instruction_aarch32::extract(as_uint(self), 23, 23)) {
    offset += ldr_off;
  } else {
    offset -= ldr_off;
  }

  return (intptr_t*)(self + offset);
}

void NativeFarLdr::set_data_addr(intptr_t *data_addr) {
  address self = skip_patching_prolog(addr());
  off_t offset = (address)data_addr - (self + 8);
  bool minus = false;
  if (offset < 0) {
    offset = -offset;
    minus = true;
  }
  guarantee((0 <= offset) && (offset <= 0xffffff), "offset too large");
  set_uint_at(self - addr(), (as_uint(self) & ~0xc00fff) |
    (minus ? 0x400000u /*sub*/ : 0x800000u /*add*/) |
    Assembler::encode_imm12(offset & 0xff000));

  set_uint_at(self - addr() + arm_insn_sz,
      (as_uint(self + arm_insn_sz) & ~0x800fff) |
      (minus ? 0x000000 : 0x800000) |
      (offset & 0xfff));
  ICache::invalidate_range(self, 2*arm_insn_sz);
}

address NativeFarLdr::next_instruction_address() const {
  return skip_patching_prolog(addr()) + NativeMovConstReg::far_ldr_sz;
}

//-------------------------------------------------------------------

void NativeMovConstReg::verify() {
  if (!is_mov_const_reg()) {
    fatal("not a mov const reg");
  }
}

intptr_t NativeMovConstReg::data() const {
  if (NativeFarLdr::is_at(addr())) {
    return *NativeFarLdr::from(addr())->data_addr();
  }
  return (intptr_t) MacroAssembler::target_addr_for_insn(addr());
}

void NativeMovConstReg::set_data(intptr_t x) {
  if (NativeFarLdr::is_at(addr())) {
    *NativeFarLdr::from(addr())->data_addr() = x;
    // Fences should be provided by calling code!
  } else {
    MacroAssembler::pd_patch_instruction(addr(), (address)x);
    ICache::invalidate_range(addr(), max_instruction_size);
  }
}

void NativeMovConstReg::print() {
  tty->print_cr(PTR_FORMAT ": mov reg, " INTPTR_FORMAT,
                p2i(addr()), data());
}

Register NativeMovConstReg::destination() const {
  return (Register) Instruction_aarch32::extract(as_uint(), 15, 12);
}

NativeMovConstReg* NativeMovConstReg::from(address addr) {
  assert(NativeMovConstReg::is_at(addr), "");
  return (NativeMovConstReg*) addr;
}

bool NativeMovConstReg::is_ldr_literal_at(address addr, Register from) {
  unsigned insn = as_uint(addr);
  if (from == noreg) {
    return (Instruction_aarch32::extract(insn, 27, 20) & 0b11100101) == 0b01000001;
  }
  unsigned reg = from->encoding();
  return (Instruction_aarch32::extract(insn, 27, 16) & 0b111001011111) == (0b010000010000 | reg);
}

bool NativeMovConstReg::is_far_ldr_literal_at(address addr) {
  return NativeFarLdr::is_at(addr);
}

bool NativeMovConstReg::is_movw_movt_at(address addr) {
  unsigned insn = as_uint(addr);
  unsigned insn2 = as_uint(addr + arm_insn_sz);
  return Instruction_aarch32::extract(insn,  27, 20) == 0b00110000 && //mov
         Instruction_aarch32::extract(insn2, 27, 20) == 0b00110100;   //movt
}

bool NativeMovConstReg::is_mov_n_three_orr_at(address addr) {
  return (Instruction_aarch32::extract(as_uint(addr), 27, 16) & 0b111111101111) == 0b001110100000 &&
          Instruction_aarch32::extract(as_uint(addr+arm_insn_sz), 27, 20) == 0b00111000 &&
          Instruction_aarch32::extract(as_uint(addr+2*arm_insn_sz), 27, 20) == 0b00111000 &&
          Instruction_aarch32::extract(as_uint(addr+3*arm_insn_sz), 27, 21) == 0b0011100;
}

bool NativeMovConstReg::is_at(address addr) {
  return is_ldr_literal_at(addr) ||
          is_far_ldr_literal_at(addr) ||
          is_movw_movt_at(addr) ||
          is_mov_n_three_orr_at(addr);
}

//-------------------------------------------------------------------
address NativeMovRegMem::instruction_address() const {
  return addr();
}

int NativeMovRegMem::offset() const  {
  assert(NativeMovConstReg::is_at(addr()), "no others");
  return NativeMovConstReg::from(addr())->data();
}

void NativeMovRegMem::set_offset(int x) {
  assert(NativeMovConstReg::is_at(addr()), "no others");
  NativeMovConstReg::from(addr())->set_data(x);
}

void NativeMovRegMem::verify() {
  assert(NativeMovConstReg::is_at(addr()), "no others");
}

//--------------------------------------------------------------------------------

void NativeJump::verify() {
  if (!is_jump()) {
    fatal("not a call");
  }
}

void NativeJump::check_verified_entry_alignment(address entry, address verified_entry) {
}

address NativeJump::jump_destination() const {
  assert(is_jump(), "not a call");
  if (NativeImmJump::is_at(addr())) {
    return NativeImmJump::from(addr())->destination();
  } else if (NativeMovConstReg::is_at(addr())) {
    return address(NativeMovConstReg::from(addr())->data());
  }
  ShouldNotReachHere();
  return NULL;
}

void NativeJump::set_jump_destination(address dest) {
  assert(is_jump(), "not a call");
  if (NativeImmJump::is_at(addr())) {
    NativeImmJump::from(addr())->set_destination(dest);
  } else if (NativeMovConstReg::is_at(addr())) {
    NativeMovConstReg::from(addr())->set_data((uintptr_t) dest);
  } else {
    ShouldNotReachHere();
  }
}

address NativeJump::next_instruction_address() const {
  assert(is_jump(), "not a call");
  if (NativeImmJump::is_at(addr())) {
    return NativeImmJump::from(addr())->next_instruction_address();
  } else if (NativeMovConstReg::is_at(addr())) {
    address after_move = NativeMovConstReg::from(addr())->next_instruction_address();
    assert(NativeRegJump::is_at(after_move), "should be jump");
    return NativeRegJump::from(after_move)->next_instruction_address();
  }
  ShouldNotReachHere();
  return NULL;
}

bool NativeJump::is_at(address addr) {
  if (NativeImmJump::is_at(addr)) {
    return true;
  }
  if (NativeMovConstReg::is_at(addr)) {
    NativeMovConstReg *nm = NativeMovConstReg::from(addr);
    address next_instr = nm->next_instruction_address();
    return NativeRegJump::is_at(next_instr) &&
      NativeRegJump::from(next_instr)->destination() == nm->destination();
  }
  return false;
}

NativeJump* NativeJump::from(address addr) {
  assert(NativeJump::is_at(addr), "");
  return (NativeJump*) addr;
}

// MT-safe inserting of a jump over a jump or a nop (used by
// nmethod::make_not_entrant_or_zombie)

void NativeJump::patch_verified_entry(address entry, address verified_entry, address dest) {

  assert(dest == SharedRuntime::get_handle_wrong_method_stub(),
     "expected fixed destination of patch");
  assert(NativeInstruction::from(verified_entry)->is_jump_or_nop() ||
      NativeInstruction::from(verified_entry)->is_sigill_zombie_not_entrant(),
         "Aarch32 cannot replace non-jump with jump");

  // Patch this nmethod atomically.
  if (Assembler::reachable_from_branch_at(verified_entry, dest)) {
    assert((((intptr_t) dest & 0x3) == 0) && (((intptr_t) verified_entry & 0x3) == 0),
        "addresses should be aligned on 4");
    ptrdiff_t disp = (dest - verified_entry - 8) >> 2;
    guarantee((-(1 << 23) <= disp) && (disp < (1 << 23)), "branch overflow");

    unsigned int insn = (0b11101010 << 24) | (disp & 0xffffff);
    *(unsigned int*)verified_entry = insn;
  } else {
    // We use an illegal instruction for marking a method as
    // not_entrant or zombie.
    NativeIllegalInstruction::insert(verified_entry);
  }

  ICache::invalidate_range(verified_entry, instruction_size);
}

//-------------------------------------------------------------------

bool NativeBranchType::is_branch_type(uint32_t insn) {
  return Instruction_aarch32::extract(insn, 27, 20) == 0b00010010 &&
    Instruction_aarch32::extract(insn, 19, 8) == 0b111111111111;
}

void NativeBranchType::patch_offset_to(address dest) {
  uint32_t insn = as_uint();
  const intptr_t off = (dest - (addr() + 8));
  assert((off & 3) == 0, "should be");
  assert(-32 * 1024 * 1024 <= off && off < 32 * 1024 * 1042,
      "new offset should fit in instruction");

  const unsigned off_mask = ((1U << 24) - 1);
  insn &= ~off_mask; // mask off offset part
  insn |= ((unsigned) off >> 2) & off_mask;

  set_uint(insn);
  ICache::invalidate_range(addr_at(0), instruction_size);
}

//-------------------------------------------------------------------

address NativeImmJump::destination() const {
  assert(is_imm_jump(), "not jump");
  return addr() + 8 + 4 * Instruction_aarch32::sextract(as_uint(), 23, 0);
}

void NativeImmJump::set_destination(address addr) {
  assert(is_imm_jump(), "");
  patch_offset_to(addr);
}

bool NativeImmJump::is_at(address addr) {
  unsigned insn = as_uint(addr);
  return Instruction_aarch32::extract(insn, 27, 24)  == 0b1010;
}

NativeImmJump* NativeImmJump::from(address addr) {
  assert(NativeImmJump::is_at(addr), "");
  return (NativeImmJump*) addr;
}

//-------------------------------------------------------------------

bool NativeRegJump::is_at(address addr) {
  unsigned insn = as_uint(addr);
  return is_branch_type(insn) && Instruction_aarch32::extract(insn, 7, 4) == 0b0001;
}

NativeRegJump* NativeRegJump::from(address addr) {
  assert(NativeRegJump::is_at(addr), "");
  return (NativeRegJump*) addr;
}

Register NativeRegJump::destination() const {
  assert(is_reg_jump(), "");
  return (Register) Instruction_aarch32::extract(as_uint(), 3, 0);
}

//-------------------------------------------------------------------

bool NativeInstruction::is_safepoint_poll() {
  // a safepoint_poll is implemented in two steps as
  //
  // movw(r9, polling_page & 0xffff);
  // movt(r9, polling_page >> 16);
  // ldr(r9, [r9, #0]);
  //
  // We can rely on this instructions order until we have only C1

  const intptr_t paddr = (intptr_t)os::get_polling_page();
  const Register scratch = rscratch1;

  if (NativeInstruction::from(addr())->is_ldr(scratch, Address(scratch))) {
    NativeMovConstReg* mov_const = NativeMovConstReg::before(addr());
    return (mov_const->data() == paddr) && (mov_const->destination() == scratch);
  }

  return false;
}

bool NativeInstruction::is_movt(Register dst, unsigned imm, Assembler::Condition cond) {
  bool a1 = Instruction_aarch32::extract(uint_at(0), 27, 20) == 0b00110100;
  bool a2 = Instruction_aarch32::extract(uint_at(0), 15, 12) == (unsigned)dst;
  bool a3 = Instruction_aarch32::extract(uint_at(0), 11, 0) == ((unsigned)imm & 0xfff);
  bool a4 = Instruction_aarch32::extract(uint_at(0), 19, 16) == ((unsigned)imm >> 12);
  bool a5 = Instruction_aarch32::extract(uint_at(0), 31, 28) == (unsigned)cond;

  return a1 && a2 && a3 && a4 && a5;
}

bool NativeInstruction::is_movw(Register dst, unsigned imm, Assembler::Condition cond) {
  bool a1 = Instruction_aarch32::extract(uint_at(0), 27, 20) == 0b00110000;
  bool a2 = Instruction_aarch32::extract(uint_at(0), 15, 12) == (unsigned)dst;
  bool a3 = Instruction_aarch32::extract(uint_at(0), 11, 0) == ((unsigned)imm & 0xfff);
  bool a4 = Instruction_aarch32::extract(uint_at(0), 19, 16) == ((unsigned)imm >> 12);
  bool a5 = Instruction_aarch32::extract(uint_at(0), 31, 28) == (unsigned)cond;

  return a1 && a2 && a3 && a4 && a5;
}

bool NativeInstruction::is_ldr(Register dst, Address addr, Assembler::Condition cond) {
    assert(addr.get_mode() == Address::imm, "unimplemented");
    assert(addr.get_wb_mode() == Address::off, "unimplemented");
    assert(addr.index() == noreg, "unimplemented");
    assert(addr.offset() == 0, "unimplemented");

    bool b0 = Instruction_aarch32::extract(uint_at(0), 24, 24) == 1; //P
    bool b1 = Instruction_aarch32::extract(uint_at(0), 23, 23) == 1; //U
    bool b2 = Instruction_aarch32::extract(uint_at(0), 21, 21) == 0; //W
    bool b3 = Instruction_aarch32::extract(uint_at(0), 19, 16) == (unsigned)addr.base();
    bool b4 = Instruction_aarch32::extract(uint_at(0), 11, 0) == 0;

    bool a1 = b0 && b1 && b2 && b3 && b4; //Address encoding

    bool a2 = Instruction_aarch32::extract(uint_at(0), 15, 12) == (unsigned)dst;
    bool a3 = Instruction_aarch32::extract(uint_at(0), 20, 20) == 1;
    bool a4 = Instruction_aarch32::extract(uint_at(0), 22, 22) == 0;
    bool a5 = Instruction_aarch32::extract(uint_at(0), 27, 25) == 0b010;
    bool a6 = Instruction_aarch32::extract(uint_at(0), 31, 28) == (unsigned)cond;

    return a1 && a2 && a3 && a4 && a5 && a6;
}


bool NativeInstruction::is_movt() {
  return Instruction_aarch32::extract(int_at(0), 27, 20) == 0b00110100;
}

bool NativeInstruction::is_orr() {
  return Instruction_aarch32::extract(int_at(0), 27, 21) == 0b0011100;
}

bool NativeInstruction::is_sigill_zombie_not_entrant() {
  return as_uint() == 0xe7fdeafd; // udf #0xdead
}

void NativeIllegalInstruction::insert(address code_pos) {
  *(juint*)code_pos = 0xe7fdeafd; // udf #0xdead
}

//-------------------------------------------------------------------

void NativeGeneralJump::verify() {  }

void NativeGeneralJump::insert_unconditional(address code_pos, address entry) {
  NativeGeneralJump* n_jump = (NativeGeneralJump*)code_pos;
  assert(n_jump->is_nop() || n_jump->is_imm_jump(), "not overwrite whats not supposed");

  CodeBuffer cb(code_pos, instruction_size);
  MacroAssembler a(&cb);

  a.b(entry);

  ICache::invalidate_range(code_pos, instruction_size);
}

// MT-safe patching of a long jump instruction.
void NativeGeneralJump::replace_mt_safe(address instr_addr, address code_buffer) {
  if (NativeFarLdr::is_at(instr_addr+2*arm_insn_sz)) {
    assert(NativeInstruction::from(code_buffer)->is_nop(), "code_buffer image");
    assert(NativeImmJump::is_at(instr_addr), "instr_image image");
    // first 'b' prevents NativeFarLdr to recognize patching_prolog, skip it manually
    address load_instr = instr_addr+2*arm_insn_sz;

    NativeFarLdr::from(load_instr)->set_data_addr(NativeFarLdr::from(code_buffer)->data_addr());

    WRITE_MEM_BARRIER;
    *(uintptr_t*)instr_addr = *(uintptr_t*)code_buffer;
    ICache::invalidate_word(instr_addr);

    assert(NativeFarLdr::is_at(instr_addr), "now valid constant loading");
  } else {
    ShouldNotReachHere();
  }
}
