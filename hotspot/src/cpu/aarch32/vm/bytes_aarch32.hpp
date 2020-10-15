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

#ifndef CPU_AARCH32_VM_BYTES_AARCH32_HPP
#define CPU_AARCH32_VM_BYTES_AARCH32_HPP

#include "memory/allocation.hpp"

class Bytes: AllStatic {
 public:
  // Returns true if the byte ordering used by Java is different from the native byte ordering
  // of the underlying machine. For example, this is true for Intel x86, but false for Solaris
  // on Sparc.
  //
  // AArch32 is little-endian, Java is big-endian; returns true
  static inline bool is_Java_byte_ordering_different(){ return true; }


  // Efficient reading and writing of unaligned unsigned data in platform-specific byte ordering.
  // Since ARMv6 unaligned short and word accesses are handled by hardware.
  // However, unaligned double-word access causes kernel trap and software processing,
  // so we turn it to fast unalinged word access.
  static inline u2   get_native_u2(address p)         { return *(u2*)p; }
  static inline u4   get_native_u4(address p)         { return *(u4*)p; }
  static inline u8   get_native_u8(address p)         {
    if (!(uintptr_t(p) & 3)) {
      return *(u8*)p;
    }
    u4 *const a = (u4*) p;
    return (u8(a[1]) << 32) | a[0];
  }

  static inline void put_native_u2(address p, u2 x)   { *(u2*)p = x; }
  static inline void put_native_u4(address p, u4 x)   { *(u4*)p = x; }
  static inline void put_native_u8(address p, u8 x)   { *(u8*)p = x; }


  // Efficient reading and writing of unaligned unsigned data in Java
  // byte ordering (i.e. big-endian ordering). Byte-order reversal is
  // needed since AArch32 use little-endian format.
  static inline u2   get_Java_u2(address p)           { return swap_u2(get_native_u2(p)); }
  static inline u4   get_Java_u4(address p)           { return swap_u4(get_native_u4(p)); }
  static inline u8   get_Java_u8(address p)           { return swap_u8(get_native_u8(p)); }

  static inline void put_Java_u2(address p, u2 x)     { put_native_u2(p, swap_u2(x)); }
  static inline void put_Java_u4(address p, u4 x)     { put_native_u4(p, swap_u4(x)); }
  static inline void put_Java_u8(address p, u8 x)     {
    const u8 nx = swap_u8(x);
    if (!(uintptr_t(p) & 3)) {
      *(u8*)p = nx;
    } else {
      u4 *const a = (u4*) p;
      a[0] = nx;
      a[1] = nx >> 32;
    }
  }

  // Efficient swapping of byte ordering
  static inline u2   swap_u2(u2 x);                   // compiler-dependent implementation
  static inline u4   swap_u4(u4 x);                   // compiler-dependent implementation
  static inline u8   swap_u8(u8 x);
};


// The following header contains the implementations of swap_u2, swap_u4, and swap_u8[_base]

#ifdef TARGET_OS_ARCH_linux_aarch32
# include "bytes_linux_aarch32.inline.hpp"
#endif

#endif // CPU_AARCH32_VM_BYTES_AARCH32_HPP
