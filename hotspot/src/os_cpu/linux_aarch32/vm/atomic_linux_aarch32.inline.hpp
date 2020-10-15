/*
 * Copyright (c) 1999, 2011, Oracle and/or its affiliates. All rights reserved.
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

#ifndef OS_CPU_LINUX_AARCH32_VM_ATOMIC_LINUX_AARCH32_INLINE_HPP
#define OS_CPU_LINUX_AARCH32_VM_ATOMIC_LINUX_AARCH32_INLINE_HPP

#include "runtime/atomic.hpp"
#include "runtime/os.hpp"
#include "vm_version_aarch32.hpp"

// Implementation of class atomic

// various toolchains set different symbols to indicate that ARMv7 architecture is set as a target
// startign from v7 use more lightweight barrier instructions
#if (defined(__ARM_ARCH) && __ARM_ARCH >= 7) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7__)
#define FULL_MEM_BARRIER  __asm__ __volatile__ ("dmb ish"   : : : "memory")
#define READ_MEM_BARRIER  __asm__ __volatile__ ("dmb ish"   : : : "memory")
#define WRITE_MEM_BARRIER __asm__ __volatile__ ("dmb ishst" : : : "memory")
#else
#define FULL_MEM_BARRIER  __sync_synchronize()
#define READ_MEM_BARRIER  __asm__ __volatile__ ("mcr p15,0,r0,c7,c10,5" : : : "memory")
#define WRITE_MEM_BARRIER __asm__ __volatile__ ("mcr p15,0,r0,c7,c10,5" : : : "memory")
#endif

inline void Atomic::store    (jbyte    store_value, jbyte*    dest) { *dest = store_value; }
inline void Atomic::store    (jshort   store_value, jshort*   dest) { *dest = store_value; }
inline void Atomic::store    (jint     store_value, jint*     dest) { *dest = store_value; }
inline void Atomic::store_ptr(intptr_t store_value, intptr_t* dest) { *dest = store_value; }
inline void Atomic::store_ptr(void*    store_value, void*     dest) { *(void**)dest = store_value; }

inline void Atomic::store    (jbyte    store_value, volatile jbyte*    dest) { *dest = store_value; }
inline void Atomic::store    (jshort   store_value, volatile jshort*   dest) { *dest = store_value; }
inline void Atomic::store    (jint     store_value, volatile jint*     dest) { *dest = store_value; }
inline void Atomic::store_ptr(intptr_t store_value, volatile intptr_t* dest) { *dest = store_value; }
inline void Atomic::store_ptr(void*    store_value, volatile void*     dest) { *(void* volatile *)dest = store_value; }


inline jint Atomic::add(jint add_value, volatile jint* dest)
{
 return __sync_add_and_fetch(dest, add_value);
}

inline void Atomic::inc(volatile jint* dest)
{
 add(1, dest);
}

inline void Atomic::inc_ptr(volatile void* dest)
{
 add_ptr(1, dest);
}

inline void Atomic::dec (volatile jint* dest)
{
 add(-1, dest);
}

inline void Atomic::dec_ptr(volatile void* dest)
{
 add_ptr(-1, dest);
}

inline jint Atomic::xchg (jint exchange_value, volatile jint* dest)
{
  jint res = __sync_lock_test_and_set (dest, exchange_value);
  FULL_MEM_BARRIER;
  return res;
}

inline void* Atomic::xchg_ptr(void* exchange_value, volatile void* dest)
{
  return (void *) xchg_ptr((intptr_t) exchange_value,
                           (volatile intptr_t*) dest);
}


inline jint Atomic::cmpxchg (jint exchange_value, volatile jint* dest, jint compare_value)
{
 return __sync_val_compare_and_swap(dest, compare_value, exchange_value);
}

inline void Atomic::store (jlong store_value, jlong* dest) {
    store(store_value, (volatile jlong *)dest);
}

inline void Atomic::store (jlong store_value, volatile jlong* dest) {
// have seen a few toolchains which only set a subset of appropriate defines
// and as well do not provide atomic API, hence so complicated condition
#if (defined(__ARM_ARCH) && __ARM_ARCH >= 7) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_6ZK__) || defined(__ARM_ARCH_6K__) || (defined(__ARM_FEATURE_LDREX) && (__ARM_FEATURE_LDREX & 8))
  // the below is only supported since ARMv6K, adapt otherwise
  register long long t1;
  register int t3;
  __asm__ __volatile__ (
      "repeat_%=:\n\t"
      "ldrexd %Q[t1],%R[t1],[%[addr]]\n\t"
      "strexd %[t3],%Q[val],%R[val],[%[addr]]\n\t"
      "cmp %[t3],#0\n\t"
      "bne repeat_%="
      : [t1] "=&r" (t1),
        [t3] "=&r" (t3)
      : [val] "r" (store_value), [addr] "r" (dest)
      : "memory");
#else
  __atomic_store_n(dest, store_value, __ATOMIC_RELAXED);
#endif
}

inline intptr_t Atomic::add_ptr(intptr_t add_value, volatile intptr_t* dest)
{
 return __sync_add_and_fetch(dest, add_value);
}

inline void* Atomic::add_ptr(intptr_t add_value, volatile void* dest)
{
  return (void *) add_ptr(add_value, (volatile intptr_t *) dest);
}

inline void Atomic::inc_ptr(volatile intptr_t* dest)
{
 add_ptr(1, dest);
}

inline void Atomic::dec_ptr(volatile intptr_t* dest)
{
 add_ptr(-1, dest);
}

inline intptr_t Atomic::xchg_ptr(intptr_t exchange_value, volatile intptr_t* dest)
{
  intptr_t res = __sync_lock_test_and_set (dest, exchange_value);
  FULL_MEM_BARRIER;
  return res;
}

inline jlong Atomic::cmpxchg (jlong exchange_value, volatile jlong* dest, jlong compare_value)
{
// have seen a few toolchains which only set a subset of appropriate defines
// and as well do not provide dword CAS, hence so complicated condition
#if (defined(__ARM_ARCH) && __ARM_ARCH >= 7) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_6ZK__) || defined(__ARM_ARCH_6K__) || (defined(__ARM_FEATURE_LDREX) && (__ARM_FEATURE_LDREX & 8))
  register long long old_value;
  register int store_result;
  __asm__ __volatile__ (
      "mov %[res],#1\n\t"
      "repeat_%=:\n\t"
      "ldrexd %Q[old],%R[old],[%[addr]]\n\t"
      "cmp %Q[old], %Q[cmpr]\n\t"
      "ittt eq\n\t"
      "cmpeq %R[old], %R[cmpr]\n\t"
      "strexdeq %[res],%Q[exch],%R[exch],[%[addr]]\n\t"
      "cmpeq %[res],#1\n\t"
      "beq repeat_%="
      : [old] "=&r" (old_value),
        [res] "=&r" (store_result)
      : [exch] "r" (exchange_value),
        [cmpr] "r" (compare_value),
        [addr] "r" (dest)
      : "memory");
  return old_value;
#else
  return __sync_val_compare_and_swap(dest, compare_value, exchange_value);
#endif
}

inline intptr_t Atomic::cmpxchg_ptr(intptr_t exchange_value, volatile intptr_t* dest, intptr_t compare_value)
{
 return __sync_val_compare_and_swap(dest, compare_value, exchange_value);
}

inline void* Atomic::cmpxchg_ptr(void* exchange_value, volatile void* dest, void* compare_value)
{
  return (void *) cmpxchg_ptr((intptr_t) exchange_value,
                              (volatile intptr_t*) dest,
                              (intptr_t) compare_value);
}

inline jlong Atomic::load(volatile jlong* src) {
// have seen a few toolchains which only set a subset of appropriate defines
// and as well do not provide atomic API, hence so complicated condition
#if (defined(__ARM_ARCH) && __ARM_ARCH >= 7) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_6ZK__) || defined(__ARM_ARCH_6K__) || (defined(__ARM_FEATURE_LDREX) && (__ARM_FEATURE_LDREX & 8))
  register long long res;
  __asm__ __volatile__ (
      "ldrexd %Q[res], %R[res], [%[addr]]"
      : [res] "=r" (res)
      : [addr] "r" (src)
      : "memory");
  return res;
#else
  return __atomic_load_n(src, __ATOMIC_RELAXED);
#endif
}

#endif // OS_CPU_LINUX_AARCH32_VM_ATOMIC_LINUX_AARCH32_INLINE_HPP
