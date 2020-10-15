/*
 * Copyright (c) 2003, 2011, Oracle and/or its affiliates. All rights reserved.
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

#ifndef OS_CPU_LINUX_AARCH32_VM_ORDERACCESS_LINUX_AARCH32_INLINE_HPP
#define OS_CPU_LINUX_AARCH32_VM_ORDERACCESS_LINUX_AARCH32_INLINE_HPP

#include "runtime/atomic.inline.hpp"
#include "runtime/orderAccess.hpp"
#include "runtime/os.hpp"
#include "vm_version_aarch32.hpp"

// Implementation of class OrderAccess.

inline void OrderAccess::loadload()   { acquire(); }
inline void OrderAccess::storestore() { release(); }
inline void OrderAccess::loadstore()  { acquire(); }
inline void OrderAccess::storeload()  { fence(); }

inline void OrderAccess::acquire() {
  READ_MEM_BARRIER;
}

inline void OrderAccess::release() {
  WRITE_MEM_BARRIER;
}

inline void OrderAccess::fence() {
  FULL_MEM_BARRIER;
}

// __atomic builtins should be supported since gcc 4.4, however not all 4.4 do support.
// for simplicity, provide own implementation with same semantic.
#define ARM_ATOMIC_ACQUIRE 2
#define ARM_ATOMIC_RELEASE 3
#define ARM_ATOMIC_RELAXED 0

// the following implementation is only valid for values up to 4 bytes long. DO NOT USE for jlong!
#define arm_atomic_load(S, D, X) { \
    STATIC_ASSERT(sizeof(*S) <= sizeof(jint)); \
    STATIC_ASSERT(X == ARM_ATOMIC_ACQUIRE || X == ARM_ATOMIC_RELAXED); \
    *(D) = *(S); if (X == ARM_ATOMIC_ACQUIRE) READ_MEM_BARRIER; \
}
#define arm_atomic_store(D, S, X) { \
    STATIC_ASSERT(sizeof(*S) <= sizeof(jint)); \
    STATIC_ASSERT(X == ARM_ATOMIC_RELEASE || X == ARM_ATOMIC_RELAXED); \
    if (X == ARM_ATOMIC_RELEASE) WRITE_MEM_BARRIER; *(D) = *(S); \
}

inline jbyte    OrderAccess::load_acquire(volatile jbyte*   p)
{ jbyte data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline jshort   OrderAccess::load_acquire(volatile jshort*  p)
{ jshort data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline jint     OrderAccess::load_acquire(volatile jint*    p)
{ jint data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline jlong    OrderAccess::load_acquire(volatile jlong*   p)
{
    jlong data;
    data = Atomic::load(p);
    READ_MEM_BARRIER;
    return data;
}
inline jubyte    OrderAccess::load_acquire(volatile jubyte*   p)
{ jubyte data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline jushort   OrderAccess::load_acquire(volatile jushort*  p)
{ jushort data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline juint     OrderAccess::load_acquire(volatile juint*    p)
{ juint data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline julong   OrderAccess::load_acquire(volatile julong*  p)
{
    julong data;
    data = (julong)Atomic::load((volatile jlong*)p);
    READ_MEM_BARRIER;
    return data;
}
inline jfloat   OrderAccess::load_acquire(volatile jfloat*  p)
{ jfloat data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline jdouble  OrderAccess::load_acquire(volatile jdouble* p)
{
    jlong data = Atomic::load((volatile jlong*)p);
    READ_MEM_BARRIER;
    // in -fno-strict-aliasing we trust. this option should be (and is) provided to g++
    return *(jdouble*)&data;
}
inline intptr_t OrderAccess::load_ptr_acquire(volatile intptr_t*   p)
{ intptr_t data; arm_atomic_load(p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline void*    OrderAccess::load_ptr_acquire(volatile void*       p)
{ void* data; arm_atomic_load((void* volatile *)p, &data, ARM_ATOMIC_ACQUIRE); return data; }
inline void*    OrderAccess::load_ptr_acquire(const volatile void* p)
{ void* data; arm_atomic_load((void* const volatile *)p, &data, ARM_ATOMIC_ACQUIRE); return data; }

inline void     OrderAccess::release_store(volatile jbyte*   p, jbyte   v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile jshort*  p, jshort  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile jint*    p, jint    v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile jlong*   p, jlong   v)
{
  WRITE_MEM_BARRIER;
  Atomic::store(v, p);
}
inline void     OrderAccess::release_store(volatile jubyte*  p, jubyte  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile jushort* p, jushort v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile juint*   p, juint   v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile julong*  p, julong  v)
{
  WRITE_MEM_BARRIER;
  Atomic::store(*(jlong*)&v, (jlong*)p);
}
inline void     OrderAccess::release_store(volatile jfloat*  p, jfloat  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store(volatile jdouble* p, jdouble v)
{
  WRITE_MEM_BARRIER;
  // in -fno-strict-aliasing we trust. this option should be (and is) provided to g++
  Atomic::store(*(jlong*)&v, (jlong*)p);
}
inline void     OrderAccess::release_store_ptr(volatile intptr_t* p, intptr_t v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELEASE); }
inline void     OrderAccess::release_store_ptr(volatile void*     p, void*    v)
{ arm_atomic_store((void* volatile *)p, &v, ARM_ATOMIC_RELEASE); }

inline void     OrderAccess::store_fence(jbyte*   p, jbyte   v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(jshort*  p, jshort  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(jint*    p, jint    v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(jlong*   p, jlong   v)
{ Atomic::store(v, p); fence(); }
inline void     OrderAccess::store_fence(jubyte*  p, jubyte  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(jushort* p, jushort v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(juint*   p, juint   v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_fence(julong*  p, julong  v)
{ Atomic::store(*(jlong*)&v, (jlong*)p); fence(); }
inline void     OrderAccess::store_fence(jfloat*  p, jfloat  v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
// in -fno-strict-aliasing we trust. this option should be (and is) provided to g++
inline void     OrderAccess::store_fence(jdouble* p, jdouble v)
{ Atomic::store(*(jlong*)&v, (jlong*)p); fence(); }
inline void     OrderAccess::store_ptr_fence(intptr_t* p, intptr_t v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }
inline void     OrderAccess::store_ptr_fence(void**    p, void*    v)
{ arm_atomic_store(p, &v, ARM_ATOMIC_RELAXED); fence(); }

inline void     OrderAccess::release_store_fence(volatile jbyte*   p, jbyte   v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jshort*  p, jshort  v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jint*    p, jint    v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jlong*   p, jlong   v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jubyte*  p, jubyte  v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jushort* p, jushort v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile juint*   p, juint   v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile julong*  p, julong  v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jfloat*  p, jfloat  v) { release_store(p, v); fence(); }
inline void     OrderAccess::release_store_fence(volatile jdouble* p, jdouble v) { release_store(p, v); fence(); }

inline void     OrderAccess::release_store_ptr_fence(volatile intptr_t* p, intptr_t v) { release_store_ptr(p, v); fence(); }
inline void     OrderAccess::release_store_ptr_fence(volatile void*     p, void*    v) { release_store_ptr(p, v); fence(); }

#undef arm_atomic_load
#undef arm_atomic_store
#undef ARM_ATOMIC_ACQUIRE
#undef ARM_ATOMIC_RELEASE
#undef ARM_ATOMIC_RELAXED

#endif // OS_CPU_LINUX_AARCH32_VM_ORDERACCESS_LINUX_AARCH32_INLINE_HPP
