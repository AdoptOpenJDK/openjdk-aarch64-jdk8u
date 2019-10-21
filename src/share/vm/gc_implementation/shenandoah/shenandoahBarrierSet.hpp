/*
 * Copyright (c) 2013, 2018, Red Hat, Inc. All rights reserved.
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

#ifndef SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_HPP
#define SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_HPP

#include "memory/barrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahAsserts.hpp"

class ShenandoahBarrierSetAssembler;
class ShenandoahBarrierSetC1;
class ShenandoahBarrierSetC2;
class ShenandoahHeap;

class ShenandoahBarrierSet: public BarrierSet {
private:
  ShenandoahHeap* _heap;
  ShenandoahBarrierSetAssembler* const _bsasm;
  ShenandoahBarrierSetC1* const _bsc1;
  ShenandoahBarrierSetC2* const _bsc2;

public:
  ShenandoahBarrierSet(ShenandoahHeap* heap);

  inline static ShenandoahBarrierSet* barrier_set() {
    BarrierSet *bs = oopDesc::bs();
    assert(bs->kind() == BarrierSet::ShenandoahBarrierSet, "sanity");
    return (ShenandoahBarrierSet*)bs;
  }

  ShenandoahBarrierSetAssembler* bsasm() const;
  ShenandoahBarrierSetC1* bsc1() const;
  ShenandoahBarrierSetC2* bsc2() const;

  void print_on(outputStream* st) const;

  bool is_a(BarrierSet::Name bsn);

  bool has_read_prim_array_opt();
  bool has_read_prim_barrier();
  bool has_read_ref_array_opt();
  bool has_read_ref_barrier();
  bool has_read_region_opt();
  bool has_write_prim_array_opt();
  bool has_write_prim_barrier();
  bool has_write_ref_array_opt();
  bool has_write_ref_barrier();
  bool has_write_ref_pre_barrier();
  bool has_write_region_opt();
  bool is_aligned(HeapWord* hw);
  void read_prim_array(MemRegion mr)            shenandoah_not_implemented;
  void read_prim_field(HeapWord* hw, size_t s)  shenandoah_not_implemented;
  bool read_prim_needs_barrier(HeapWord* hw, size_t s);
  void read_ref_array(MemRegion mr)             shenandoah_not_implemented;

  void read_ref_field(void* v);

  bool read_ref_needs_barrier(void* v)          shenandoah_not_implemented_return(false);
  void read_region(MemRegion mr)                shenandoah_not_implemented;
  void resize_covered_region(MemRegion mr)      shenandoah_not_implemented;
  void write_prim_array(MemRegion mr)           shenandoah_not_implemented;
  void write_prim_field(HeapWord* hw, size_t s , juint x, juint y) shenandoah_not_implemented;
  bool write_prim_needs_barrier(HeapWord* hw, size_t s, juint x, juint y) shenandoah_not_implemented_return(false);
  void write_ref_array(HeapWord* start, size_t count);
  void write_ref_array_work(MemRegion r)        shenandoah_not_implemented;

  template <class T> void
  write_ref_array_pre_work(T* dst, size_t count);

  void write_ref_array_pre(oop* dst, int count, bool dest_uninitialized);

  void write_ref_array_pre(narrowOop* dst, int count, bool dest_uninitialized);

  template <class T> static void write_ref_field_pre_static(T* field, oop newVal);

  // We export this to make it available in cases where the static
  // type of the barrier set is known.  Note that it is non-virtual.
  template <class T> inline void inline_write_ref_field_pre(T* field, oop newVal);

  // These are the more general virtual versions.
  void write_ref_field_pre_work(oop* field, oop new_val);
  void write_ref_field_pre_work(narrowOop* field, oop new_val);
  void write_ref_field_pre_work(void* field, oop new_val) shenandoah_not_implemented;

  void write_ref_field_work(void* v, oop o, bool release = false);
  void write_region_work(MemRegion mr);

  static inline oop resolve_forwarded_not_null(oop p);
  static inline oop resolve_forwarded(oop p);

  static oopDesc* write_barrier_IRT(oopDesc* src);
  static oopDesc* write_barrier_JRT(oopDesc* src);

  oop write_barrier_mutator(oop obj);

  oop load_reference_barrier(oop obj);
  oop load_reference_barrier_mutator(oop obj);
  oop load_reference_barrier_not_null(oop obj);

  oop oop_atomic_cmpxchg_in_heap(oop new_value, volatile HeapWord* dest, oop compare_value);

  void enqueue(oop obj);

private:
  inline bool need_update_refs_barrier();

  template <class T>
  void write_ref_array_loop(HeapWord* start, size_t count);

  oop load_reference_barrier_impl(oop obj);

  oop atomic_compare_exchange_oop(oop exchange_value,
                                  volatile HeapWord *dest,
                                  oop compare_value);
};

#endif //SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_HPP
