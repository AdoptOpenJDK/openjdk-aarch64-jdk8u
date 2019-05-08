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

#include "precompiled.hpp"
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#include "gc_implementation/shenandoah/shenandoahAsserts.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahCollectorPolicy.hpp"
#include "gc_implementation/shenandoah/shenandoahForwarding.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahHeuristics.hpp"
#include "runtime/interfaceSupport.hpp"
#include "utilities/macros.hpp"

#ifdef COMPILER1
#include "gc_implementation/shenandoah/shenandoahBarrierSetC1.hpp"
#endif
#ifdef COMPILER2
#include "gc_implementation/shenandoah/shenandoahBarrierSetC2.hpp"
#endif

#if defined(TARGET_ARCH_aarch64)
#include "shenandoahBarrierSetAssembler_aarch64.hpp"
#elif defined(TARGET_ARCH_x86)
#include "shenandoahBarrierSetAssembler_x86.hpp"
#else
#include "shenandoahBarrierSetAssembler_stub.hpp"
#endif

class ShenandoahUpdateRefsForOopClosure: public ExtendedOopClosure {
private:
  ShenandoahHeap* _heap;
  ShenandoahBarrierSet* _bs;

  template <class T>
  inline void do_oop_work(T* p) {
    _heap->maybe_update_with_forwarded(p);
  }
public:
  ShenandoahUpdateRefsForOopClosure() : _heap(ShenandoahHeap::heap()), _bs(ShenandoahBarrierSet::barrier_set()) {
    assert(UseShenandoahGC && ShenandoahCloneBarrier, "should be enabled");
  }
  void do_oop(oop* p)       { do_oop_work(p); }
  void do_oop(narrowOop* p) { do_oop_work(p); }
};

ShenandoahBarrierSet::ShenandoahBarrierSet(ShenandoahHeap* heap) :
  BarrierSet(),
  _heap(heap),
  _bsasm(new ShenandoahBarrierSetAssembler()),
  _bsc1(COMPILER1_PRESENT(new ShenandoahBarrierSetC1()) NOT_COMPILER1(NULL)),
  _bsc2(COMPILER2_PRESENT(new ShenandoahBarrierSetC2()) NOT_COMPILER2(NULL))
{
  _kind = BarrierSet::ShenandoahBarrierSet;
}

ShenandoahBarrierSetAssembler* ShenandoahBarrierSet::bsasm() const {
  return _bsasm;
}

ShenandoahBarrierSetC1* ShenandoahBarrierSet::bsc1() const {
  return _bsc1;
}

ShenandoahBarrierSetC2* ShenandoahBarrierSet::bsc2() const {
  return _bsc2;
}

void ShenandoahBarrierSet::print_on(outputStream* st) const {
  st->print("ShenandoahBarrierSet");
}

bool ShenandoahBarrierSet::is_a(BarrierSet::Name bsn) {
  return bsn == BarrierSet::ShenandoahBarrierSet;
}

bool ShenandoahBarrierSet::has_read_prim_array_opt() {
  return true;
}

bool ShenandoahBarrierSet::has_read_prim_barrier() {
  return false;
}

bool ShenandoahBarrierSet::has_read_ref_array_opt() {
  return true;
}

bool ShenandoahBarrierSet::has_read_ref_barrier() {
  return false;
}

bool ShenandoahBarrierSet::has_read_region_opt() {
  return true;
}

bool ShenandoahBarrierSet::has_write_prim_array_opt() {
  return true;
}

bool ShenandoahBarrierSet::has_write_prim_barrier() {
  return false;
}

bool ShenandoahBarrierSet::has_write_ref_array_opt() {
  return true;
}

bool ShenandoahBarrierSet::has_write_ref_barrier() {
  return true;
}

bool ShenandoahBarrierSet::has_write_ref_pre_barrier() {
  return true;
}

bool ShenandoahBarrierSet::has_write_region_opt() {
  return true;
}

bool ShenandoahBarrierSet::is_aligned(HeapWord* hw) {
  return true;
}

bool ShenandoahBarrierSet::read_prim_needs_barrier(HeapWord* hw, size_t s) {
  return false;
}

void ShenandoahBarrierSet::read_ref_field(void* v) {
  //    tty->print_cr("read_ref_field: v = "PTR_FORMAT, v);
  // return *v;
}

template <class T>
void ShenandoahBarrierSet::write_ref_array_loop(HeapWord* start, size_t count) {
  assert(UseShenandoahGC && ShenandoahCloneBarrier, "Should be enabled");
  ShenandoahUpdateRefsForOopClosure cl;
  T* dst = (T*) start;
  for (size_t i = 0; i < count; i++) {
    cl.do_oop(dst++);
  }
}

void ShenandoahBarrierSet::write_ref_array(HeapWord* start, size_t count) {
  assert(UseShenandoahGC, "should be enabled");
  if (!ShenandoahCloneBarrier) return;
  if (!need_update_refs_barrier()) return;

  ShenandoahEvacOOMScope oom_evac_scope;
  if (UseCompressedOops) {
    write_ref_array_loop<narrowOop>(start, count);
  } else {
    write_ref_array_loop<oop>(start, count);
  }
}

template <class T>
void ShenandoahBarrierSet::write_ref_array_pre_work(T* dst, size_t count) {
  assert (UseShenandoahGC && ShenandoahSATBBarrier, "Should be enabled");

  shenandoah_assert_not_in_cset_loc_except(dst, _heap->cancelled_gc());

  if (! JavaThread::satb_mark_queue_set().is_active()) return;
  T* elem_ptr = dst;
  for (size_t i = 0; i < count; i++, elem_ptr++) {
    T heap_oop = oopDesc::load_heap_oop(elem_ptr);
    if (!oopDesc::is_null(heap_oop)) {
      enqueue(oopDesc::decode_heap_oop_not_null(heap_oop));
    }
  }
}

void ShenandoahBarrierSet::write_ref_array_pre(oop* dst, int count, bool dest_uninitialized) {
  if (! dest_uninitialized && ShenandoahSATBBarrier) {
    write_ref_array_pre_work(dst, (size_t)count);
  }
}

void ShenandoahBarrierSet::write_ref_array_pre(narrowOop* dst, int count, bool dest_uninitialized) {
  if (! dest_uninitialized && ShenandoahSATBBarrier) {
    write_ref_array_pre_work(dst, (size_t)count);
  }
}

template <class T>
void ShenandoahBarrierSet::write_ref_field_pre_static(T* field, oop newVal) {
  T heap_oop = oopDesc::load_heap_oop(field);

  shenandoah_assert_not_in_cset_loc_except(field, ShenandoahHeap::heap()->cancelled_gc());

  if (!oopDesc::is_null(heap_oop)) {
    ShenandoahBarrierSet::barrier_set()->enqueue(oopDesc::decode_heap_oop(heap_oop));
  }
}

template <class T>
inline void ShenandoahBarrierSet::inline_write_ref_field_pre(T* field, oop newVal) {
  write_ref_field_pre_static(field, newVal);
}

// These are the more general virtual versions.
void ShenandoahBarrierSet::write_ref_field_pre_work(oop* field, oop new_val) {
  write_ref_field_pre_static(field, new_val);
}

void ShenandoahBarrierSet::write_ref_field_pre_work(narrowOop* field, oop new_val) {
  write_ref_field_pre_static(field, new_val);
}

void ShenandoahBarrierSet::write_ref_field_work(void* v, oop o, bool release) {
  shenandoah_assert_not_in_cset_loc_except(v, _heap->cancelled_gc());
  shenandoah_assert_not_forwarded_except  (v, o, o == NULL || _heap->cancelled_gc() || !_heap->is_concurrent_mark_in_progress());
  shenandoah_assert_not_in_cset_except    (v, o, o == NULL || _heap->cancelled_gc() || !_heap->is_concurrent_mark_in_progress());
}

void ShenandoahBarrierSet::write_region_work(MemRegion mr) {
  assert(UseShenandoahGC, "should be enabled");
  if (!ShenandoahCloneBarrier) return;
  if (! need_update_refs_barrier()) return;

  // This is called for cloning an object (see jvm.cpp) after the clone
  // has been made. We are not interested in any 'previous value' because
  // it would be NULL in any case. But we *are* interested in any oop*
  // that potentially need to be updated.

  ShenandoahEvacOOMScope oom_evac_scope;
  oop obj = oop(mr.start());
  shenandoah_assert_correct(NULL, obj);
  ShenandoahUpdateRefsForOopClosure cl;
  obj->oop_iterate(&cl);
}

oop ShenandoahBarrierSet::load_reference_barrier_not_null(oop obj) {
  assert(obj != NULL, "");
  if (ShenandoahLoadRefBarrier && _heap->has_forwarded_objects()) {
    return load_reference_barrier_impl(obj);
  } else {
    return obj;
  }
}

oop ShenandoahBarrierSet::load_reference_barrier(oop obj) {
  if (obj != NULL) {
    return load_reference_barrier_not_null(obj);
  } else {
    return obj;
  }
}


oop ShenandoahBarrierSet::load_reference_barrier_mutator(oop obj) {
  assert(ShenandoahLoadRefBarrier, "should be enabled");
  assert(_heap->is_gc_in_progress_mask(ShenandoahHeap::EVACUATION), "evac should be in progress");
  shenandoah_assert_in_cset(NULL, obj);

  oop fwd = resolve_forwarded_not_null(obj);
  if (obj == fwd) {
    ShenandoahEvacOOMScope oom_evac_scope;

    Thread* thread = Thread::current();
    oop res_oop = _heap->evacuate_object(obj, thread);

    // Since we are already here and paid the price of getting through runtime call adapters
    // and acquiring oom-scope, it makes sense to try and evacuate more adjacent objects,
    // thus amortizing the overhead. For sparsely live heaps, scan costs easily dominate
    // total assist costs, and can introduce a lot of evacuation latency. This is why we
    // only scan for _nearest_ N objects, regardless if they are eligible for evac or not.
    // The scan itself should also avoid touching the non-marked objects below TAMS, because
    // their metadata (notably, klasses) may be incorrect already.

    size_t max = ShenandoahEvacAssist;
    if (max > 0) {
      ShenandoahMarkingContext* ctx = _heap->complete_marking_context();

      ShenandoahHeapRegion* r = _heap->heap_region_containing(obj);
      assert(r->is_cset(), "sanity");

      HeapWord* cur = (HeapWord*)obj + obj->size() + ShenandoahForwarding::word_size();

      size_t count = 0;
      while ((cur < r->top()) && ctx->is_marked(oop(cur)) && (count++ < max)) {
        oop cur_oop = oop(cur);
        if (cur_oop == resolve_forwarded_not_null(cur_oop)) {
          _heap->evacuate_object(cur_oop, thread);
        }
        cur = cur + cur_oop->size() + ShenandoahForwarding::word_size();
      }
    }

    return res_oop;
  }
  return fwd;
}

oop ShenandoahBarrierSet::load_reference_barrier_impl(oop obj) {
  assert(ShenandoahLoadRefBarrier, "should be enabled");
  if (!oopDesc::is_null(obj)) {
    bool evac_in_progress = _heap->is_gc_in_progress_mask(ShenandoahHeap::EVACUATION);
    oop fwd = resolve_forwarded_not_null(obj);
    if (evac_in_progress &&
        _heap->in_collection_set(obj) &&
        obj == fwd) {
      Thread *t = Thread::current();
      if (t->is_GC_task_thread()) {
        return _heap->evacuate_object(obj, t);
      } else {
        ShenandoahEvacOOMScope oom_evac_scope;
        return _heap->evacuate_object(obj, t);
      }
    } else {
      return fwd;
    }
  } else {
    return obj;
  }
}

void ShenandoahBarrierSet::enqueue(oop obj) {
  // Filter marked objects before hitting the SATB queues. The same predicate would
  // be used by SATBMQ::filter to eliminate already marked objects downstream, but
  // filtering here helps to avoid wasteful SATB queueing work to begin with.
  if (!_heap->requires_marking(obj)) return;

  G1SATBCardTableModRefBS::enqueue(obj);
}

oop ShenandoahBarrierSet::atomic_compare_exchange_oop(oop exchange_value,
                                                      volatile HeapWord *dest,
                                                      oop compare_value) {
  if (UseCompressedOops) {
    // encode exchange and compare value from oop to T
    narrowOop val = oopDesc::encode_heap_oop(exchange_value);
    narrowOop cmp = oopDesc::encode_heap_oop(compare_value);

    narrowOop old = (narrowOop) Atomic::cmpxchg(val, (narrowOop*)dest, cmp);
    // decode old from T to oop
    return oopDesc::decode_heap_oop(old);
  } else {
    return (oop)Atomic::cmpxchg_ptr(exchange_value, (oop*)dest, compare_value);
  }
}

oop ShenandoahBarrierSet::oop_atomic_cmpxchg_in_heap(oop new_value, volatile HeapWord* dest, oop compare_value) {
  oop expected;
  bool success;
  do {
    expected = compare_value;
    compare_value = atomic_compare_exchange_oop(new_value, dest, expected);
    success = (compare_value == expected);
  } while ((! success) && resolve_forwarded(compare_value) == resolve_forwarded(expected));
  oop result = load_reference_barrier(compare_value);
  if (ShenandoahSATBBarrier && success && result != NULL) {
    enqueue(result);
  }
  return result;
}
