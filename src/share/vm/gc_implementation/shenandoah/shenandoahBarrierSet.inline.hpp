/*
 * Copyright (c) 2015, 2018, Red Hat, Inc. All rights reserved.
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

#ifndef SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_INLINE_HPP
#define SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_INLINE_HPP

#include "gc_implementation/shenandoah/shenandoahBarrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahCollectionSet.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahForwarding.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahMarkingContext.inline.hpp"
#include "memory/iterator.inline.hpp"
#include "oops/oop.inline.hpp"

inline oop ShenandoahBarrierSet::resolve_forwarded_not_null(oop p) {
  return ShenandoahForwarding::get_forwardee(p);
}

inline oop ShenandoahBarrierSet::resolve_forwarded(oop p) {
  if (((HeapWord*) p) != NULL) {
    return resolve_forwarded_not_null(p);
  } else {
    return p;
  }
}

template <class T, bool HAS_FWD, bool EVAC, bool ENQUEUE>
void ShenandoahBarrierSet::arraycopy_work(T* src, size_t count) {
  JavaThread* thread = JavaThread::current();
  ObjPtrQueue& queue = thread->satb_mark_queue();
  ShenandoahMarkingContext* ctx = _heap->marking_context();
  const ShenandoahCollectionSet* const cset = _heap->collection_set();
  T* end = src + count;
  for (T* elem_ptr = src; elem_ptr < end; elem_ptr++) {
    T o = oopDesc::load_heap_oop(elem_ptr);
    if (!oopDesc::is_null(o)) {
      oop obj = oopDesc::decode_heap_oop_not_null(o);
      if (HAS_FWD && cset->is_in((HeapWord *) obj)) {
        assert(_heap->has_forwarded_objects(), "only get here with forwarded objects");
        oop fwd = resolve_forwarded_not_null(obj);
        if (EVAC && obj == fwd) {
          fwd = _heap->evacuate_object(obj, thread);
        }
        assert(obj != fwd || _heap->cancelled_gc(), "must be forwarded");
        oop witness = ShenandoahHeap::cas_oop(fwd, elem_ptr, o);
        obj = fwd;
      }
      if (ENQUEUE && !ctx->is_marked(obj)) {
        queue.enqueue_known_active(obj);
      }
    }
  }
}

template <class T>
void ShenandoahBarrierSet::arraycopy_pre_work(T* src, T* dst, size_t count) {
  if (_heap->is_concurrent_mark_in_progress()) {
    if (_heap->has_forwarded_objects()) {
      arraycopy_work<T, true, false, true>(dst, count);
    } else {
      arraycopy_work<T, false, false, true>(dst, count);
    }
  }
 
  arraycopy_update_impl(src, count);
}

void ShenandoahBarrierSet::arraycopy_pre(oop* src, oop* dst, size_t count) {
  arraycopy_pre_work(src, dst, count);
}

void ShenandoahBarrierSet::arraycopy_pre(narrowOop* src, narrowOop* dst, size_t count) {
  arraycopy_pre_work(src, dst, count);
}

template <class T>
void ShenandoahBarrierSet::arraycopy_update_impl(T* src, size_t count) {
  if (_heap->is_evacuation_in_progress()) {
    ShenandoahEvacOOMScope oom_evac;
    arraycopy_work<T, true, true, false>(src, count);
  } else if (_heap->is_concurrent_traversal_in_progress()){
    ShenandoahEvacOOMScope oom_evac;
    arraycopy_work<T, true, true, true>(src, count);
  } else if (_heap->has_forwarded_objects()) {
    arraycopy_work<T, true, false, false>(src, count);
  }
}

void ShenandoahBarrierSet::arraycopy_update(oop* src, size_t count) {
  arraycopy_update_impl(src, count);
}

void ShenandoahBarrierSet::arraycopy_update(narrowOop* src, size_t count) {
  arraycopy_update_impl(src, count);
}

#endif //SHARE_VM_GC_SHENANDOAH_SHENANDOAHBARRIERSET_INLINE_HPP
