/*
 * Copyright (c) 2018, Red Hat, Inc. All rights reserved.
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

#ifndef SHARE_VM_GC_SHENANDOAH_SHENANDOAHTRAVERSALGC_INLINE_HPP
#define SHARE_VM_GC_SHENANDOAH_SHENANDOAHTRAVERSALGC_INLINE_HPP

#include "gc_implementation/shenandoah/shenandoahAsserts.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSet.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahMarkingContext.inline.hpp"
#include "gc_implementation/shenandoah/shenandoahStringDedup.hpp"
#include "gc_implementation/shenandoah/shenandoahTraversalGC.hpp"
#include "gc_implementation/shenandoah/shenandoahTaskqueue.hpp"
#include "gc_implementation/shenandoah/shenandoahTaskqueue.inline.hpp"
#include "oops/oop.inline.hpp"

template <class T, bool STRING_DEDUP, bool DEGEN, bool ATOMIC_UPDATE>
void ShenandoahTraversalGC::process_oop(T* p, Thread* thread, ShenandoahObjToScanQueue* queue, ShenandoahMarkingContext* const mark_context, ShenandoahStrDedupQueue* dq) {
  T o = oopDesc::load_heap_oop(p);
  if (!oopDesc::is_null(o)) {
    oop obj = oopDesc::decode_heap_oop_not_null(o);
    if (DEGEN) {
      assert(!ATOMIC_UPDATE, "Degen path assumes non-atomic updates");
      oop forw = ShenandoahBarrierSet::resolve_forwarded_not_null(obj);
      if (obj != forw) {
        // Update reference.
        oopDesc::encode_store_heap_oop_not_null(p, forw);
      }
      obj = forw;
    } else if (_heap->in_collection_set(obj)) {
      oop forw = ShenandoahBarrierSet::resolve_forwarded_not_null(obj);
      if (obj == forw) {
        forw = _heap->evacuate_object(obj, thread);
      }
      shenandoah_assert_forwarded_except(p, obj, _heap->cancelled_gc());
      // Update reference.
      if (ATOMIC_UPDATE) {
        _heap->cas_oop(forw, p, obj);
      } else {
        oopDesc::encode_store_heap_oop_not_null(p, forw);
      }
      obj = forw;
    }

    shenandoah_assert_not_forwarded(p, obj);
    shenandoah_assert_not_in_cset_except(p, obj, _heap->cancelled_gc());

    if (mark_context->mark(obj)) {
      bool succeeded = queue->push(ShenandoahMarkTask(obj));
      assert(succeeded, "must succeed to push to task queue");

      if (STRING_DEDUP && ShenandoahStringDedup::is_candidate(obj) && !_heap->cancelled_gc()) {
        assert(ShenandoahStringDedup::is_enabled(), "Must be enabled");
        // Only dealing with to-space string, so that we can avoid evac-oom protocol, which is costly here.
        shenandoah_assert_not_in_cset(p, obj);
        assert(dq != NULL, "Dedup queue not set");
        ShenandoahStringDedup::enqueue_candidate(obj, dq);
      }
    }
  }
}

#endif // SHARE_VM_GC_SHENANDOAH_SHENANDOAHTRAVERSALGC_INLINE_HPP
