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

#include "precompiled.hpp"
#include "gc_implementation/shenandoah/shenandoahBarrierSet.hpp"
#include "gc_implementation/shenandoah/shenandoahRuntime.hpp"
#include "runtime/interfaceSupport.hpp"
#include "oops/oop.inline.hpp"

void ShenandoahRuntime::write_ref_array_pre_oop_entry(oop* dst, size_t length) {
  ShenandoahBarrierSet *bs = ShenandoahBarrierSet::barrier_set();
  assert(length <= INT_MAX, err_msg("sanity: " SIZE_FORMAT, length));
  bs->write_ref_array_pre(dst, (int)length, false);
}

void ShenandoahRuntime::write_ref_array_pre_narrow_oop_entry(narrowOop* dst, size_t length) {
  ShenandoahBarrierSet *bs = ShenandoahBarrierSet::barrier_set();
  assert(length <= INT_MAX, err_msg("sanity: " SIZE_FORMAT, length));
  bs->write_ref_array_pre(dst, (int)length, false);
}

void ShenandoahRuntime::write_ref_array_post_entry(HeapWord* dst, size_t length) {
  ShenandoahBarrierSet *bs = ShenandoahBarrierSet::barrier_set();
  bs->ShenandoahBarrierSet::write_ref_array(dst, length);
}

// Shenandoah pre write barrier slowpath
JRT_LEAF(void, ShenandoahRuntime::write_ref_field_pre_entry(oopDesc* orig, JavaThread *thread))
  if (orig == NULL) {
    assert(false, "should be optimized out");
    return;
  }
  shenandoah_assert_correct(NULL, orig);
  // store the original value that was in the field reference
  assert(thread->satb_mark_queue().is_active(), "Shouldn't be here otherwise");
  thread->satb_mark_queue().enqueue_known_active(orig);
JRT_END

JRT_LEAF(oopDesc*, ShenandoahRuntime::load_reference_barrier_JRT(oopDesc* src))
  oop result = ShenandoahBarrierSet::barrier_set()->load_reference_barrier_mutator(oop(src));
  return (oopDesc*) result;
JRT_END

IRT_LEAF(oopDesc*, ShenandoahRuntime::load_reference_barrier_interpreter(oopDesc* src))
  oop result = ShenandoahBarrierSet::barrier_set()->load_reference_barrier(oop(src));
  return (oopDesc*) result;
IRT_END

// Shenandoah clone barrier: makes sure that references point to to-space
// in cloned objects.
JRT_LEAF(void, ShenandoahRuntime::shenandoah_clone_barrier(oopDesc* obj))
  ShenandoahBarrierSet::barrier_set()->write_region(MemRegion((HeapWord*) obj, obj->size()));
JRT_END
