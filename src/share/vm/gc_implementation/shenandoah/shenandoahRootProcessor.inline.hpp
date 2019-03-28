/*
 * Copyright (c) 2019, Red Hat, Inc. All rights reserved.
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

#ifndef SHARE_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_INLINE_HPP
#define SHARE_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_INLINE_HPP

#include "gc_implementation/shenandoah/shenandoahPhaseTimings.hpp"
#include "gc_implementation/shenandoah/shenandoahRootProcessor.hpp"

template <typename IsAlive>
void ShenandoahRootProcessor::update_all_roots(OopClosure* oops,
                                               CLDClosure* clds,
                                               CodeBlobClosure* blobs,
                                               ThreadClosure* thread_cl,
                                               uint worker_id) {
  assert(thread_cl == NULL, "not implemented yet");
  IsAlive is_alive;
  process_java_roots(oops, clds, clds, blobs, thread_cl, worker_id);
  process_vm_roots(oops, oops, &is_alive, worker_id);

  if (blobs != NULL) {
    _coderoots_all_iterator.possibly_parallel_blobs_do(blobs);
  }

  _process_strong_tasks->all_tasks_completed();
}

#endif // SHARE_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_INLINE_HPP
