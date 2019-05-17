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

#ifndef SHARE_VM_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_HPP
#define SHARE_VM_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_HPP

#include "code/codeCache.hpp"
#include "gc_implementation/shenandoah/shenandoahCodeRoots.hpp"
#include "gc_implementation/shenandoah/shenandoahHeap.hpp"
#include "gc_implementation/shenandoah/shenandoahPhaseTimings.hpp"
#include "gc_implementation/shenandoah/shenandoahSynchronizerIterator.hpp"
#include "memory/allocation.hpp"
#include "memory/iterator.hpp"
#include "memory/sharedHeap.hpp"
#include "utilities/workgroup.hpp"


class ShenandoahSerialRoot {
public:
  typedef void (*OopsDo)(OopClosure*);
private:
  volatile jint                             _claimed;
  const OopsDo                              _oops_do;
  const ShenandoahPhaseTimings::GCParPhases _phase;

public:
  ShenandoahSerialRoot(OopsDo oops_do, ShenandoahPhaseTimings::GCParPhases);
  void oops_do(OopClosure* cl, uint worker_id);
};

class ShenandoahSerialRoots {
private:
  ShenandoahSerialRoot            _universe_roots;
  ShenandoahSerialRoot            _management_roots;
  ShenandoahSerialRoot            _jvmti_roots;
  ShenandoahSerialRoot            _jni_handle_roots;
  ShenandoahSerialRoot            _flat_profiler_roots;
  ShenandoahSynchronizerIterator  _om_iterator;
public:
  ShenandoahSerialRoots();
  void oops_do(OopClosure* cl, uint worker_id);
};

class ShenandoahSystemDictionaryRoots {
private:
  volatile int _claimed;
public:
  ShenandoahSystemDictionaryRoots();
  void strong_oops_do(OopClosure* oops, uint worker_id);
  void oops_do(OopClosure* oops, uint worker_id);
};

class ShenandoahStringTableRoots {
public:
  void oops_do(OopClosure* oops, uint worker_id);
};

class ShenandoahThreadRoots {
public:
  ShenandoahThreadRoots();
  void oops_do(OopClosure* oops_cl, CLDClosure* cld_cl, CodeBlobClosure* code_cl, uint worker_id);
};

class ShenandoahWeakRoot {
public:
  typedef void (*WeakOopsDo)(BoolObjectClosure*, OopClosure*);
private:
  volatile int      _claimed;
  const WeakOopsDo  _weak_oops_do;

public:
  ShenandoahWeakRoot(WeakOopsDo oops_do);
  void oops_do(OopClosure* keep_alive, uint worker_id);
  void weak_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive, uint worker_id);
};

class ShenandoahWeakRoots {
private:
  ShenandoahWeakRoot  _jni_weak_roots;
public:
  ShenandoahWeakRoots();
  void oops_do(OopClosure* keep_alive, uint worker_id);
  void weak_oops_do(BoolObjectClosure* is_alive, OopClosure* keep_alive, uint worker_id);
};

class ShenandoahStringDedupRoots {
public:
  ShenandoahStringDedupRoots();
  void oops_do(OopClosure* oops, uint worker_id);
};

template <typename ITR>
class ShenandoahCodeCacheRoots {
private:
  ITR _coderoots_iterator;
public:
  void code_blobs_do(CodeBlobClosure* blob_cl, uint worker_id);
};

class ShenandoahClassLoaderDataRoots {
public:
  ShenandoahClassLoaderDataRoots();

  void clds_do(CLDClosure* strong_clds, CLDClosure* weak_clds, uint worker_id);
};

class ShenandoahRootProcessor : public StackObj {
private:
  SharedHeap::StrongRootsScope _srs;
  ShenandoahHeap* const               _heap;
  const ShenandoahPhaseTimings::Phase _phase;
public:
  ShenandoahRootProcessor(ShenandoahPhaseTimings::Phase phase);
  ~ShenandoahRootProcessor();

  ShenandoahHeap* heap() const { return _heap; }
};

class ShenandoahRootScanner : public ShenandoahRootProcessor {
private:
  ShenandoahSerialRoots           _serial_roots;
  ShenandoahSystemDictionaryRoots _dict_roots;
  ShenandoahClassLoaderDataRoots  _cld_roots;
  ShenandoahThreadRoots           _thread_roots;
  ShenandoahWeakRoots             _weak_roots;
  ShenandoahStringDedupRoots      _dedup_roots;
  ShenandoahStringTableRoots      _string_table_roots;
  ShenandoahCodeCacheRoots<ShenandoahAllCodeRootsIterator>
                                  _code_roots;
public:
  ShenandoahRootScanner(ShenandoahPhaseTimings::Phase phase);

  // Apply oops, clds and blobs to all strongly reachable roots in the system,
  // during class unloading cycle
  void strong_roots_do(uint worker_id, OopClosure* cl);
  void strong_roots_do(uint worker_id, OopClosure* oops, CLDClosure* clds, CodeBlobClosure* code);

  // Apply oops, clds and blobs to all strongly reachable roots and weakly reachable
  // roots when class unloading is disabled during this cycle
  void roots_do(uint worker_id, OopClosure* cl);
  void roots_do(uint worker_id, OopClosure* oops, CLDClosure* clds, CodeBlobClosure* code);
};

// Evacuate all roots at a safepoint
class ShenandoahRootEvacuator : public ShenandoahRootProcessor {
private:
  ShenandoahSerialRoots           _serial_roots;
  ShenandoahSystemDictionaryRoots _dict_roots;
  ShenandoahClassLoaderDataRoots  _cld_roots;
  ShenandoahThreadRoots           _thread_roots;
  ShenandoahWeakRoots             _weak_roots;
  ShenandoahStringDedupRoots      _dedup_roots;
  ShenandoahStringTableRoots      _string_table_roots;
  ShenandoahCodeCacheRoots<ShenandoahCsetCodeRootsIterator>
                                  _code_roots;

public:
  ShenandoahRootEvacuator(ShenandoahPhaseTimings::Phase phase);

  void roots_do(uint worker_id, OopClosure* oops);
};

// Update all roots at a safepoint
class ShenandoahRootUpdater : public ShenandoahRootProcessor {
private:
  ShenandoahSerialRoots           _serial_roots;
  ShenandoahSystemDictionaryRoots _dict_roots;
  ShenandoahClassLoaderDataRoots  _cld_roots;
  ShenandoahThreadRoots           _thread_roots;
  ShenandoahWeakRoots             _weak_roots;
  ShenandoahStringDedupRoots      _dedup_roots;
  ShenandoahStringTableRoots      _string_table_roots;
  ShenandoahCodeCacheRoots<ShenandoahCsetCodeRootsIterator>
                                  _code_roots;
  const bool                      _update_code_cache;

public:
  ShenandoahRootUpdater(ShenandoahPhaseTimings::Phase phase, bool update_code_cache);
  void roots_do(uint worker_id, BoolObjectClosure* is_alive, OopClosure* keep_alive);
};

// Adjuster all roots at a safepoint during full gc
class ShenandoahRootAdjuster : public ShenandoahRootProcessor {
private:
  ShenandoahSerialRoots           _serial_roots;
  ShenandoahSystemDictionaryRoots _dict_roots;
  ShenandoahClassLoaderDataRoots  _cld_roots;
  ShenandoahThreadRoots           _thread_roots;
  ShenandoahWeakRoots             _weak_roots;
  ShenandoahStringDedupRoots      _dedup_roots;
  ShenandoahStringTableRoots      _string_table_roots;
  ShenandoahCodeCacheRoots<ShenandoahAllCodeRootsIterator>
                                  _code_roots;

public:
  ShenandoahRootAdjuster(ShenandoahPhaseTimings::Phase phase);

  void roots_do(uint worker_id, OopClosure* oops);
};

#endif // SHARE_VM_GC_SHENANDOAH_SHENANDOAHROOTPROCESSOR_HPP
