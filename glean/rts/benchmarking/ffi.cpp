/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifdef OSS
#include <cpp/memory.h> // @manual
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/memory.h>
#include <common/hs/util/cpp/wrap.h>
#endif

#include "glean/rts/benchmarking/factblock.h"
#include "glean/rts/benchmarking/ffi.h"
#include "glean/rts/factset.h"

using namespace facebook::hs;

namespace facebook::glean::rts::benchmarking::c {

const char* glean_benchmarking_factblock_create(
    Lookup* lookup,
    FactBlock** block) {
  return ffi::wrap([=] {
    *block = new FactBlock(FactBlock::create(*(lookup->enumerate())));
  });
}

void glean_benchmarking_factblock_free(FactBlock* block) {
  ffi::free_(block);
}

size_t glean_benchmarking_factblock_fact_count(FactBlock* block) {
  return block->size();
}

size_t glean_benchmarking_factblock_fact_memory(FactBlock* block) {
  return block->dataSize();
}

const char*
glean_benchmarking_define_each(Define* define, FactBlock* block, bool* result) {
  return ffi::wrap([=] { *result = block->defineEach(*define); });
}

const char* glean_benchmarking_lookup_each_type(
    Lookup* lookup,
    FactBlock* block,
    bool* result) {
  return ffi::wrap([=] { *result = block->lookupEachType(*lookup); });
}

const char* glean_benchmarking_lookup_each_by_id(
    Lookup* lookup,
    FactBlock* block,
    bool* result) {
  return ffi::wrap([=] { *result = block->lookupEachById(*lookup, true); });
}

const char* glean_benchmarking_lookup_each_by_key(
    Lookup* lookup,
    FactBlock* block,
    bool* result) {
  return ffi::wrap([=] { *result = block->lookupEachByKey(*lookup); });
}

const char* glean_benchmarking_seek_to_each(
    Lookup* lookup,
    FactBlock* block,
    bool* result) {
  return ffi::wrap([=] { *result = block->seekToEach(*lookup); });
}

const char* glean_benchmarking_seek_count(
    Lookup* lookup,
    const int64_t* pids,
    size_t pids_count,
    size_t* count) {
  return ffi::wrap([=] {
    size_t n = 0;
    for (size_t i = 0; i < pids_count; ++i) {
      auto iter = lookup->seek(Pid::fromThrift(pids[i]), {}, 0);
      while (auto ref = iter->get(FactIterator::Demand::KeyOnly)) {
        ++n;
        iter->next();
      }
    }
    *count = n;
  });
}

} // namespace facebook::glean::rts::benchmarking::c
