/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifdef OSS
#include <cpp/memory.h> // @manual
#else
#include <common/hs/util/cpp/memory.h>
#endif

#include "glean/rts/stats.h"

using namespace facebook::hs;

namespace facebook {
namespace glean {
namespace rts {

void marshal(
    const PredicateStats& stats,
    size_t *count,
    int64_t **ids,
    uint64_t **counts,
    uint64_t **sizes) {
  const auto n = stats.size();
  auto ids_arr = ffi::malloc_array<int64_t>(n);
  auto counts_arr = ffi::malloc_array<uint64_t>(n);
  auto sizes_arr = ffi::malloc_array<uint64_t>(n);
  size_t i = 0;
  for (const auto x : stats) {
    if (x.second.count) {
      ids_arr[i] = x.first.toThrift();
      counts_arr[i] = x.second.count;
      sizes_arr[i] = x.second.memory;
      ++i;
    }
  }
  *count = i;
  *ids = ids_arr.release();
  *counts = counts_arr.release();
  *sizes = sizes_arr.release();
}

}
}
}
