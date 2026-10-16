/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/densemap.h"
#include "glean/rts/factset.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"
#include "glean/rts/ownership/derived.h"
#include "glean/rts/stats.h"
#include "glean/rts/store.h"
#include "glean/storage/db.h"

namespace rocksdb {
class Cache;
class Iterator;
} // namespace rocksdb

namespace facebook {
namespace glean {
namespace rocks {

using namespace facebook::glean::db;

using rts::Id;
using rts::Pid;

using Cache = ::rocksdb::Cache;

std::shared_ptr<Cache> newCache(size_t capacity);
size_t getCacheCapacity(const std::shared_ptr<Cache>& cache);

/// RocksDB tuning applied when opening a container.
struct ContainerOptions {
  bool cache_index_and_filter_blocks = false;
  /// Background compaction and flush threads; 0 keeps RocksDB's default.
  /// The thread pool is process-wide, so this applies to every open DB.
  int background_jobs = 0;
};

std::unique_ptr<Container> open(
    const std::string& path,
    Mode mode,
    const ContainerOptions& container_options,
    folly::Optional<std::shared_ptr<Cache>> cache);

void restore(const std::string& target, const std::string& source);

} // namespace rocks
} // namespace glean
} // namespace facebook
