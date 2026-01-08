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
#include "glean/rocksdb/ffi.h"

using namespace facebook::hs;

namespace facebook {
namespace glean {
namespace rocks {
namespace c {
extern "C" {

struct SharedCache {
  std::shared_ptr<facebook::glean::rocks::Cache> value;
};

const char* glean_rocksdb_new_cache(size_t capacity, SharedCache** cache) {
  return ffi::wrap(
      [=] { *cache = new SharedCache{rocks::newCache(capacity)}; });
}

void glean_rocksdb_free_cache(SharedCache* cache) {
  ffi::free_(cache);
}

size_t glean_rocksdb_cache_capacity(SharedCache* cache) {
  if (cache) {
    return rocks::getCacheCapacity(cache->value);
  }
  return 0;
}

const char* glean_rocksdb_container_open(
    const char* path,
    int mode,
    bool cache_index_and_filter_blocks,
    SharedCache* cache,
    Container** container) {
  return ffi::wrap([=] {
    folly::Optional<std::shared_ptr<rocks::Cache>> cache_ptr;
    if (cache) {
      cache_ptr = cache->value;
    }
    *container = rocks::open(
                     path,
                     static_cast<rocks::Mode>(mode),
                     cache_index_and_filter_blocks,
                     std::move(cache_ptr))
                     .release();
  });
}

const char* glean_rocksdb_container_open_database(
    Container* container,
    glean_fact_id_t start,
    uint32_t first_unit_id,
    int64_t version,
    Database** database) {
  return ffi::wrap([=] {
    *database = std::move(*container)
                    .openDatabase(Id::fromThrift(start), first_unit_id, version)
                    .release();
  });
}

const char* glean_rocksdb_restore(const char* target, const char* source) {
  return ffi::wrap([=] { facebook::glean::rocks::restore(target, source); });
}

}
} // namespace c
} // namespace rocks
} // namespace glean
} // namespace facebook
