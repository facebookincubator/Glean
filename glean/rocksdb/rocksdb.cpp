/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rocksdb/rocksdb.h"
#include "glean/rocksdb/container-impl.h"

namespace facebook {
namespace glean {
namespace rocks {

std::shared_ptr<Cache> newCache(size_t capacity) {
  return rocksdb::NewLRUCache(capacity);
}

std::unique_ptr<Container> open(
    const std::string& path,
    Mode mode,
    bool cache_index_and_filter_blocks,
    folly::Optional<std::shared_ptr<Cache>> cache) {
  return std::make_unique<impl::ContainerImpl>(
      path, mode, cache_index_and_filter_blocks, std::move(cache));
}

} // namespace rocks
} // namespace glean
} // namespace facebook
