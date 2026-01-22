/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lmdb/glean_lmdb.h"
#include "glean/lmdb/container-impl.h"

namespace facebook {
namespace glean {
namespace lmdb {

std::unique_ptr<Container> open(
    const std::string& path,
    Mode mode) {
  return std::make_unique<impl::ContainerImpl>(path, mode);
}

} // namespace rocks
} // namespace glean
} // namespace facebook
