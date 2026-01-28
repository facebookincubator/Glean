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
#include "glean/lmdb/ffi.h"

using namespace facebook::hs;

namespace facebook {
namespace glean {
namespace lmdb {
namespace c {
extern "C" {

const char*
glean_lmdb_container_open(const char* path, int mode, Container** container) {
  return ffi::wrap([=] {
    *container = lmdb::open(path, static_cast<lmdb::Mode>(mode)).release();
  });
}

const char* glean_lmdb_container_open_database(
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
}
} // namespace c
} // namespace lmdb
} // namespace glean
} // namespace facebook
