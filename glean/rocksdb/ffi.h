/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ffi.h"
#include "glean/storage/db.h"
#include "glean/rocksdb/rocksdb.h"

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace rocks {
namespace c {
using namespace facebook::glean::rts;
using namespace facebook::glean::rts::c;
using namespace facebook::glean::db;

extern "C" {
#endif

typedef struct SharedCache SharedCache;

const char* glean_rocksdb_new_cache(size_t capacity, SharedCache** cache);
void glean_rocksdb_free_cache(SharedCache* cache);
size_t glean_rocksdb_cache_capacity(SharedCache* cache);

const char* glean_rocksdb_container_open(
    const char* path,
    int mode,
    bool cache_index_and_filter_blocks,
    SharedCache* cache,
    Container** container);

const char* glean_rocksdb_container_open_database(
    Container* container,
    glean_fact_id_t start,
    uint32_t first_unit_id,
    int64_t version,
    Database** db);

const char* glean_rocksdb_restore(const char* target, const char* source);

#ifdef __cplusplus
}
}
}
}
}
#endif
