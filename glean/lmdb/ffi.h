/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/lmdb/glean_lmdb.h"
#include "glean/rts/ffi.h"
#include "glean/storage/db.h"

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace lmdb {
namespace c {
using namespace facebook::glean::rts;
using namespace facebook::glean::rts::c;
using namespace facebook::glean::db;

extern "C" {
#endif

typedef struct SharedCache SharedCache;

const char*
glean_lmdb_container_open(const char* path, int mode, Container** container);

const char* glean_lmdb_container_open_database(
    Container* container,
    glean_fact_id_t start,
    uint32_t first_unit_id,
    int64_t version,
    Database** db);

#ifdef __cplusplus
}
}
}
}
}
#endif
