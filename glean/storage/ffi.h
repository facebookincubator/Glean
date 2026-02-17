/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ffi.h"

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace db {
#endif

using Container = struct Container;
using Database = struct Database;

#ifdef __cplusplus
}
}
}
#endif

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace db {
namespace c {
using namespace facebook::glean::rts;
using namespace facebook::glean::rts::c;

extern "C" {
#endif

void glean_rocksdb_container_close(Container* container);

const char* glean_rocksdb_container_write_data(
    Container* container,
    const void* key,
    size_t key_size,
    const void* value,
    size_t value_size);

const char* glean_rocksdb_container_read_data(
    Container* container,
    const void* key,
    size_t key_size,
    void** value,
    size_t* value_size,
    unsigned char* found);

const char* glean_rocksdb_container_optimize(Container* db, bool compact);

const char* glean_rocksdb_container_backup(Container* db, const char* path);

void glean_rocksdb_container_free(Container* container);

void glean_rocksdb_database_free(Database* db);

Container* glean_rocksdb_database_container(Database* db);

Lookup* glean_rocksdb_database_lookup(Database* db);

const char* glean_rocksdb_commit(Database* db, FactSet* facts);

const char* glean_rocksdb_add_batch_descriptor(
    Database* db,
    const char* location,
    size_t location_size,
    int format);

const char* glean_rocksdb_mark_batch_descriptor_as_written(
    Database* db,
    const char* location,
    size_t location_size);

const char* glean_rocksdb_is_batch_descriptor_stored(
    Database* db,
    const char* location,
    size_t location_size,
    unsigned char* found);

const char* glean_rocksdb_get_unprocessed_batch_descriptors(
    Database* db,
    size_t* count,
    char*** locations,
    size_t** location_sizes,
    uint32_t** formats);

const char* glean_rocksdb_add_ownership(
    Database* db,
    size_t count,
    const void** units,
    const size_t* unit_sizes,
    const int64_t** ids,
    const size_t* id_sizes);

const char* glean_rocksdb_get_ownership_unit_iterator(
    Database* db,
    OwnershipUnitIterator** iter);

const char* glean_rocksdb_get_unit_id(
    Database* db,
    void* unit,
    size_t unit_size,
    uint64_t* unit_id);

const char* glean_rocksdb_get_unit(
    Database* db,
    uint32_t unit_id,
    void** unit,
    size_t* unit_size);

const char* glean_rocksdb_database_predicateStats(
    Database* db,
    size_t* count,
    int64_t** ids,
    uint64_t** counts,
    uint64_t** sizes);

const char* glean_rocksdb_store_ownership(
    Database* db,
    ComputedOwnership* ownership);

const char* glean_rocksdb_get_ownership(Database* db, Ownership** ownership);

const char* glean_rocksdb_add_define_ownership(
    Database* db,
    DefineOwnership* define);

const char* glean_rocksdb_get_derived_fact_ownership_iterator(
    Database* db,
    uint64_t pid,
    DerivedFactOwnershipIterator** iter);

const char* glean_rocksdb_database_cache_ownership(Database* db);

const char* glean_rocksdb_prepare_fact_owner_cache(Database* db);

#ifdef __cplusplus
}
}
}
}
}
#endif
