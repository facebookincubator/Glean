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

#include "glean/storage/db.h"
#include "glean/storage/ffi.h"

using namespace facebook::hs;

namespace facebook {
namespace glean {
namespace db {
namespace c {
extern "C" {

void glean_rocksdb_container_close(Container* container) {
  container->close();
}

const char* glean_rocksdb_container_write_data(
    Container* container,
    const void* key,
    size_t key_size,
    const void* value,
    size_t value_size) {
  return ffi::wrap([=] {
    container->writeData(
        {static_cast<const unsigned char*>(key), key_size},
        {static_cast<const unsigned char*>(value), value_size});
  });
}

const char* glean_rocksdb_container_read_data(
    Container* container,
    const void* key,
    size_t key_size,
    void** value,
    size_t* value_size,
    unsigned char* found) {
  return ffi::wrap([=] {
    *found = container->readData(
                 {static_cast<const unsigned char*>(key), key_size},
                 [=](folly::ByteRange val) {
                   auto bytes = ffi::clone_bytes(val);
                   *value_size = bytes.size();
                   *value = bytes.release();
                 })
        ? 1
        : 0;
  });
}

const char* glean_rocksdb_container_optimize(
    Container* container,
    bool compact) {
  return ffi::wrap([=] { container->optimize(compact); });
}

const char* glean_rocksdb_container_backup(
    Container* container,
    const char* path) {
  return ffi::wrap([=] { container->backup(path); });
}

void glean_rocksdb_container_free(Container* container) {
  ffi::free_(container);
}

void glean_rocksdb_database_free(Database* db) {
  ffi::free_(db);
}

Container* glean_rocksdb_database_container(Database* db) {
  return &(db->container());
}

Lookup* glean_rocksdb_database_lookup(Database* db) {
  return db;
}

const char* glean_rocksdb_commit(Database* db, FactSet* facts) {
  return ffi::wrap([=] { db->commit(*facts); });
}

const char* glean_rocksdb_add_batch_descriptor(
    Database* db,
    const char* location,
    size_t location_size,
    int format) {
  return ffi::wrap([=] {
    Database::BatchDescriptor desc;
    desc.location = std::string(location, location_size);
    desc.format = static_cast<uint32_t>(format);
    db->addBatchDescriptor(std::move(desc));
  });
}

const char* glean_rocksdb_mark_batch_descriptor_as_written(
    Database* db,
    const char* location,
    size_t location_size) {
  return ffi::wrap([=] {
    db->markBatchDescriptorAsWritten(
        folly::ByteRange(
            reinterpret_cast<const unsigned char*>(location), location_size));
  });
}

const char* glean_rocksdb_is_batch_descriptor_stored(
    Database* db,
    const char* location,
    size_t location_size,
    unsigned char* found) {
  return ffi::wrap([=] {
    *found = db->isBatchDescriptorStored(
                 folly::ByteRange(
                     reinterpret_cast<const unsigned char*>(location),
                     location_size))
        ? 1
        : 0;
  });
}

const char* glean_rocksdb_get_unprocessed_batch_descriptors(
    Database* db,
    size_t* count,
    char*** locations,
    size_t** location_sizes,
    uint32_t** formats) {
  return ffi::wrap([=] {
    auto writes = db->getUnprocessedBatchDescriptors();
    const auto n = writes.size();
    *count = n;
    auto locs = ffi::malloc_array<char*>(n);
    auto sizes = ffi::malloc_array<size_t>(n);
    auto fmts = ffi::malloc_array<uint32_t>(n);
    for (size_t i = 0; i < n; ++i) {
      const auto& w = writes[i];
      fmts[i] = w.format;
      sizes[i] = w.location.size();
      auto loc = ffi::clone_bytes(w.location);
      locs[i] = reinterpret_cast<char*>(loc.release());
    }
    *locations = locs.release();
    *location_sizes = sizes.release();
    *formats = fmts.release();
  });
}

const char* glean_rocksdb_add_ownership(
    Database* db,
    size_t count,
    const void** units,
    const size_t* unit_sizes,
    const int64_t** ids,
    const size_t* id_sizes) {
  return ffi::wrap([=] {
    std::vector<Database::OwnershipSet> v;
    v.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      v.push_back(
          {{static_cast<const unsigned char*>(units[i]), unit_sizes[i]},
           {ids[i], id_sizes[i]}});
    }
    db->addOwnership(v);
  });
}

const char* glean_rocksdb_get_ownership_unit_iterator(
    Database* db,
    OwnershipUnitIterator** iter) {
  return ffi::wrap([=] { *iter = db->getOwnershipUnitIterator().release(); });
}

const char* glean_rocksdb_get_unit_id(
    Database* db,
    void* unit,
    size_t unit_size,
    uint64_t* unit_id) {
  return ffi::wrap([=] {
    auto res = db->getUnitId(
        folly::ByteRange(
            reinterpret_cast<const unsigned char*>(unit), unit_size));
    if (res.hasValue()) {
      *unit_id = *res;
    } else {
      *unit_id = UINT64_MAX;
    }
  });
}

const char* glean_rocksdb_get_unit(
    Database* db,
    uint32_t unit_id,
    void** unit,
    size_t* unit_size) {
  return ffi::wrap([=] {
    auto res = db->getUnit(unit_id);
    if (res.hasValue()) {
      ffi::clone_bytes(*res).release_to(unit, unit_size);
    } else {
      *unit = nullptr;
      *unit_size = 0;
    }
  });
}

const char* glean_rocksdb_database_predicateStats(
    Database* db,
    size_t* count,
    int64_t** ids,
    uint64_t** counts,
    uint64_t** sizes) {
  return ffi::wrap(
      [=] { rts::marshal(db->predicateStats(), count, ids, counts, sizes); });
}

const char* glean_rocksdb_store_ownership(
    Database* db,
    ComputedOwnership* ownership) {
  return ffi::wrap([=] { db->storeOwnership(*ownership); });
}

const char* glean_rocksdb_get_ownership(Database* db, Ownership** ownership) {
  return ffi::wrap([=] { *ownership = db->getOwnership().release(); });
}

const char* glean_rocksdb_add_define_ownership(
    Database* db,
    DefineOwnership* define) {
  return ffi::wrap([=] { db->addDefineOwnership(*define); });
}

const char* glean_rocksdb_get_derived_fact_ownership_iterator(
    Database* db,
    uint64_t pid,
    DerivedFactOwnershipIterator** iter) {
  return ffi::wrap([=] {
    *iter = db->getDerivedFactOwnershipIterator(Pid::fromWord(pid)).release();
  });
}

const char* glean_rocksdb_cache_ownership(Database* db) {
  return ffi::wrap([=] { db->cacheOwnership(); });
}

const char* glean_rocksdb_prepare_fact_owner_cache(Database* db) {
  return ffi::wrap([=] { db->prepareFactOwnerCache(); });
}
}
} // namespace c
} // namespace db
} // namespace glean
} // namespace facebook
