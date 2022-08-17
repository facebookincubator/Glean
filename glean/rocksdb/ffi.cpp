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
#include "glean/rocksdb/rocksdb.h"
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

const char *glean_rocksdb_new_cache(
    size_t capacity,
    SharedCache **cache) {
  return ffi::wrap([=]{
    *cache = new SharedCache{rocks::newCache(capacity)};
  });
}

void glean_rocksdb_free_cache(SharedCache *cache) {
  ffi::free_(cache);
}


void glean_rocksdb_container_close(Container *container) {
  container->close();
}

const char *glean_rocksdb_container_write_data(
    Container *container,
    const void *key,
    size_t key_size,
    const void *value,
    size_t value_size) {
  return ffi::wrap([=] {
    container->writeData(
      {static_cast<const unsigned char *>(key), key_size},
      {static_cast<const unsigned char *>(value), value_size});
  });
}

const char *glean_rocksdb_container_read_data(
    Container *container,
    const void *key,
    size_t key_size,
    void **value,
    size_t *value_size,
    unsigned char *found) {
  return ffi::wrap([=] {
    *found = container->readData(
      {static_cast<const unsigned char *>(key), key_size},
      [=](folly::ByteRange val) {
        auto bytes = ffi::clone_bytes(val);
        *value_size = bytes.size();
        *value = bytes.release();
      }) ? 1 : 0;
  });
}

const char *glean_rocksdb_container_optimize(
    Container *container) {
  return ffi::wrap([=] {
    container->optimize();
  });
}

const char *glean_rocksdb_container_backup(
    Container *container,
    const char *path) {
  return ffi::wrap([=] {
    container->backup(path);
  });
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
void glean_rocksdb_container_free(Container* container) {
  ffi::free_(container);
}

const char *glean_rocksdb_container_open_database(
    Container *container,
    glean_fact_id_t start,
    int64_t version,
    Database **database) {
  return ffi::wrap([=] {
    *database = std::move(*container)
      .openDatabase(Id::fromThrift(start), version)
      .release();
  });
}
void glean_rocksdb_database_free(Database *db) {
  ffi::free_(db);
}

Container *glean_rocksdb_database_container(
    Database *db) {
  return &(db->container());
}

Lookup *glean_rocksdb_database_lookup(Database *db) {
  return db;
}

const char *glean_rocksdb_commit(
    Database *db,
    FactSet *facts) {
  return ffi::wrap([=] {
    db->commit(*facts);
  });
}

const char *glean_rocksdb_add_ownership(
    Database *db,
    size_t count,
    const void **units,
    const size_t *unit_sizes,
    const int64_t **ids,
    const size_t *id_sizes) {
  return ffi::wrap([=] {
    std::vector<Database::OwnershipSet> v;
    v.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      v.push_back({
        {static_cast<const unsigned char *>(units[i]), unit_sizes[i]},
        {ids[i], id_sizes[i]}});
    }
    db->addOwnership(v);
  });
}

const char *glean_rocksdb_get_ownership_unit_iterator(
    Database *db,
    OwnershipUnitIterator **iter) {
  return ffi::wrap([=] {
    *iter = db->getOwnershipUnitIterator().release();
  });
}

const char *glean_rocksdb_get_unit_id(Database *db, void *unit,
                                      size_t unit_size, uint64_t *unit_id) {
  return ffi::wrap([=] {
    auto res = db->getUnitId(folly::ByteRange(
        reinterpret_cast<const unsigned char *>(unit), unit_size));
    if (res.hasValue()) {
      *unit_id = *res;
    } else {
      *unit_id = UINT64_MAX;
    }
  });
}

const char *glean_rocksdb_get_unit(Database *db, uint32_t unit_id, void **unit,
                                   size_t *unit_size) {
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


const char *glean_rocksdb_database_stats(
    Database *db,
    size_t *count,
    int64_t **ids,
    uint64_t **counts,
    uint64_t **sizes) {
  return ffi::wrap([=] {
    const auto stats = db->stats();
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
  });
}

const char *glean_rocksdb_restore(const char *target, const char *source) {
  return ffi::wrap([=] {
    facebook::glean::rocks::restore(target, source);
  });
}

const char *glean_rocksdb_store_ownership(
  Database *db,
  ComputedOwnership *ownership) {
  return ffi::wrap([=] {
    db->storeOwnership(*ownership);
  });
}

const char *glean_rocksdb_get_ownership(
  Database *db,
  Ownership **ownership) {
  return ffi::wrap([=] {
    *ownership = db->getOwnership().release();
  });
}

const char *glean_rocksdb_add_define_ownership(
  Database *db,
  DefineOwnership *define) {
  return ffi::wrap([=] {
    db->addDefineOwnership(*define);
  });
}

const char *glean_rocksdb_get_derived_fact_ownership_iterator(
  Database *db,
  uint64_t pid,
  DerivedFactOwnershipIterator **iter) {
  return ffi::wrap([=] {
    *iter = db->getDerivedFactOwnershipIterator(Pid::fromWord(pid)).release();
  });
}

}
}
}
}
}
