// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/rts/ffi.h"

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace rocks {
#endif

typedef struct Container Container;
typedef struct Database Database;

#ifdef __cplusplus
}
}
}
#endif

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace rocks {
namespace c {
using namespace facebook::glean::rts;
using namespace facebook::glean::rts::c;

extern "C" {
#endif

typedef struct SharedCache SharedCache;

const char *glean_rocksdb_new_cache(
  size_t capacity,
  SharedCache **cache
);
void glean_rocksdb_free_cache(
  SharedCache *cache
);


void glean_rocksdb_container_close(
  Container *container
);

const char *glean_rocksdb_container_write_data(
  Container *container,
  const void *key,
  size_t key_size,
  const void *value,
  size_t value_size
);

const char *glean_rocksdb_container_read_data(
  Container *container,
  const void *key,
  size_t key_size,
  void **value,
  size_t *value_size,
  unsigned char *found
);

const char *glean_rocksdb_container_optimize(
  Container *db
);

const char *glean_rocksdb_container_backup(
  Container *db,
  const char *path
);


const char *glean_rocksdb_container_open(
  const char *path,
  int mode,
  SharedCache *cache,
  Container **container
);
void glean_rocksdb_container_free(
  Container *container
);

const char *glean_rocksdb_container_open_database(
  Container *container,
  glean_fact_id_t start,
  int64_t version,
  Database **db
);
void glean_rocksdb_database_free(
  Database *db
);

Container *glean_rocksdb_database_container(
  Database *db
);

Lookup *glean_rocksdb_database_lookup(
  Database *db
);

const char *glean_rocksdb_commit(
  Database *db,
  FactSet *facts
);

const char *glean_rocksdb_add_ownership(
  Database *db,
  size_t count,
  const void **units,
  const size_t *unit_sizes,
  const int64_t **ids,
  const size_t *id_sizes
);

const char *glean_rocksdb_get_ownership_unit_iterator(
  Database *db,
  OwnershipUnitIterator **iter
);

const char *glean_rocksdb_get_unit_id(
  Database *db,
  void *unit,
  size_t unit_size,
  uint64_t *unit_id
);

const char *glean_rocksdb_database_stats(
  Database *db,
  size_t *count,
  int64_t **ids,
  uint64_t **counts,
  uint64_t **sizes
);

const char *glean_rocksdb_restore(
  const char *target,
  const char *source
);

const char *glean_rocksdb_store_ownership(
  Database *db,
  ComputedOwnership *ownership
);

const char *glean_rocksdb_get_ownership(
  Database *db,
  Ownership **ownership
);

const char *glean_rocksdb_add_define_ownership(
  Database *db,
  DefineOwnership *define
);
#ifdef __cplusplus
}
}
}
}
}
#endif
