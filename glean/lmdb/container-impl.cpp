/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lmdb/container-impl.h"
#include "glean/lmdb/database-impl.h"
#include "glean/lmdb/util.h"

#include <fmt/core.h>
#include <folly/ScopeGuard.h>

namespace facebook {
namespace glean {
namespace lmdb {
namespace impl {

std::vector<const Family*> Family::families;

const Family Family::admin("admin", 0);
const Family Family::entities("entities", 0);
const Family Family::keys("keys", MDB_DUPSORT);
const Family Family::stats("stats", 0);
const Family Family::meta("meta", 0);

// Maps a unit's name to its UnitId
// ownershipUnits: String -> UnitId
const Family Family::ownershipUnits("ownershipUnits", 0);

// Translates UnitId to its name. Dual of ownershipUnits.
// ownershipUnitIds: UnitId -> String
const Family Family::ownershipUnitIds("ownershipUnitIds", 0);

// Append-only log of non-derived facts' ownership information.
// Describes a map from UnitId to list of facts.
// The key contains a tuple of UnitId and a monotonically incrementing counter.
// The value is an interval map of fact ids.
// ownershipRaw: (UnitId, nat) -> [Fid]
const Family Family::ownershipRaw("ownershipRaw", 0, false);

// Append-only log of derived facts' ownership information.
// Describes a (Map Pid (Map Fid UsetId))
// The key contains a tuple of Pid and a monotonically incrementing counter.
// ownershipDerivedRaw: (Pid, nat) -> ( [Fid], [UsetId] )
const Family Family::ownershipDerivedRaw("ownershipDerivedRaw", 0, false);

// ownershipSets: UsetId -> (Operation, [UsetId])
const Family Family::ownershipSets("ownershipSets", 0);

// An interval map, mapping fact ids to UsetId.
// factOwners: Fid -> UsetId
const Family Family::factOwners("factOwners", 0, false);

// Used to efficiently map fact ids to UsetIds
// Contains:
// - A page index at key INDEX_KEY with type [UsetId]
// - A map from page prefix to a map from page suffix to UsetId
//    prefix -> ([suffix], [UsetId])
const Family Family::factOwnerPages("factOwnerPages", 0);

// Stores locations of fact batches
// which should be written to the DB before glean complete.
// The key is a batch location
// The value is a tuple of (batch format, is the batch written)
const Family Family::batchDescriptors("batchDescriptors", 0, false);

ContainerImpl::ContainerImpl(const std::string& path, Mode m)
    : db(nullptr, &mdb_env_close) {
  mode = m;

  unsigned int flags = (m == Mode::ReadOnly ? MDB_RDONLY : 0) | MDB_NOTLS;

  MDB_env* db_ptr;
  check(mdb_env_create(&db_ptr));
  check(mdb_env_set_maxdbs(db_ptr, Family::count()));

  // LMDB needs us to pre-set the memory map size, which determines
  // the maximum size of the DB. Setting it to zero uses the mapsize
  // when the DB was created, while setting it to a value smaller than
  // the actual size of the DB causes LMDB to use the DB size - this
  // is what we want for a read-only DB. For a read/write DB we have
  // to pick a huge value, because the mapsize can only be increased
  // if there are no transactions in progress, and we don't have a good
  // way to do that aside from closing and re-opening the DB.
  check(mdb_env_set_mapsize(
      db_ptr, m == Mode::ReadOnly ? 1 : 1024 * 1024 * 1024 * 1024UL));

  check(mdb_env_open(db_ptr, path.c_str(), flags, 0666));
  SCOPE_FAIL {
    mdb_env_close(db_ptr);
  };

  key_size = mdb_env_get_maxkeysize(db_ptr);
  LOG(INFO) << folly::sformat("max key size: {}", key_size);

  // open each subdb. Names and flags come from Family::families

  {
    Txn txn(db_ptr, mode == Mode::ReadOnly ? MDB_RDONLY : 0);
    families.resize(Family::count());
    for (size_t i = 0; i < Family::count(); i++) {
      auto family = Family::family(i);
      unsigned int familyFlags = family->flags;
      if (mode != Mode::ReadOnly) {
        familyFlags |= MDB_CREATE; // create it if it doesn't exist
      }
      check(mdb_dbi_open(
          txn.ptr(), family->name, familyFlags, &families[family->index]));
    }
    txn.commit();
  }

  db.reset(db_ptr);
}

void ContainerImpl::close() noexcept {
  if (db) {
    mdb_env_close(db.release());
  }
}

void ContainerImpl::requireOpen() const {
  if (!db) {
    rts::error("lmdb: database is closed");
  }
}

MDB_dbi ContainerImpl::family(const Family& family) const {
  assert(family.index < families.size());
  return families[family.index];
}

void ContainerImpl::writeData(folly::ByteRange key, folly::ByteRange value) {
  requireOpen();
  auto writer = write();
  writer.put(Family::meta, key, value);
  writer.commit();
}

void ContainerImpl::optimize(bool compact) {
  Txn txn = txn_write();
  for (uint32_t i = 0; i < families.size(); i++) {
    if (!Family::family(i)->keep) {
      check(mdb_drop(txn.ptr(), families[i], 0 /* empty */));
    }
  }
}

std::unique_ptr<Database> ContainerImpl::openDatabase(
    Id start,
    rts::UsetId first_unit_id,
    int32_t version) && {
  return std::make_unique<DatabaseImpl>(
      std::move(*this), start, first_unit_id, version);
}

void ContainerImpl::backup(const std::string& path) {
  requireOpen();

  check(mdb_env_copy2(db.get(), path.c_str(), MDB_CP_COMPACT));
}

} // namespace impl
} // namespace lmdb
} // namespace glean
} // namespace facebook
