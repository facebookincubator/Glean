/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <stdexcept>

#include "glean/rts/densemap.h"
#include "glean/rts/factset.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"
#include "glean/rts/ownership/derived.h"
#include "glean/rts/stats.h"
#include "glean/rts/store.h"

namespace facebook {
namespace glean {
namespace db {

using rts::Id;
using rts::Pid;

struct Database;

/// A rocksdb container for storing facts
struct Container {
  virtual ~Container() {}

  /// Close the 'Container' - accessing it afterwards isn't allowed.
  virtual void close() noexcept = 0;

  /// Write a key/value pair to the Container metadata.
  virtual void writeData(folly::ByteRange key, folly::ByteRange value) = 0;

  /// Lookup a key in the Container metadata.
  virtual bool readData(
      folly::ByteRange key,
      std::function<void(folly::ByteRange)> f) = 0;

  /// Optimise the container for reading
  virtual void optimize(bool compact) = 0;

  /// Backup the Container to the specified directory.
  virtual void backup(const std::string& path) = 0;

  /// Create an openable snapshot (a RocksDB Checkpoint) of the Container at
  /// the specified (absolute) directory, which must not already exist. Unlike
  /// 'backup' (a BackupEngine backup), the resulting directory is itself a
  /// valid DB, so a restore can rename it into place without a second copy.
  /// Only supported by storage backends that implement it; the default throws.
  // The default unconditionally throws, but this is a virtual whose overrides
  // (e.g. rocksdb) return normally, so it must not be marked [[noreturn]].
  // NOLINTNEXTLINE(clang-diagnostic-missing-noreturn)
  virtual void checkpoint(const std::string& /*path*/) {
    throw std::runtime_error("checkpoint not supported for this storage");
  }

  /// Convert the Container to a full fact Database with the given
  /// representation version - accessing the original Container afterwards isn't
  /// allowed. If the database is being created, start is the starting fact id.
  ///
  /// first_unit_id is the starting point of this DB's allocation range in
  /// the shared unit/set ID namespace. For a new DB this is typically 0;
  /// for a stacked DB it is the base DB's next_uset_id.
  ///
  /// The base Ownership passed in (if any) must not be destructed before the
  /// Database.
  virtual std::unique_ptr<Database>
  openDatabase(Id start, rts::UsetId first_unit_id, int32_t version) && = 0;
};

enum class Mode { ReadOnly = 0, ReadWrite = 1, Create = 2 };

/// A fact database
struct Database : rts::Lookup {
  virtual Container& container() noexcept = 0;

  virtual rts::PredicateStats predicateStats() const = 0;

  /// A batch of fact-ID ranges owned by a single ownership unit.
  struct OwnershipSet {
    folly::ByteRange unit;
    /// Packed inclusive intervals [lo1,hi1, lo2,hi2, …] of fact IDs
    /// that belong to this unit.
    folly::Range<const int64_t*> ids;
  };

  struct BatchDescriptor {
    std::string location;
    uint32_t format{};
  };

  virtual void commit(rts::FactSet& facts) = 0;

  virtual void addBatchDescriptor(BatchDescriptor batchDescriptor) = 0;

  virtual void markBatchDescriptorAsWritten(folly::ByteRange location) = 0;

  virtual bool isBatchDescriptorStored(folly::ByteRange location) = 0;

  virtual std::vector<BatchDescriptor> getUnprocessedBatchDescriptors() = 0;

  /// Register ownership units and their fact-ID ranges. Assigns new
  /// UnitIds (from the shared namespace) for previously unseen unit names.
  virtual void addOwnership(const std::vector<OwnershipSet>& ownership) = 0;

  /// Register ownership units that own no facts, allocating a UnitId for
  /// each previously-unseen name. Returns the smallest UnitId among the
  /// given names -- the boundary between earlier units and these. Used to
  /// reserve UnitIds for ACL group units ("acl:<name>") at completion time.
  virtual uint32_t registerUnits(
      const std::vector<folly::ByteRange>& names) = 0;

  /// Iterate over raw (unit-ID, fact-ID-ranges) pairs stored by addOwnership.
  virtual std::unique_ptr<rts::OwnershipUnitIterator>
  getOwnershipUnitIterator() = 0;

  /// Persist derived-fact ownership produced by a derivation pass.
  /// Rebases set IDs from the DefineOwnership's local namespace into the
  /// DB's global namespace.
  virtual void addDefineOwnership(rts::DefineOwnership& def) = 0;

  /// Iterate over raw derived-fact ownership entries for a given predicate.
  virtual std::unique_ptr<rts::DerivedFactOwnershipIterator>
  getDerivedFactOwnershipIterator(Pid pid) = 0;

  virtual folly::Optional<uint32_t> getUnitId(folly::ByteRange) = 0;
  virtual folly::Optional<std::string> getUnit(uint32_t) = 0;
  virtual std::vector<std::pair<std::string, uint32_t>> getUnitsByPrefix(
      folly::ByteRange prefix) = 0;

  /// Persist the results of computeOwnership(): serializes promoted
  /// sets to Elias-Fano encoding and writes the fact→owner interval map.
  virtual void storeOwnership(rts::ComputedOwnership& ownership) = 0;

  /// Return an Ownership handle for querying set and fact ownership at
  /// runtime (set lookup, iteration, fact→owner resolution).
  virtual std::unique_ptr<rts::Ownership> getOwnership() = 0;

  /// Enable the read-only fact-owner page cache. Call only after the DB
  /// is finalized and prepareFactOwnerCache() has run.
  virtual void cacheOwnership() = 0;

  /// Translate the factOwners interval map into the compact
  /// factOwnerPages representation. Call once when the DB is complete,
  /// before cacheOwnership().
  virtual void prepareFactOwnerCache() = 0;
};

void restore(const std::string& target, const std::string& source);

} // namespace db
} // namespace glean
} // namespace facebook
